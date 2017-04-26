/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Planning

open System
open System.Reflection
open System.Collections.Generic
open System.Collections.Concurrent
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Introspection

/// Field definition allowing to access the current type schema of this server.
let SchemaMetaFieldDef = 
    Define.Field(
        name = "__schema",
        description = "Access the current type schema of this server.",
        typedef = __Schema,
        resolve = fun ctx (_: obj) -> ctx.Schema.Introspected)
    
/// Field definition allowing to request the type information of a single type.
let TypeMetaFieldDef = 
    Define.Field(
        name = "__type",
        description = "Request the type information of a single type.",
        typedef = __Type,
        args = [
            { Name = "name"
              Description = None
              TypeDef = String
              DefaultValue = None
              ExecuteInput = variableOrElse(coerceStringInput >> Option.map box >> Option.toObj) }
        ],
        resolve = fun ctx (_:obj) -> 
            ctx.Schema.Introspected.Types 
            |> Seq.find (fun t -> t.Name = ctx.Arg("name")) 
            |> IntrospectionTypeRef.Named)
    
/// Field definition allowing to resolve a name of the current Object type at runtime.
let TypeNameMetaFieldDef : FieldDef<obj> = 
    Define.Field(
        name = "__typename",
        description = "The name of the current Object type at runtime.",
        typedef = String,
        resolve = fun ctx (_:obj) -> ctx.ParentType.Name)
        
let private tryFindDef (schema: ISchema) (objdef: ObjectDef) (field: Field) : FieldDef option =
        match field.Name with
        | "__schema" when Object.ReferenceEquals(schema.Query, objdef) -> Some (upcast SchemaMetaFieldDef)
        | "__type" when Object.ReferenceEquals(schema.Query, objdef) -> Some (upcast TypeMetaFieldDef)
        | "__typename" -> Some (upcast TypeNameMetaFieldDef)
        | fieldName -> objdef.Fields |> Map.tryFind fieldName

let private objectInfo(ctx: PlanningContext, parentDef: ObjectDef, field: Field, includer: Includer) =
    match tryFindDef ctx.Schema parentDef field with
    | Some fdef ->
        { Identifier = field.AliasOrName
          Kind = ResolveValue
          ParentDef = parentDef
          ReturnDef = fdef.TypeDef
          Definition = fdef
          Ast = field
          Include = includer 
          IsNullable = false }
    | None ->
        raise (GraphQLException (sprintf "No field '%s' was defined in object definition '%s'" field.Name parentDef.Name))

let rec private abstractionInfo (ctx:PlanningContext) (parentDef: AbstractDef) (field: Field) typeCondition includer =
    let objDefs = ctx.Schema.GetPossibleTypes parentDef
    match typeCondition with
    | None ->
        objDefs
        |> Array.choose (fun objDef ->
            match tryFindDef ctx.Schema objDef field with
            | Some fdef ->
                let data = 
                    { Identifier = field.AliasOrName
                      ParentDef = parentDef :?> OutputDef
                      ReturnDef = fdef.TypeDef
                      Definition = fdef
                      Ast = field
                      Kind = ResolveAbstraction Map.empty
                      Include = includer 
                      IsNullable = false }
                Some (objDef.Name, data)
            | None -> None)
        |> Map.ofArray
    | Some typeName ->
        match objDefs |> Array.tryFind (fun o -> o.Name = typeName) with
        | Some objDef ->
            match tryFindDef ctx.Schema objDef field with
            | Some fdef ->
                let data = 
                    { Identifier = field.AliasOrName
                      ParentDef = parentDef  :?> OutputDef
                      ReturnDef = fdef.TypeDef
                      Definition = fdef
                      Ast = field
                      Kind = ResolveAbstraction Map.empty
                      Include = includer 
                      IsNullable = false }
                Map.ofList [ objDef.Name, data ]
            | None -> Map.empty
        | None -> 
            match ctx.Schema.TryFindType typeName with
            | Some (Abstract abstractDef) -> 
                abstractionInfo ctx abstractDef field None includer
            | _ ->
                let pname = parentDef :?> NamedDef
                raise (GraphQLException (sprintf "There is no object type named '%s' that is a possible type of '%s'" typeName pname.Name))
    
let private directiveIncluder (directive: Directive) : Includer =
    fun variables ->
        match directive.If.Value with
        | Variable vname -> downcast variables.[vname]
        | other -> 
            match coerceBoolInput other with
            | Some s -> s
            | None -> raise (GraphQLException (sprintf "Expected 'if' argument of directive '@%s' to have boolean value but got %A" directive.Name other))

let private incl: Includer = fun _ -> true
let private excl: Includer = fun _ -> false
let private getIncluder (directives: Directive list) parentIncluder : Includer =
    directives
    |> List.fold (fun acc directive ->
        match directive.Name with
        | "skip" ->
            fun vars -> acc vars && not(directiveIncluder directive vars)
        | "include" -> 
            fun vars -> acc vars && (directiveIncluder directive vars)
        | _ -> acc) parentIncluder

let private doesFragmentTypeApply (schema: ISchema) fragment (objectType: ObjectDef) = 
    match fragment.TypeCondition with
    | None -> true
    | Some typeCondition ->
        match schema.TryFindType typeCondition with
        | None -> false
        | Some conditionalType when conditionalType.Name = objectType.Name -> true
        | Some (Abstract conditionalType) -> schema.IsPossibleType conditionalType objectType
        | _ -> false
                
let rec private plan (ctx: PlanningContext) (info) : ExecutionInfo =
    match info.ReturnDef with
    | Leaf _ -> info
    | Object _ -> planSelection ctx info info.Ast.SelectionSet (ref [])
    | Nullable returnDef -> 
        let inner = plan ctx { info with ParentDef = info.ReturnDef; ReturnDef = downcast returnDef }
        { inner with IsNullable = true}
    | List returnDef -> 
        let inner = plan ctx { info with ParentDef = info.ReturnDef; ReturnDef = downcast returnDef }
        { info with Kind = ResolveCollection inner }
    | Abstract _ -> planAbstraction ctx info info.Ast.SelectionSet (ref []) None

and private planSelection (ctx: PlanningContext) (info) (selectionSet: Selection list) visitedFragments : ExecutionInfo = 
    let parentDef = downcast info.ReturnDef
    let plannedFields =
        selectionSet
        |> List.fold(fun (fields: ExecutionInfo list) selection ->
            //FIXME: includer is not passed along from top level fragments (both inline and spreads)
            let includer = getIncluder selection.Directives info.Include
            let updatedInfo = { info with Include = includer }
            match selection with
            | Field field ->
                let identifier = field.AliasOrName
                if fields |> List.exists (fun f -> f.Identifier = identifier) 
                then fields
                else 
                    let innerInfo = objectInfo(ctx, parentDef, field, includer)
                    let executionPlan = plan ctx innerInfo 
                    fields @ [executionPlan]    // unfortunatelly, order matters here
            | FragmentSpread spread ->
                let spreadName = spread.Name
                if !visitedFragments |> List.exists (fun name -> name = spreadName) 
                then fields  // fragment already found
                else
                    visitedFragments := spreadName::!visitedFragments
                    match ctx.Document.Definitions |> List.tryFind (function FragmentDefinition f -> f.Name.Value = spreadName | _ -> false) with
                    | Some (FragmentDefinition fragment) when doesFragmentTypeApply ctx.Schema fragment parentDef ->
                        // retrieve fragment data just as it was normal selection set
                        let fragmentInfo = planSelection ctx updatedInfo fragment.SelectionSet visitedFragments
                        let (SelectFields(fragmentFields)) = fragmentInfo.Kind
                        // filter out already existing fields
                        List.mergeBy (fun field -> field.Identifier) fields fragmentFields
                    | _ -> fields
            | InlineFragment fragment when doesFragmentTypeApply ctx.Schema fragment parentDef ->
                 // retrieve fragment data just as it was normal selection set
                 let fragmentInfo = planSelection ctx updatedInfo fragment.SelectionSet visitedFragments
                 let (SelectFields(fragmentFields)) = fragmentInfo.Kind
                 // filter out already existing fields
                 List.mergeBy (fun field -> field.Identifier) fields fragmentFields
            | _ -> fields
        ) []
    { info with Kind = SelectFields plannedFields }
    
and private planAbstraction (ctx:PlanningContext) (info) (selectionSet: Selection list) visitedFragments typeCondition : ExecutionInfo =
    let plannedTypeFields =
        selectionSet
        |> List.fold(fun (fields: Map<string, ExecutionInfo list>) selection ->
            let includer = getIncluder selection.Directives info.Include
            let innerData = { info with Include = includer }
            match selection with
            | Field field ->
                abstractionInfo ctx (info.ReturnDef :?> AbstractDef) field typeCondition includer
                |> Map.map (fun _ data -> [ plan ctx data ])
                |> Map.merge (fun _ oldVal newVal -> oldVal @ newVal) fields
            | FragmentSpread spread ->
                let spreadName = spread.Name
                if !visitedFragments |> List.exists (fun name -> name = spreadName) 
                then fields  // fragment already found
                else
                    visitedFragments := spreadName::!visitedFragments
                    match ctx.Document.Definitions |> List.tryFind (function FragmentDefinition f -> f.Name.Value = spreadName | _ -> false) with
                    | Some (FragmentDefinition fragment) ->
                        // retrieve fragment data just as it was normal selection set
                        let fragmentInfo = planAbstraction ctx innerData fragment.SelectionSet visitedFragments fragment.TypeCondition
                        let (ResolveAbstraction(fragmentFields)) = fragmentInfo.Kind
                        // filter out already existing fields
                        Map.merge (fun _ oldVal newVal -> oldVal @ newVal) fields fragmentFields
                    | _ -> fields
            | InlineFragment fragment ->
                 // retrieve fragment data just as it was normal selection set
                 let fragmentInfo = planAbstraction ctx innerData fragment.SelectionSet visitedFragments fragment.TypeCondition
                 let (ResolveAbstraction(fragmentFields)) = fragmentInfo.Kind
                 // filter out already existing fields
                 Map.merge (fun _ oldVal newVal -> oldVal @ newVal) fields fragmentFields
            | _ -> fields
        ) Map.empty
    { info with Kind = ResolveAbstraction plannedTypeFields }

let private planVariables (schema: ISchema) (operation: OperationDefinition) =
    operation.VariableDefinitions 
    |> List.map (fun vdef ->
        let vname = vdef.VariableName
        match Values.tryConvertAst schema vdef.Type with
        | None -> raise (MalformedQueryException (sprintf "GraphQL query defined variable '$%s' of type '%s' which is not known in the current schema" vname (vdef.Type.ToString()) ))
        | Some tdef ->
            match tdef with
            | :? InputDef as idef ->
                { VarDef.Name = vname; TypeDef = idef; DefaultValue = vdef.DefaultValue }
            | _ -> raise (MalformedQueryException (sprintf "GraphQL query defined variable '$%s' of type '%s' which is not an input type definition" vname (tdef.ToString()))))

let internal planOperation documentId (ctx: PlanningContext) (operation: OperationDefinition) : ExecutionPlan =
    // create artificial plan info to start with
    let rootInfo = { 
        Identifier = null
        Kind = Unchecked.defaultof<ExecutionInfoKind>
        Ast = Unchecked.defaultof<Field>
        ParentDef = ctx.RootDef
        ReturnDef = ctx.RootDef
        Definition = Unchecked.defaultof<FieldDef> 
        Include = incl 
        IsNullable = false }
    let resolvedInfo = planSelection ctx rootInfo operation.SelectionSet (ref [])
    let (SelectFields(topFields)) = resolvedInfo.Kind
    let variables = planVariables ctx.Schema operation
    match operation.OperationType with
    | Query ->
        { DocumentId = documentId
          Operation = operation 
          Fields = topFields
          RootDef = ctx.Schema.Query
          Strategy = Parallel
          Variables = variables }
    | Mutation ->
        match ctx.Schema.Mutation with
        | Some mutationDef ->
            { DocumentId = documentId
              Operation = operation
              Fields = topFields
              RootDef = mutationDef
              Strategy = Sequential 
              Variables = variables }
        | None -> 
            raise (GraphQLException "Tried to execute a GraphQL mutation on schema with no mutation type defined")
    | Subscription ->
        match ctx.Schema.Subscription with
        | Some subscriptionDef ->
            { DocumentId = documentId
              Operation = operation
              Fields = topFields
              RootDef = subscriptionDef
              Strategy = Sequential 
              Variables = variables }
        | None -> 
            raise (GraphQLException "Tried to execute a GraphQL subscription on schema with no subscription type defined")