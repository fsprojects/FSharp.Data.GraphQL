/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Planning

open System
open System.Reflection
open System.Collections.Generic
open System.Collections.Concurrent
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Introspection

let SchemaMetaFieldDef = Define.Field(
    name = "__schema",
    description = "Access the current type schema of this server.",
    typedef = __Schema,
    resolve = fun ctx (_: obj) -> ctx.Schema.Introspected)
    
let TypeMetaFieldDef = Define.Field(
    name = "__type",
    description = "Request the type information of a single type.",
    typedef = __Type,
    args = [
        { Name = "name"
          Description = None
          Type = String
          DefaultValue = None
          ExecuteInput = variableOrElse(coerceStringInput >> Option.map box >> Option.toObj) }
    ],
    resolve = fun ctx (_:obj) -> 
        ctx.Schema.Introspected.Types 
        |> Seq.find (fun t -> t.Name = ctx.Arg("name")) 
        |> IntrospectionTypeRef.Named)
    
let TypeNameMetaFieldDef : FieldDef<obj> = Define.Field(
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
    
let private coerceVariables (schema: #ISchema) (variables: VariableDefinition list) (inputs: Map<string, obj> option) =
    match inputs with
    | None -> 
        variables
        |> List.filter (fun vardef -> Option.isSome vardef.DefaultValue)
        |> List.fold (fun acc vardef ->
            let variableName = vardef.VariableName
            Map.add variableName (coerceVariable schema vardef Map.empty) acc) Map.empty
    | Some vars -> 
        variables
        |> List.fold (fun acc vardef ->
            let variableName = vardef.VariableName
            Map.add variableName (coerceVariable schema vardef vars) acc) Map.empty

let objectData(ctx: PlanningContext, parentDef: ObjectDef, field: Field, includer: Includer) : PlanningData =
    match tryFindDef ctx.Schema parentDef field with
    | Some fdef ->
        { Identifier = field.AliasOrName
          ParentDef = parentDef
          Definition = fdef
          Ast = field
          IsNullable = fdef.Type :? NullableDef
          Include = includer }
    | None ->
        raise (GraphQLException (sprintf "No field '%s' was defined in object definition '%s'" field.Name parentDef.Name))

let abstractionData(ctx: PlanningContext, parentDef: AbstractDef, field: Field, typeCondition: string option, includer: Includer) : Map<string, PlanningData> =
    let objDefs = ctx.Schema.GetPossibleTypes parentDef
    match typeCondition with
    | None ->
        objDefs
        |> Array.choose (fun objDef ->
            match tryFindDef ctx.Schema objDef field with
            | Some fdef ->
                let data = 
                    { Identifier = field.AliasOrName
                      ParentDef = parentDef
                      Definition = fdef
                      Ast = field
                      IsNullable = fdef.Type :? NullableDef 
                      Include = includer }
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
                      ParentDef = parentDef
                      Definition = fdef
                      Ast = field
                      IsNullable = fdef.Type :? NullableDef 
                      Include = includer }
                Map.ofList [ objDef.Name, data ]
            | None -> Map.empty
        | None -> 
            let pname = parentDef :?> NamedDef
            raise (GraphQLException (sprintf "An abstract type '%s' has no relation with a type named '%s'" pname.Name typeName))
    
let private directiveIncluder (directive: Directive) : Includer =
    fun variables ->
        match directive.If.Value with
        | Variable vname -> downcast variables.[vname]
        | other -> 
            match coerceBoolInput other with
            | Some s -> s
            | None -> raise (
                GraphQLException (sprintf "Expected 'if' argument of directive '@%s' to have boolean value but got %A" directive.Name other))

let incl: Includer = fun _ -> true
let excl: Includer = fun _ -> false
let private getIncluder (directives: Directive list) : Includer =
    directives
    |> List.fold (fun acc directive ->
        match directive.Name with
        | "skip" ->
            let excluder = directiveIncluder directive >> not
            fun vars -> acc vars && excluder vars
        | "include" -> 
            let includer = directiveIncluder directive
            fun vars -> acc vars && includer vars
        | _ -> acc) incl

let private doesFragmentTypeApply (schema: ISchema) fragment (objectType: ObjectDef) = 
    match fragment.TypeCondition with
    | None -> true
    | Some typeCondition ->
        match schema.TryFindType typeCondition with
        | None -> false
        | Some conditionalType when conditionalType.Name = objectType.Name -> true
        | Some (Abstract conditionalType) -> schema.IsPossibleType conditionalType objectType
        | _ -> false
        
let rec private plan (ctx: PlanningContext) (data: PlanningData) (typedef: TypeDef) : ExecutionPlanInfo =
    match typedef with
    | Leaf leafDef -> planLeaf ctx data leafDef
    | Object objDef -> planSelection ctx { data with ParentDef = objDef } data.Ast.SelectionSet (ref [])
    | Nullable innerDef -> plan ctx { data with IsNullable = true } innerDef
    | List innerDef -> planList ctx data innerDef
    | Abstract abstractDef -> planAbstraction ctx { data with ParentDef = abstractDef } data.Ast.SelectionSet (ref []) None

and private planSelection (ctx: PlanningContext) (data: PlanningData) (selectionSet: Selection list) visitedFragments : ExecutionPlanInfo = 
    let parentDef = downcast data.ParentDef
    let plannedFields =
        selectionSet
        |> List.fold(fun (fields: ExecutionPlanInfo list) selection ->
            let includer = getIncluder selection.Directives
            match selection with
            | Field field ->
                let identifier = field.AliasOrName
                if fields |> List.exists (fun f -> f.Data.Identifier = identifier) 
                then fields
                else 
                    let data = objectData(ctx, parentDef, field, includer)
                    let executionPlan = plan ctx data data.Definition.Type
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
                        let (SelectFields(_, fragmentFields)) = planSelection ctx data fragment.SelectionSet visitedFragments
                        // filter out already existing fields
                        List.mergeBy (fun field -> field.Data.Identifier) fields fragmentFields
                    | _ -> fields
            | InlineFragment fragment when doesFragmentTypeApply ctx.Schema fragment parentDef ->
                 // retrieve fragment data just as it was normal selection set
                 let (SelectFields(_, fragmentFields)) = planSelection ctx data fragment.SelectionSet visitedFragments
                 // filter out already existing fields
                 List.mergeBy (fun field -> field.Data.Identifier) fields fragmentFields
            | _ -> fields
        ) []
    SelectFields(data, plannedFields)

and private planList (ctx: PlanningContext) (data: PlanningData) (innerDef: TypeDef) : ExecutionPlanInfo =
    ResolveCollection(data, plan ctx data innerDef)

and private planLeaf (ctx: PlanningContext) (data: PlanningData) (leafDef: LeafDef) : ExecutionPlanInfo =
    ResolveValue(data)

and private planAbstraction (ctx:PlanningContext) (data: PlanningData) (selectionSet: Selection list) visitedFragments typeCondition : ExecutionPlanInfo =
    let parentDef = downcast data.ParentDef
    let plannedTypeFields =
        selectionSet
        |> List.fold(fun (fields: Map<string, ExecutionPlanInfo list>) selection ->
            let includer = getIncluder selection.Directives
            match selection with
            | Field field ->
                abstractionData(ctx, parentDef, field, typeCondition, includer)
                |> Map.map (fun typeName data -> [ plan ctx data data.Definition.Type ])
                |> Map.merge (fun typeName oldVal newVal -> oldVal @ newVal) fields
            | FragmentSpread spread ->
                let spreadName = spread.Name
                if !visitedFragments |> List.exists (fun name -> name = spreadName) 
                then fields  // fragment already found
                else
                    visitedFragments := spreadName::!visitedFragments
                    match ctx.Document.Definitions |> List.tryFind (function FragmentDefinition f -> f.Name.Value = spreadName | _ -> false) with
                    | Some (FragmentDefinition fragment) ->
                        // retrieve fragment data just as it was normal selection set
                        let (ResolveAbstraction(_, fragmentFields)) = planAbstraction ctx data fragment.SelectionSet visitedFragments fragment.TypeCondition
                        // filter out already existing fields
                        Map.merge (fun typeName oldVal newVal -> oldVal @ newVal) fields fragmentFields
                    | _ -> fields
            | InlineFragment fragment ->
                 // retrieve fragment data just as it was normal selection set
                 let (ResolveAbstraction(_, fragmentFields)) = planAbstraction ctx data fragment.SelectionSet visitedFragments fragment.TypeCondition
                 // filter out already existing fields
                 Map.merge (fun typeName oldVal newVal -> oldVal @ newVal) fields fragmentFields
            | _ -> fields
        ) Map.empty
    ResolveAbstraction(data, plannedTypeFields)

let planOperation (ctx: PlanningContext) (operation: OperationDefinition) : ExecutionPlan =
    let data = { 
        Identifier = null;
        Ast = Unchecked.defaultof<Field>
        IsNullable = false
        ParentDef = ctx.RootDef
        Definition = Unchecked.defaultof<FieldDef> 
        Include = incl }
    let (SelectFields(_, topFields)) = planSelection ctx data operation.SelectionSet (ref [])
    match operation.OperationType with
    | Query ->
        { Operation = operation 
          Fields = topFields
          RootDef = ctx.Schema.Query
          Strategy = Parallel }
    | Mutation ->
        match ctx.Schema.Mutation with
        | Some mutationDef ->
            { Operation = operation
              Fields = topFields
              RootDef = mutationDef
              Strategy = Serial }
        | None -> 
            raise (GraphQLException "Tried to execute a GraphQL mutation on schema with no mutation type defined")