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
          ReturnDef = 
            match parentDef with
            | SubscriptionObject _ -> (fdef :?> SubscriptionFieldDef).InputTypeDef
            | Object _ -> fdef.TypeDef
            | _ -> raise (GraphQLException (sprintf "Unexpected parentdef type!"))
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

let private isDeferredField (field: Field) : bool =
    field.Directives |> List.exists(fun d -> d.Name = "defer")

let private isStreamedField (field : Field) : bool =
    field.Directives |> List.exists (fun d -> d.Name = "stream")
                
type PlanningStage = ExecutionInfo * DeferredExecutionInfo list * StreamedExecutionInfo list * string list

let private getSelectionFrag = function
    | SelectFields(fragmentFields) -> fragmentFields
    | _ -> failwith "Expected a Selection!"

let private getAbstractionFrag = function
    | ResolveAbstraction(fragmentFields) -> fragmentFields
    | _ -> failwith "Expected an Abstraction!"

let rec private deepMerge (xs: ExecutionInfo list) (ys: ExecutionInfo list) =
     let rec merge (x: ExecutionInfo) (y: ExecutionInfo) =
         match x.Kind, y.Kind with 
         | ResolveValue, ResolveValue -> x
         | ResolveCollection(x'), ResolveCollection(y') -> { x with Kind = ResolveCollection(merge x' y') }
         | ResolveAbstraction(xs'), ResolveAbstraction(ys') -> { x with Kind = ResolveAbstraction(Map.merge (fun _ x' y' -> deepMerge x' y') xs' ys')} 
         | SelectFields(xs'), SelectFields(ys') -> { x with Kind = SelectFields(deepMerge xs' ys') }
         | k1, k2 -> failwithf "Cannot merge ExecutionInfos with different kinds!"
     // Apply the merge to every conflict
     let xs' = 
         xs  
         |> List.fold(fun acc x -> 
             match List.tryFind(fun y -> y.Identifier = x.Identifier) ys with
             | Some y -> (merge x y)::acc
             | None -> x::acc) [] 
         |> List.rev
     // Remove all merged conflicts from ys
     let ys' =
         ys
         |> List.filter(fun y -> not <| List.exists(fun x -> x.Identifier = y.Identifier) xs')
     xs' @ ys'
 

let rec private plan (ctx : PlanningContext) (stage : PlanningStage) : PlanningStage =
    let info, deferredFields, streamedFields, path = stage
    match info.ReturnDef with
    | Leaf _ -> info, deferredFields, streamedFields, info.Identifier::path
    | SubscriptionObject _ -> planSelection ctx info.Ast.SelectionSet (info, deferredFields, streamedFields, info.Identifier::path) (ref [])
    | Object _ -> planSelection ctx info.Ast.SelectionSet (info, deferredFields, streamedFields, info.Identifier::path) (ref [])
    | Nullable returnDef -> 
        let inner, deferredFields', streamedFields', path' = plan ctx ({ info with ParentDef = info.ReturnDef; ReturnDef = downcast returnDef }, deferredFields, streamedFields, path)
        { inner with IsNullable = true}, deferredFields', streamedFields', path'
    | List returnDef -> 
        // We dont yet know the indicies of our elements so we append a dummy value on
        let inner, deferredFields', streamedFields', path' = plan ctx ({ info with ParentDef = info.ReturnDef; ReturnDef = downcast returnDef; Identifier = "__index" }, deferredFields, streamedFields, "__index"::info.Identifier::path)
        { info with Kind = ResolveCollection inner }, deferredFields', streamedFields', path'
    | Abstract _ -> planAbstraction ctx info.Ast.SelectionSet (info, deferredFields, streamedFields, path) (ref []) None 
    | _ -> failwith "Invalid Return Type in Planning!"

and private planSelection (ctx: PlanningContext) (selectionSet: Selection list) (stage: PlanningStage) visitedFragments : PlanningStage = 
    let info, deferredFields, streamedFields, path = stage
    let parentDef = downcast info.ReturnDef
    let plannedFields, deferredFields', streamedFields' =
        selectionSet
        |> List.fold(fun (fields : ExecutionInfo list, deferredFields : DeferredExecutionInfo list, streamedFields : StreamedExecutionInfo list) selection ->
            //FIXME: includer is not passed along from top level fragments (both inline and spreads)
            let includer = getIncluder selection.Directives info.Include
            let updatedInfo = { info with Include = includer }
            match selection with
            | Field field ->
                let identifier = field.AliasOrName
                if fields |> List.exists (fun f -> f.Identifier = identifier) 
                then fields, deferredFields, streamedFields
                else 
                    let innerInfo = objectInfo(ctx, parentDef, field, includer)
                    let executionPlan, deferredFields', streamedFields', path' = plan ctx (innerInfo, deferredFields, streamedFields, path)
                    if isDeferredField field
                    then (fields, {Info = {info with Kind = SelectFields [executionPlan]}; Path = path'} :: deferredFields', streamedFields')
                    elif isStreamedField field
                    then (fields, deferredFields', {Info = {info with Kind = SelectFields [executionPlan]}; Path = path'} :: streamedFields')
                    else (fields @ [executionPlan], deferredFields', streamedFields') // unfortunatelly, order matters here
            | FragmentSpread spread ->
                let spreadName = spread.Name
                if !visitedFragments |> List.exists (fun name -> name = spreadName) 
                then fields, deferredFields, streamedFields  // fragment already found
                else
                    visitedFragments := spreadName::!visitedFragments
                    match ctx.Document.Definitions |> List.tryFind (function FragmentDefinition f -> f.Name.Value = spreadName | _ -> false) with
                    | Some (FragmentDefinition fragment) when doesFragmentTypeApply ctx.Schema fragment parentDef ->
                        // retrieve fragment data just as it was normal selection set
                        // TODO: Check if the path is correctly defined
                        let fragmentInfo, deferredFields', streamedFields', path' = planSelection ctx fragment.SelectionSet (updatedInfo, deferredFields, streamedFields, path) visitedFragments 
                        let fragmentFields = getSelectionFrag fragmentInfo.Kind
                        // filter out already existing fields
                        deepMerge fields fragmentFields, deferredFields', streamedFields'
                        // List.mergeBy (fun field -> field.Identifier) fields fragmentFields, deferedFields'
                    | _ -> fields, deferredFields, streamedFields
            | InlineFragment fragment when doesFragmentTypeApply ctx.Schema fragment parentDef ->
                 // retrieve fragment data just as it was normal selection set
                 let fragmentInfo, deferredFields', streamedFields', path' = planSelection ctx fragment.SelectionSet (updatedInfo, deferredFields, streamedFields, path) visitedFragments
                 let fragmentFields = getSelectionFrag fragmentInfo.Kind
                 // filter out already existing fields
                 deepMerge fields fragmentFields, deferredFields', streamedFields'
            | _ -> fields, deferredFields, streamedFields
        ) ([],deferredFields, streamedFields)
    { info with Kind = SelectFields plannedFields }, deferredFields', streamedFields', path
    
and private planAbstraction (ctx:PlanningContext) (selectionSet: Selection list) (stage:PlanningStage) visitedFragments typeCondition : PlanningStage =
    let info, deferredFields, streamedFields, path = stage
    let plannedTypeFields, deferredFields', streamedFields' =
        selectionSet
        |> List.fold(fun (fields : Map<string, ExecutionInfo list>, deferredFields : DeferredExecutionInfo list, streamedFields : StreamedExecutionInfo list) selection ->
            let includer = getIncluder selection.Directives info.Include
            let innerData = { info with Include = includer }
            match selection with
            | Field field ->
                let a = abstractionInfo ctx (info.ReturnDef :?> AbstractDef) field typeCondition includer
                // Make sure that we properly deal with the deferred and streamed fields
                let foldPlan (f:Map<string, ExecutionInfo list>, d, s, _) k data =
                    let f', d', s', p' = plan ctx (data, d, s, path)
                    f.Add(k, [f']), d', s', p'
                let infoMap, deferredFields', streamedFields', path' = Map.fold (foldPlan) (Map.empty, deferredFields, streamedFields, []) a  
                if isDeferredField field
                then 
                    printfn "Abstraction Info Keys: %A" (a |> Map.toSeq |> Seq.map fst)
                    printfn "InfoMap Keys: %A" (infoMap |> Map.toSeq |> Seq.map fst)
                    fields, {Info = { innerData with Kind = ResolveAbstraction infoMap}; Path = path'} :: deferredFields', streamedFields'
                elif isStreamedField field
                then
                    printfn "Abstraction Info Keys: %A" (a |> Map.toSeq |> Seq.map fst)
                    printfn "InfoMap Keys: %A" (infoMap |> Map.toSeq |> Seq.map fst)
                    fields, deferredFields', {Info = { innerData with Kind = ResolveAbstraction infoMap}; Path = path'} :: streamedFields'
                else Map.merge (fun _ oldVal newVal -> deepMerge oldVal newVal) fields infoMap, deferredFields', streamedFields'
            | FragmentSpread spread ->
                let spreadName = spread.Name
                if !visitedFragments |> List.exists (fun name -> name = spreadName) 
                then fields, deferredFields, streamedFields  // fragment already found
                else
                    visitedFragments := spreadName::!visitedFragments
                    match ctx.Document.Definitions |> List.tryFind (function FragmentDefinition f -> f.Name.Value = spreadName | _ -> false) with
                    | Some (FragmentDefinition fragment) ->
                        // retrieve fragment data just as it was normal selection set
                        let fragmentInfo, deferredFields', streamedFields', path' = planAbstraction ctx fragment.SelectionSet (innerData, deferredFields, streamedFields, path) visitedFragments fragment.TypeCondition
                        let fragmentFields = getAbstractionFrag fragmentInfo.Kind
                        // filter out already existing fields
                        Map.merge (fun _ oldVal newVal -> deepMerge oldVal newVal) fields fragmentFields, deferredFields', streamedFields'
                    | _ -> fields, deferredFields, streamedFields
            | InlineFragment fragment ->
                // retrieve fragment data just as it was normal selection set
                let fragmentInfo, deferredFields', streamedFields', path' = planAbstraction ctx fragment.SelectionSet (innerData, deferredFields, streamedFields, path) visitedFragments fragment.TypeCondition
                let fragmentFields = getAbstractionFrag fragmentInfo.Kind
                // filter out already existing fields
                Map.merge (fun _ oldVal newVal -> deepMerge oldVal newVal) fields fragmentFields, deferredFields', streamedFields'
        ) (Map.empty, deferredFields, streamedFields)
    { info with Kind = ResolveAbstraction plannedTypeFields }, deferredFields', streamedFields', path

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
    let resolvedInfo, deferredFields, streamedFields, _ = planSelection ctx operation.SelectionSet (rootInfo, [], [], []) (ref [])
    let deferredFields' = 
        deferredFields
        |> List.map (fun d -> {d with Path = List.rev d.Path})
    let streamedFields' =
        streamedFields
        |> List.map (fun d -> {d with Path = List.rev d.Path})
    let (SelectFields(topFields)) = resolvedInfo.Kind
    let variables = planVariables ctx.Schema operation
    match operation.OperationType with
    | Query ->
        { DocumentId = documentId
          Operation = operation 
          Fields = topFields
          DeferredFields = deferredFields'
          StreamedFields = streamedFields'
          RootDef = ctx.Schema.Query
          Strategy = Parallel
          Variables = variables }
    | Mutation ->
        match ctx.Schema.Mutation with
        | Some mutationDef ->
            { DocumentId = documentId
              Operation = operation
              Fields = topFields
              DeferredFields = deferredFields'
              StreamedFields = streamedFields'
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
              DeferredFields = deferredFields'
              StreamedFields = streamedFields'
              RootDef = subscriptionDef
              Strategy = Sequential 
              Variables = variables }
        | None -> 
            raise (GraphQLException "Tried to execute a GraphQL subscription on schema with no mutation type defined")