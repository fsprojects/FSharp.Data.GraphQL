// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Planning

open System
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
              TypeDef = StringType
              DefaultValue = None
              ExecuteInput = variableOrElse(InlineConstant >> coerceStringInput >> Option.map box >> Option.toObj) }
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
        typedef = StringType,
        resolve = fun ctx (_:obj) -> ctx.ParentType.Name)

let private tryFindDef (schema: ISchema) (objdef: ObjectDef) (field: Field) : FieldDef option =
        match field.Name with
        | "__schema" when Object.ReferenceEquals(schema.Query, objdef) -> Some (upcast SchemaMetaFieldDef)
        | "__type" when Object.ReferenceEquals(schema.Query, objdef) -> Some (upcast TypeMetaFieldDef)
        | "__typename" -> Some (upcast TypeNameMetaFieldDef)
        | fieldName -> objdef.Fields |> Map.tryFind fieldName

let private objectInfo (ctx: PlanningContext) (parentDef: ObjectDef) field includer =
    match tryFindDef ctx.Schema parentDef field with
    | Some fdef ->
        { Identifier = field.AliasOrName
          Kind = ResolveValue
          ParentDef = parentDef
          ReturnDef =
            match parentDef with
            | SubscriptionObject _ -> (fdef :?> SubscriptionFieldDef).OutputTypeDef
            | Object _ -> fdef.TypeDef
            | _ -> raise (GraphQLException (sprintf "Unexpected parentdef type!"))
          Definition = fdef
          Ast = field
          Include = includer
          IsNullable = false }
    | None ->
        raise (GraphQLException (sprintf "No field '%s' was defined in object definition '%s'" field.Name parentDef.Name))

let rec private abstractionInfo (ctx : PlanningContext) (parentDef : AbstractDef) field typeCondition includer =
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
        | VariableName vname -> downcast variables.[vname]
        | other ->
            match coerceBoolInput (InlineConstant other) with
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

let private isDeferredField (field: Field) =
    field.Directives |> List.exists(fun d -> d.Name = "defer")

let private isStreamedField (field : Field) =
    field.Directives |> List.exists(fun d -> d.Name = "stream")

let private getStreamBufferMode (field : Field) =
    let cast argName value =
        match value with
        | IntValue v -> int v
        | _ -> raise <| GraphQLException(sprintf "Stream directive parsing error: expected an integer value in argument '%s', but could not parse it." argName)
    let directive =
        field.Directives
        |> List.tryFind (fun d -> d.Name = "stream")
    let getArg argName (d : Directive) =
        d.Arguments
        |> List.tryFind (fun x -> x.Name = argName)
        |> Option.map (fun x -> x.Value |> cast argName)
    let interval = getArg "interval"
    let preferredBatchSize = getArg "preferredBatchSize"
    match directive with
    | Some d -> { Interval = interval d; PreferredBatchSize = preferredBatchSize d }
    | None -> failwithf "Expected Stream directive on field '%s', but it does not exist." field.AliasOrName

let private isLiveField (field : Field) =
    field.Directives |> List.exists (fun d -> d.Name = "live")

let private (|Planned|Deferred|Streamed|Live|) field =
    if isStreamedField field then Streamed (getStreamBufferMode field)
    elif isDeferredField field then Deferred
    elif isLiveField field then Live
    else Planned

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
         | _ -> failwithf "Cannot merge ExecutionInfos with different kinds!"
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

let rec private plan (ctx : PlanningContext) (info : ExecutionInfo) : ExecutionInfo =
    match info.ReturnDef with
    | Leaf _ -> info
    | SubscriptionObject _ -> planSelection ctx info.Ast.SelectionSet info (ref [])
    | Object _ -> planSelection ctx info.Ast.SelectionSet info (ref [])
    | Nullable returnDef ->
        let inner = plan ctx { info with ParentDef = info.ReturnDef; ReturnDef = downcast returnDef }
        { inner with IsNullable = true }
    | List returnDef ->
        // We dont yet know the indices of our elements so we append a dummy value on
        let inner = plan ctx { info with ParentDef = info.ReturnDef; ReturnDef = downcast returnDef; }
        { info with Kind = ResolveCollection inner }
    | Abstract _ ->
        planAbstraction ctx info.Ast.SelectionSet info (ref []) None
    | _ -> failwith "Invalid Return Type in Planning!"

and private planSelection (ctx: PlanningContext) (selectionSet: Selection list) (info: ExecutionInfo) visitedFragments : ExecutionInfo =
    let parentDef = downcast info.ReturnDef
    let plannedFields =
        selectionSet
        |> List.fold(fun (fields : ExecutionInfo list) (selection : Selection) ->
            // FIXME: includer is not passed along from top level fragments (both inline and spreads)
            let includer = getIncluder selection.Directives info.Include
            let updatedInfo = { info with Include = includer }
            match selection with
            | Field field ->
                let identifier = field.AliasOrName
                if fields |> List.exists (fun f -> f.Identifier = identifier)
                then fields
                else
                    let innerInfo = objectInfo ctx parentDef field includer
                    let executionPlan = plan ctx innerInfo
                    match field with
                    | Deferred -> fields @ [ { executionPlan with Kind = ResolveDeferred executionPlan } ]
                    | Live -> fields @ [ { executionPlan with Kind = ResolveLive executionPlan } ]
                    | Streamed mode -> fields @ [ { executionPlan with Kind = ResolveStreamed (executionPlan, mode) } ]
                    | Planned -> fields @ [ executionPlan ]
            | FragmentSpread spread ->
                let spreadName = spread.Name
                if visitedFragments.Value |> List.exists (fun name -> name = spreadName)
                then fields // Fragment already found
                else
                    visitedFragments.Value <- spreadName :: visitedFragments.Value
                    match ctx.Document.Definitions |> List.tryFind (function FragmentDefinition f -> f.Name.Value = spreadName | _ -> false) with
                    | Some (FragmentDefinition fragment) when doesFragmentTypeApply ctx.Schema fragment parentDef ->
                        // Retrieve fragment data just as it was normal selection set
                        // TODO: Check if the path is correctly defined
                        let fragmentInfo = planSelection ctx fragment.SelectionSet updatedInfo visitedFragments
                        let fragmentFields = getSelectionFrag fragmentInfo.Kind
                        // filter out already existing fields
                        deepMerge fields fragmentFields
                        // List.mergeBy (fun field -> field.Identifier) fields fragmentFields, deferedFields'
                    | _ -> fields
            | InlineFragment fragment when doesFragmentTypeApply ctx.Schema fragment parentDef ->
                 // retrieve fragment data just as it was normal selection set
                 let fragmentInfo = planSelection ctx fragment.SelectionSet updatedInfo visitedFragments
                 let fragmentFields = getSelectionFrag fragmentInfo.Kind
                 // filter out already existing fields
                 deepMerge fields fragmentFields
            | _ -> fields
        ) []
    { info with Kind = SelectFields plannedFields }

and private planAbstraction (ctx:PlanningContext) (selectionSet: Selection list) (info : ExecutionInfo) visitedFragments typeCondition : ExecutionInfo =
    let plannedTypeFields =
        selectionSet
        |> List.fold(fun (fields : Map<string, ExecutionInfo list>) selection ->
            let includer = getIncluder selection.Directives info.Include
            let innerData = { info with Include = includer }
            match selection with
            | Field field ->
                let a = abstractionInfo ctx (info.ReturnDef :?> AbstractDef) field typeCondition includer
                let infoMap = Map.map (fun _ data -> [plan ctx data]) a
                let withKind update m : Map<string, ExecutionInfo list> = Map.map (fun _ -> List.map(fun info -> { info with Kind = update info })) m
                match field with
                | Deferred -> Map.merge (fun _ -> deepMerge) fields <| withKind ResolveDeferred infoMap
                | Live -> Map.merge (fun _ -> deepMerge) fields <| withKind ResolveLive infoMap
                | Streamed mode -> Map.merge(fun _ -> deepMerge) fields <| withKind (fun info -> ResolveStreamed (info, mode)) infoMap
                | Planned -> Map.merge (fun _ -> deepMerge) fields infoMap
            | FragmentSpread spread ->
                let spreadName = spread.Name
                if visitedFragments.Value |> List.exists (fun name -> name = spreadName)
                then fields // Fragment already found
                else
                    visitedFragments.Value <- spreadName :: visitedFragments.Value
                    match ctx.Document.Definitions |> List.tryFind (function FragmentDefinition f -> f.Name.Value = spreadName | _ -> false) with
                    | Some (FragmentDefinition fragment) ->
                        // Retrieve fragment data just as it was normal selection set
                        let fragmentInfo = planAbstraction ctx fragment.SelectionSet innerData visitedFragments fragment.TypeCondition
                        let fragmentFields = getAbstractionFrag fragmentInfo.Kind
                        // Filter out already existing fields
                        Map.merge (fun _ -> deepMerge) fields fragmentFields
                    | _ -> fields
            | InlineFragment fragment ->
                // Retrieve fragment data just as it was normal selection set
                let fragmentInfo = planAbstraction ctx fragment.SelectionSet innerData visitedFragments fragment.TypeCondition
                let fragmentFields = getAbstractionFrag fragmentInfo.Kind
                // Filter out already existing fields
                Map.merge (fun _ -> deepMerge) fields fragmentFields
        ) Map.empty
    if Map.isEmpty plannedTypeFields
    then { info with Kind = ResolveDeferred info }
    else { info with Kind = ResolveAbstraction plannedTypeFields }

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

let internal planOperation (ctx: PlanningContext) : ExecutionPlan =
    // Create artificial plan info to start with
    let rootInfo = {
        Identifier = null
        Kind = Unchecked.defaultof<ExecutionInfoKind>
        Ast = Unchecked.defaultof<Field>
        ParentDef = ctx.RootDef
        ReturnDef = ctx.RootDef
        Definition = Unchecked.defaultof<FieldDef>
        Include = incl
        IsNullable = false }
    let resolvedInfo = planSelection ctx ctx.Operation.SelectionSet rootInfo (ref [])
    let topFields =
        match resolvedInfo.Kind with
        | SelectFields tf -> tf
        | x -> failwith <| sprintf "Expected SelectFields Kind, but got %A" x
    let variables = planVariables ctx.Schema ctx.Operation
    match ctx.Operation.OperationType with
    | Query ->
        { DocumentId = ctx.DocumentId
          Operation = ctx.Operation
          Fields = topFields
          RootDef = ctx.Schema.Query
          Strategy = Parallel
          Variables = variables
          Metadata = ctx.Metadata
          ValidationResult = ctx.ValidationResult }
    | Mutation ->
        match ctx.Schema.Mutation with
        | Some mutationDef ->
            { DocumentId = ctx.DocumentId
              Operation = ctx.Operation
              Fields = topFields
              RootDef = mutationDef
              Strategy = Sequential
              Variables = variables
              Metadata = ctx.Metadata
              ValidationResult = ctx.ValidationResult }
        | None ->
            raise (GraphQLException "Tried to execute a GraphQL mutation on schema with no mutation type defined")
    | Subscription ->
        match ctx.Schema.Subscription with
        | Some subscriptionDef ->
            { DocumentId = ctx.DocumentId
              Operation = ctx.Operation
              Fields = topFields
              RootDef = subscriptionDef
              Strategy = Sequential
              Variables = variables
              Metadata = ctx.Metadata
              ValidationResult = ctx.ValidationResult }
        | None ->
            raise (GraphQLException "Tried to execute a GraphQL subscription on schema with no mutation type defined")
