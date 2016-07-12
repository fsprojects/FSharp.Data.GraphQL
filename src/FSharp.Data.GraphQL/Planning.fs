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
    
type PlanningContext =
    { Schema: ISchema
      Document: Document }

type Includer = Map<string,obj> -> bool
type PlanningData =
    { ParentDef: ObjectDef
      Definition: FieldDef
      IsNullable: bool
      Ast: Field }
    static member Create(ctx: PlanningContext, parentDef: ObjectDef, field: Field) : PlanningData =
        match tryFindDef ctx.Schema parentDef field with
        | Some fdef ->
            { ParentDef = parentDef
              Definition = fdef
              Ast = field
              IsNullable = fdef.Type :? NullableDef }
        | None ->
            raise (GraphQLException (sprintf "No field '%s' was defined in object definition '%s'" field.Name parentDef.Name))

/// plan of reduction being a result of application of a query AST on existing schema
type ExecutionPlan =
    // reducer for scalar or enum
    | ResolveValue of data:PlanningData
    // reducer for selection set applied upon output object
    | SelectFields of data:PlanningData * fields:ExecutionPlan list
    // reducer for each of the collection elements
    | ResolveCollection of data:PlanningData * elementPlan:ExecutionPlan
    // reducer for union and interface types to be resolved into ReduceSelection at runtime
    | ResolveAbstraction of data:PlanningData * chooseReduction:Map<string, ExecutionPlan>
    member x.Data = 
        match x with
        | ResolveValue(data) -> data
        | SelectFields(data, _) -> data
        | ResolveCollection(data, _) -> data
        | ResolveAbstraction(data, _) -> data


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

//let rec private findGroupIndexByName (groupedFields: System.Collections.Generic.List<string * Field>) (name: string) (i: int) : int =
//    if i < 0
//    then -1
//    else 
//        let (k, _) = groupedFields.[i]
//        if k = name then i
//        else findGroupIndexByName groupedFields name (i-1)
//
//let rec private groupFields (ctx: PlanningContext) typedef (selectionSet: Selection list) (visitedFragments): (string * Field []) [] =
//    let groupedFields = System.Collections.Generic.List(selectionSet.Length)
//    selectionSet 
//    |> List.iteri(fun i selection ->
//        match selection with
//        | Field field ->
//            let name = field.AliasOrName
//            match findGroupIndexByName groupedFields name (groupedFields.Count-1) with
//            | -1 -> 
//                groupedFields.Add (name, [| field |])
//            | idx -> 
//                let (_, value) = groupedFields.[idx]
//                groupedFields.[idx] <- (name, Array.append [| field |] value)
//        | FragmentSpread spread ->
//            let fragmentSpreadName = spread.Name
//            if not (List.exists (fun fragmentName -> fragmentName = fragmentSpreadName) !visitedFragments)
//            then 
//                visitedFragments := (fragmentSpreadName::!visitedFragments)
//                let found =
//                    ctx.Document.Definitions
//                    |> List.tryFind (function FragmentDefinition f when f.Name.Value = fragmentSpreadName -> true | _ -> false)
//                match found with
//                | Some (FragmentDefinition fragment) -> 
//                    if doesFragmentTypeApply ctx.Schema fragment typedef
//                    then 
//                        let fragmentSelectionSet = fragment.SelectionSet
//                        let fragmentGroupedFieldSet = groupFields ctx typedef fragmentSelectionSet visitedFragments
//                        for j = 0 to fragmentGroupedFieldSet.Length - 1 do
//                            let (responseKey, fragmentGroup) = fragmentGroupedFieldSet.[j]
//                            match findGroupIndexByName groupedFields responseKey (groupedFields.Count-1) with
//                            | -1 ->
//                                groupedFields.Add (responseKey, fragmentGroup)
//                            | idx ->
//                                let (_, value) = groupedFields.[idx]
//                                groupedFields.[idx] <- (responseKey, Array.append fragmentGroup value)
//                | _ -> ()
//        | InlineFragment fragment -> 
//            if doesFragmentTypeApply ctx.Schema fragment typedef
//            then 
//                let fragmentSelectionSet = fragment.SelectionSet
//                let fragmentGroupedFieldSet = groupFields ctx typedef fragmentSelectionSet visitedFragments
//                for j = 0 to fragmentGroupedFieldSet.Length - 1 do
//                    let (responseKey, fragmentGroup) = fragmentGroupedFieldSet.[j]
//                    match findGroupIndexByName groupedFields responseKey (groupedFields.Count-1) with
//                    | -1 ->
//                        groupedFields.Add (responseKey, fragmentGroup)
//                    | idx ->
//                        let (_, value) = groupedFields.[idx]
//                        groupedFields.[idx] <- (responseKey, Array.append fragmentGroup value))
//    groupedFields.ToArray()

let rec private plan (ctx: PlanningContext) (data: PlanningData) (typedef: TypeDef) : ExecutionPlan =
    match typedef with
    | Leaf leafDef -> planLeaf ctx data leafDef
    | Object objDef -> planSelection ctx { data with ParentDef = objDef } data.Ast.SelectionSet (ref [])
    | Nullable innerDef -> plan ctx { data with IsNullable = true } innerDef
    | List innerDef -> planList ctx data innerDef
    | Abstract abstractDef -> planAbstraction ctx data abstractDef

and private planSelection (ctx: PlanningContext) (data: PlanningData) (selectionSet: Selection list) visitedFragments : ExecutionPlan = 
    let plannedFields =
        selectionSet
        |> List.fold(fun fields selection ->
            match selection with
            | Field field ->
                let identifier = field.AliasOrName
                match fields |> List.tryFindIndex (fun (name, f) -> name = identifier) with
                | None ->
                    let data = PlanningData.Create(ctx, data.ParentDef, field)
                    let executionPlan = plan ctx data data.Definition.Type
                    (identifier, executionPlan)::fields
                | Some _ -> fields
            | FragmentSpread spread ->
                let spreadName = spread.Name
                if !visitedFragments |> List.exists (fun name -> name = spreadName) 
                then fields  // fragment already found
                else
                    visitedFragments := spreadName::!visitedFragments
                    match ctx.Document.Definitions |> List.tryFind (function FragmentDefinition f -> f.Name.Value = spreadName | _ -> false) with
                    | Some (FragmentDefinition fragment) when doesFragmentTypeApply ctx.Schema fragment data.ParentDef ->
                        // retrieve fragment data just as it was normal selection set
                        let (SelectFields(_, fragmentFields)) = planSelection ctx data fragment.SelectionSet visitedFragments
                        // filter out already existing fields
                        let distinctFields =
                            fragmentFields
                            |> List.map (fun plan -> (plan.Data.Ast.AliasOrName, plan))
                            |> List.filter (fun (ffname, _) -> not <| List.exists (fun (name, _) -> name = ffname) fields)
                        distinctFields @ fields
                    | _ -> fields
            | InlineFragment fragment when doesFragmentTypeApply ctx.Schema fragment data.ParentDef ->
                 // retrieve fragment data just as it was normal selection set
                 let (SelectFields(_, fragmentFields)) = planSelection ctx data fragment.SelectionSet visitedFragments
                 // filter out already existing fields
                 let distinctFields =
                     fragmentFields
                     |> List.map (fun plan -> (plan.Data.Ast.AliasOrName, plan))
                     |> List.filter (fun (ffname, _) -> not <| List.exists (fun (name, _) -> name = ffname) fields)
                 distinctFields @ fields
            | _ -> fields
        ) []
    SelectFields(data, plannedFields |> List.map snd)
and private planList (ctx: PlanningContext) (data: PlanningData) (innerDef: TypeDef) : ExecutionPlan =
    ResolveCollection(data, plan ctx data innerDef)
and private planLeaf (ctx: PlanningContext) (data: PlanningData) (leafDef: LeafDef) : ExecutionPlan =
    ResolveValue(data)
and private planAbstraction (ctx:PlanningContext) (data: PlanningData) (abstractDef: AbstractDef) : ExecutionPlan =
    
    ResolveAbstraction(data,)

let planOperation (ctx: PlanningContext) (operation: OperationDefinition) : ExecutionPlan =
    planSelection ctx () (downcast ctx.ParentDef) operation.SelectionSet