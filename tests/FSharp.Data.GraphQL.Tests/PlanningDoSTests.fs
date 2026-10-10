module FSharp.Data.GraphQL.Tests.PlanningDoSTests

open System
open System.Collections.Immutable
open System.Text
open System.Text.Json
open Xunit

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Server.Middleware
open FSharp.Data.GraphQL.Types

/// A value of the test schemas, resolved as the object type named by its Type
type Node = { Type : string; Id : string; Child : Node voption; Items : Node list }

#nowarn "40"

/// The interface Node, whose field child returns the interface itself
let private nodeInterface () : InterfaceDef<Node> =
    let rec nodeType : InterfaceDef<Node> =
        DefineRec.Interface<Node>("Node", (fun () -> [ Define.Field ("id", StringType); Define.Field ("child", Nullable nodeType) ]))
    nodeType

#warnon "40"

/// An object type implementing the interface, whose field child returns the given type and whose field items lists nodes
let private implementation (nodeType : InterfaceDef<Node>) (childType : OutputDef<Node option>) (name : string) =
    Define.Object<Node>(
        name = name,
        isTypeOf =
            (fun value ->
                match value with
                | :? Node as node -> String.Equals (node.Type, name, StringComparison.Ordinal)
                | _ -> false),
        interfaces = [ nodeType ],
        fields = [
            Define.Field ("id", StringType, (fun _ node -> node.Id))
            Define.Field ("child", childType, (fun _ node -> node.Child |> ValueOption.toOption))
            Define.Field ("items", ListOf nodeType, (fun _ node -> node.Items :> Node seq))
        ]
    )

let private schemaOf (nodeType : InterfaceDef<Node>) (implementations : ObjectDef<Node> list) (root : Node) =
    let queryType =
        Define.Object<obj>(
            "Query",
            [
                Define.Field ("node", Nullable nodeType, (fun _ _ -> Some root))
                Define.Field ("hello", StringType, (fun _ _ -> "world"))
            ]
        )
    let config = {
        SchemaConfig.Default with
            Types = implementations |> Seq.cast<NamedDef> |> List.ofSeq
    }
    Schema (queryType, config = config)

let private root = {
    Type = "A"
    Id = "1"
    Child =
        ValueSome {
            Type = "B"
            Id = "2"
            Child = ValueSome { Type = "C"; Id = "3"; Child = ValueNone; Items = [] }
            Items = []
        }
    Items = []
}

/// Node implemented by A, B and C, whose field child returns Node, with the given node as the root; a new schema each
/// time, because a middleware may change the schema of its executor
let private nodeSchemaWith (root : Node) =
    let nodeType = nodeInterface ()
    let implementations =
        [ "A"; "B"; "C" ]
        |> List.map (implementation nodeType (Nullable nodeType))
    schemaOf nodeType implementations root

let private nodeSchema () = nodeSchemaWith root

let private executor = Executor (nodeSchema ())

/// Node implemented by A0 to A3, whose field child returns B0 to B3, and by B0 to B3, whose field child returns Node,
/// with the given node as the root
let private covariantSchema (root : Node) =
    let nodeType = nodeInterface ()
    let bTypes = [ for i in 0..3 -> implementation nodeType (Nullable nodeType) $"B%i{i}" ]
    let aTypes =
        bTypes
        |> List.mapi (fun i bType -> implementation nodeType (Nullable bType) $"A%i{i}")
    schemaOf nodeType [ yield! aTypes; yield! bTypes ] root

let private covariantExecutor = Executor (covariantSchema root)

let private noVariables = ImmutableDictionary<string, JsonElement>.Empty

/// Generous enough for slow CI machines, yet far below the minutes or hours that a quadratic or exponential planning takes
let private timeout = TimeSpan.FromSeconds 10.0

let private planIsolated (executor : Executor<obj>) (query : string) =
    match runOnSmallStack timeout (fun () -> executor.CreateExecutionPlan query) with
    | Ok plan -> plan
    | Error (struct (_, errors)) ->
        let messages =
            errors
            |> List.map _.Message
            |> String.concat Environment.NewLine
        failwith $"Expected an execution plan, but the document was rejected:%s{Environment.NewLine}%s{messages}"

/// Plans the first operation of the document without validating it, so that the document can exceed the limits of validation
let private planWithoutValidation (schema : #ISchema) (document : string) =
    let ast = Parser.parse document
    let operation =
        ast.Definitions
        |> List.pick (function
            | OperationDefinition operation -> Some operation
            | _ -> None)
    let context : PlanningContext = {
        Schema = schema :> ISchema
        RootDef = schema.Query
        Document = ast
        Operation = operation
        DocumentId = 0
        Metadata = Metadata.Empty
    }
    runOnSmallStack timeout (fun () -> Planning.planOperation context)

let private executeIsolated (executor : Executor<obj>) (plan : ExecutionPlan) (variables : ImmutableDictionary<string, JsonElement>) =
    runOnSmallStack timeout (fun () ->
        executor.AsyncExecute (plan, getMockInputContext, variables = variables)
        |> sync)

/// The field nested to the depth, with the selection at the bottom
let private nestedAround (depth : int) (field : string) (bottom : string) =
    let document = StringBuilder ()
    for _ in 1..depth do
        document.Append $" %s{field} {{" |> ignore
    document.Append bottom |> ignore
    for _ in 1..depth do
        document.Append " }" |> ignore
    document.ToString ()

/// The field nested to the depth, selecting id at the bottom
let private nestedChain (depth : int) (field : string) = nestedAround depth field " id"

/// The node field with the field nested to the depth under it
let private nestedUnderNode (depth : int) (field : string) = $"{{ node {{%s{nestedChain depth field} }} }}"

[<Fact>]
let ``Planning fields nested under an interface completes quickly`` () =
    // Each level is planned for every implementation of the interface: 3^30 plans without reusing them
    let plan = planIsolated executor (nestedUnderNode 30 "child")
    plan.Fields |> List.map _.Identifier |> equals [ "node" ]

[<Fact>]
let ``Planning fields nested under covariant implementations with directives completes quickly`` () =
    // The implementations return different types, so each plans the next level with includers of its own unless
    // the includers built for the same directives are reused: 4^12 plans
    let plan = planIsolated covariantExecutor (nestedUnderNode 24 "child @include(if: true)")
    plan.Fields |> List.map _.Identifier |> equals [ "node" ]

[<Fact>]
let ``Plans reused for the implementations of an interface execute correctly`` () =
    let query = "{ node { id child { id child { id __typename ... on C { leaf: id } } } } }"
    let result = executor.AsyncExecute (query, getMockInputContext) |> sync
    let expected =
        NameValueLookup.ofList [
            "node",
            upcast
                NameValueLookup.ofList [
                    "id", upcast "1"
                    "child",
                    upcast
                        NameValueLookup.ofList [
                            "id", upcast "2"
                            "child", upcast NameValueLookup.ofList [ "id", upcast "3"; "__typename", upcast "C"; "leaf", upcast "3" ]
                        ]
                ]
        ]
    ensureDirect result
    <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Planning a nested selection selected twice under an interface completes quickly`` () =
    // Merging the two plans walks the plans they share once for every implementation at every level: 3^20 merges
    let chain = nestedChain 20 "child"
    let throughFragment = planIsolated executor $"{{ node {{%s{chain} ... on Node {{%s{chain} }} }} }}"
    let twice = planIsolated executor $"{{ node {{%s{chain}%s{chain} }} }}"
    throughFragment.Fields
    |> List.map _.Identifier
    |> equals [ "node" ]
    twice.Fields |> List.map _.Identifier |> equals [ "node" ]

[<Fact>]
let ``Executing fragments deferred conditionally on a variable completes quickly when the variable disables them`` () =
    // A disabled fragment is merged into the selection of its object when the object is executed, so merging two
    // fragments that select the same nested fields must not be exponential either
    let chain = nestedChain 20 "child"
    let plan =
        planIsolated executor $"query Q($f: Boolean!) {{ node {{ ... @defer(if: $f) {{%s{chain} }} ... @defer(if: $f) {{%s{chain} }} }} }}"
    let variables = noVariables.Add ("f", JsonDocument.Parse("false").RootElement)
    let result = executeIsolated executor plan variables
    let expected =
        NameValueLookup.ofList [
            "node",
            upcast
                NameValueLookup.ofList [
                    "child", upcast NameValueLookup.ofList [ "child", upcast NameValueLookup.ofList [ "child", null ] ]
                ]
        ]
    match result with
    | Direct (ValueSome data, errors)
    | Deferred (data, errors, _) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail $"Expected the data of the node, but got %A{result.Content}"

[<Fact>]
let ``Planning a field selected many times with different selections completes quickly`` () =
    // Merging the selections of the field one after another copies its growing selection set every time
    let underInterface =
        [ for i in 0..19_999 -> $"child {{ x%i{i}: id }}" ]
        |> String.concat " "
    let underObject =
        [ for i in 0..19_999 -> $"... {{ child {{ x%i{i}: id }} }}" ]
        |> String.concat " "
    let schema = nodeSchema ()
    let planUnderInterface = planWithoutValidation schema $"{{ node {{ %s{underInterface} }} }}"
    let planUnderObject = planWithoutValidation schema $"{{ node {{ ... on A {{ %s{underObject} }} }} }}"
    planUnderInterface.Fields
    |> List.map _.Identifier
    |> equals [ "node" ]
    planUnderObject.Fields
    |> List.map _.Identifier
    |> equals [ "node" ]

[<Fact>]
let ``Planning many aliased fields completes quickly`` () =
    // Searching the fields planned so far for every field is quadratic in the number of fields
    let fields =
        [ for i in 0..99_999 -> $"a%i{i}: hello" ]
        |> String.concat " "
    let plan = planWithoutValidation (nodeSchema ()) $"{{ %s{fields} }}"
    plan.Fields |> List.length |> equals 100_000

[<Fact>]
let ``Planning many deferred fragments completes quickly`` () =
    // Appending every deferred fragment to the fields planned so far is quadratic in the number of fragments
    let fragments =
        [ for i in 0..49_999 -> $"... @defer {{ d%i{i}: hello }}" ]
        |> String.concat " "
    let plan = planWithoutValidation (nodeSchema ()) $"{{ hello %s{fragments} }}"
    plan.Fields |> List.length |> equals 50_001

[<Fact>]
let ``Executing many fields completes quickly`` () =
    // Collecting the fields of an object one after another is quadratic in the number of fields
    let fields =
        [ for i in 0..99_999 -> $"a%i{i}: hello" ]
        |> String.concat " "
    let plan = planWithoutValidation (nodeSchema ()) $"{{ %s{fields} }}"
    let result = executeIsolated executor plan noVariables
    ensureDirect result
    <| fun data errors ->
        empty errors
        data.Count |> equals 100_000

[<Fact>]
let ``Query weight middleware measures fields nested under an interface quickly`` () =
    // The middleware walked the plan as a tree, visiting the plans shared by the implementations once per path to them
    let executor = Executor (nodeSchema (), [ Define.QueryWeightMiddleware (1000.0, true) ])
    let plan = planIsolated executor (nestedUnderNode 25 "child")
    let result = executeIsolated executor plan noVariables
    ensureDirect result <| fun _ errors -> empty errors
    result.Metadata.TryFind<float> "queryWeight"
    |> equals (ValueSome 0.0)

[<Fact>]
let ``Object list filter middleware collects the filters under an interface quickly`` () =
    // The middleware walked the plan as a tree, visiting the plans shared by the implementations once per path to them
    let executor = Executor (nodeSchema (), [ Define.ObjectListFilterMiddleware<Node, Node>(true) ])
    let chain = nestedChain 25 "child"
    let plan =
        planIsolated executor $"{{ node {{%s{chain} ... on A {{ items(filter: {{ id: 1 }}) {{ id }} }} }} }}"
    let result = executeIsolated executor plan noVariables
    ensureDirect result <| fun _ errors -> empty errors
    result.Metadata.TryFind<ObjectListFilters> "filters"
    |> wantValueSome
    |> Seq.map (fun (KeyValue (path, _)) -> path |> List.map string)
    |> List.ofSeq
    |> equals [ [ "node"; "items" ] ]

[<Fact>]
let ``Object list filter middleware collects the filters under covariant implementations quickly`` () =
    // The implementations return different types, so the plans under them differ while their paths are the same:
    // walking every plan under every path to it multiplies the walks by about 5 at every level
    let executor =
        Executor (covariantSchema { root with Type = "A0"; Child = ValueNone }, [ Define.ObjectListFilterMiddleware<Node, Node>(true) ])
    let chain = nestedAround 20 "child" " ... on B0 { items(filter: { id: 1 }) { id } }"
    let plan = planIsolated executor $"{{ node {{%s{chain} }} }}"
    let result = executeIsolated executor plan noVariables
    ensureDirect result <| fun _ errors -> empty errors
    result.Metadata.TryFind<ObjectListFilters> "filters"
    |> wantValueSome
    |> Seq.map (fun (KeyValue (path, _)) -> path |> List.map string)
    |> List.ofSeq
    |> equals [ [ yield "node"; yield! List.replicate 20 "child"; yield "items" ] ]

[<Fact>]
let ``Object list filter middleware collects many filters nested deeply quickly`` () =
    // Hashing the whole path of every filter at every level takes time quadratic in the nesting. The document has more
    // selections than validation allows, so it is planned without validation
    let schema = nodeSchema ()
    let executor = Executor (schema, [ Define.ObjectListFilterMiddleware<Node, Node>(true) ])
    let lists =
        [ for i in 0..23_999 -> $"l%i{i}: items(filter: {{ id: 1 }}) {{ id }}" ]
        |> String.concat " "
    let chain = nestedAround 120 "child" $" ... on A {{ %s{lists} }}"
    let plan = planWithoutValidation schema $"{{ node {{%s{chain} }} }}"
    let result = executeIsolated executor plan noVariables
    ensureDirect result <| fun _ errors -> empty errors
    let filters = result.Metadata.TryFind<ObjectListFilters> "filters" |> wantValueSome
    filters.Count |> equals 24_000

[<Fact>]
let ``Object list filter middleware reports one filter for a list field filtered differently under different types`` () =
    // The filters are reported by path, and the field has the same path under both types
    let executor = Executor (nodeSchema (), [ Define.ObjectListFilterMiddleware<Node, Node>(true) ])
    let plan =
        planIsolated executor "{ node { ... on A { items(filter: { id: 1 }) { id } } ... on B { items(filter: { id: 2 }) { id } } } }"
    let result = executeIsolated executor plan noVariables
    ensureDirect result <| fun _ errors -> empty errors
    result.Metadata.TryFind<ObjectListFilters> "filters"
    |> wantValueSome
    |> Seq.map (fun (KeyValue (path, _)) -> path |> List.map string)
    |> List.ofSeq
    |> equals [ [ "node"; "items" ] ]

[<Fact>]
let ``Object list filter middleware accepts a deferred fragment at the root`` () =
    // The entry of a deferred fragment at the root has no field of the document, whose name the middleware read
    let executor = Executor (nodeSchema (), [ Define.ObjectListFilterMiddleware<Node, Node>(true) ])
    let plan = planIsolated executor "{ ... @defer { hello } }"
    let result = executeIsolated executor plan noVariables
    ensureDeferred result <| fun _ errors _ -> empty errors
    result.Metadata.TryFind<ObjectListFilters> "filters"
    |> wantValueSome
    |> empty

[<Fact>]
let ``Executing fragments deferred conditionally on a variable in the objects of a list completes quickly when the variable disables them`` () =
    // A disabled fragment is merged into the selection of every object executed, so merging the fields of the
    // fragments again for each object of a list multiplies the time by the number of objects
    let items = [ for i in 1..1_000 -> { Type = "B"; Id = string i; Child = ValueNone; Items = [] } ]
    let executor = Executor (nodeSchemaWith { root with Items = items })
    let aliases = String.Join (" ", Seq.init 2_000 (fun i -> $"a%i{i}: id"))
    let fragments = "... @defer(if: $f) { child { ...Big } } ... @defer(if: $f) { child { ...Big } }"
    let plan =
        planIsolated
            executor
            $"query Q($f: Boolean!) {{ node {{ ... on A {{ items {{ %s{fragments} }} }} }} }} fragment Big on Node {{ %s{aliases} }}"
    let variables = noVariables.Add ("f", JsonDocument.Parse("false").RootElement)
    let result = executeIsolated executor plan variables
    ensureDirect result
    <| fun data errors ->
        empty errors
        let node = data["node"] :?> Output
        node["items"] :?> obj seq
        |> Seq.map (fun item -> (item :?> Output)["child"])
        |> List.ofSeq
        |> equals (List.replicate 1_000 null)

[<Fact>]
let ``Reused plans keep the deferred fragments of the field reusing them`` () =
    // A deferred fragment entry copies the field holding it, so a plan reused for another implementation must not
    // keep the field of the implementation planned first: the query weight counts the field of every entry
    let xType = Define.Object<obj>("X", [ Define.Field ("id", StringType, (fun _ _ -> "x")) ])
    let holderType =
        Define.Interface<obj>("Holder", [ Define.Field ("id", StringType); Define.Field ("child", xType) ])
    let holder (name : string) (weight : float) =
        Define.Object<obj>(
            name,
            [
                Define.Field ("id", StringType, (fun _ _ -> name))
                Define.Field("child", xType, (fun _ value -> value)).WithQueryWeight weight
            ],
            interfaces = [ holderType ],
            isTypeOf = (fun _ -> String.Equals (name, "B", StringComparison.Ordinal))
        )
    let queryType =
        Define.Object<obj>("Query", [ Define.Field ("node", holderType, (fun _ value -> value)) ])
    let schema =
        Schema (queryType, config = { SchemaConfig.Default with Types = [ holder "A" 1.0; holder "B" 5.0 ] })
    let executor = Executor (schema, [ Define.QueryWeightMiddleware (10.0, true) ])
    let result =
        executor.AsyncExecute ("{ node { child { id ... @defer { id2: id } } } }", getMockInputContext)
        |> sync
    // The weights of A's child and its deferred fragment, 1 + 1, then of B's child and its deferred fragment, 5 + 5,
    // so the weight exceeds the threshold at 12
    ensureRequestError result
    <| fun errors ->
        errors
        |> List.map _.Message
        |> equals [ "Query complexity exceeds maximum threshold. Please reduce query complexity and try again." ]
    result.Metadata.TryFind<float> "queryWeight"
    |> equals (ValueSome 12.0)
