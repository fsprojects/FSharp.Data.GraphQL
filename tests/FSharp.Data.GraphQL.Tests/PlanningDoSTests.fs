module FSharp.Data.GraphQL.Tests.PlanningDoSTests

open System
open System.Text
open Xunit

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

/// A value of the test schemas, resolved as the object type named by its Type
type Node = { Type : string; Id : string; Child : Node option }

#nowarn "40"

/// The interface Node, whose field child returns the interface itself
let private nodeInterface () : InterfaceDef<Node> =
    let rec nodeType : InterfaceDef<Node> =
        DefineRec.Interface<Node>("Node", (fun () -> [ Define.Field ("id", StringType); Define.Field ("child", Nullable nodeType) ]))
    nodeType

#warnon "40"

/// An object type implementing the interface, whose field child returns the given type
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
            Define.Field ("child", childType, (fun _ node -> node.Child))
        ]
    )

let private executorOf (nodeType : InterfaceDef<Node>) (implementations : ObjectDef<Node> list) (root : Node) =
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
    Executor (Schema (queryType, config = config))

let private root = {
    Type = "A"
    Id = "1"
    Child = Some { Type = "B"; Id = "2"; Child = Some { Type = "C"; Id = "3"; Child = None } }
}

/// Node implemented by A, B and C, whose field child returns Node
let private executor =
    let nodeType = nodeInterface ()
    [ "A"; "B"; "C" ]
    |> List.map (implementation nodeType (Nullable nodeType))
    |> executorOf nodeType
    <| root

/// Node implemented by A0 to A3, whose field child returns B0 to B3, and by B0 to B3, whose field child returns Node
let private covariantExecutor =
    let nodeType = nodeInterface ()
    let bTypes = [ for i in 0..3 -> implementation nodeType (Nullable nodeType) $"B%i{i}" ]
    let aTypes =
        bTypes
        |> List.mapi (fun i bType -> implementation nodeType (Nullable bType) $"A%i{i}")
    executorOf nodeType [ yield! aTypes; yield! bTypes ] root

/// Generous enough for slow CI machines, yet far below the hours that an exponential planning takes
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

/// The node field with the field nested to the depth under it
let private nestedUnderNode (depth : int) (field : string) =
    let document = StringBuilder ()
    document.Append "{ node {" |> ignore
    for _ in 1..depth do
        document.Append $" %s{field} {{" |> ignore
    document.Append " id" |> ignore
    for _ in 1..depth do
        document.Append " }" |> ignore
    document.Append " } }" |> ignore
    document.ToString ()

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
let ``Planning many aliased fields completes quickly`` () =
    // Searching the fields planned so far for every field is quadratic in the number of fields
    let fields =
        [ for i in 0..23_999 -> $"a%i{i}: hello" ]
        |> String.concat " "
    let plan = planIsolated executor $"{{ %s{fields} }}"
    plan.Fields |> List.length |> equals 24_000

[<Fact>]
let ``Planning many deferred fragments completes quickly`` () =
    // Appending every deferred fragment to the fields planned so far is quadratic in the number of fragments
    let fragments =
        [ for i in 0..11_999 -> $"... @defer {{ d%i{i}: hello }}" ]
        |> String.concat " "
    let plan = planIsolated executor $"{{ hello %s{fragments} }}"
    plan.Fields |> List.length |> equals 12_001
