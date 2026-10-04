module FSharp.Data.GraphQL.Tests.InputCollectionTests

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Immutable
open System.Collections.ObjectModel
open System.Text.Json
open Xunit

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Types

/// A collection that only a collection initializer builds
type Bag () =
    let items = ResizeArray<int>()
    member _.Add (item : int) = items.Add item
    interface IEnumerable<int> with
        member _.GetEnumerator () : IEnumerator<int> = items.GetEnumerator ()
    interface IEnumerable with
        member _.GetEnumerator () : IEnumerator = items.GetEnumerator ()

/// A collection whose Add returns a new collection instead of changing this one
type ImmutableBag (items : int list) =
    new () = ImmutableBag ([])
    member _.Add (item : int) = ImmutableBag (items @ [ item ])
    interface IEnumerable<int> with
        member _.GetEnumerator () : IEnumerator<int> = (items :> int seq).GetEnumerator()
    interface IEnumerable with
        member _.GetEnumerator () : IEnumerator = (items :> IEnumerable).GetEnumerator()

type Collections = {
    ResizeArray : ResizeArray<int>
    Array : int[]
    Set : Set<int>
    Immutable : ImmutableArray<int>
    Optional : ResizeArray<int> option
}

/// Describes a collection as the name of its type and its items
let private describe (collection : IEnumerable) =
    let items = collection |> Seq.cast<obj> |> Seq.map string
    $"""%s{collection.GetType().Name}: %s{String.Join (", ", items)}"""

let private collectionField<'Collection when 'Collection :> int seq> (name : string) (listType : ListOfDef<int, 'Collection>) =
    Define.Field (name, StringType, "", [ Define.Input ("values", listType) ], fun ctx _ -> describe (ctx.Arg<'Collection> "values"))

let private collectionsType =
    Define.InputObject<Collections>(
        "Collections",
        [
            Define.Input ("resizeArray", (ListOf IntType : ListOfDef<int, int list>))
            Define.Input ("array", (ListOf IntType : ListOfDef<int, int list>))
            Define.Input ("set", (ListOf IntType : ListOfDef<int, int list>))
            Define.Input ("immutable", (ListOf IntType : ListOfDef<int, int list>))
            Define.Input ("optional", Nullable (ListOf IntType : ListOfDef<int, int list>))
        ]
    )

let private schema =
    Schema (
        Define.Object<unit>(
            "Query",
            [
                collectionField "array" (ListOf IntType : ListOfDef<int, int[]>)
                collectionField "resizeArray" (ListOf IntType : ListOfDef<int, ResizeArray<int>>)
                collectionField "set" (ListOf IntType : ListOfDef<int, Set<int>>)
                collectionField "hashSet" (ListOf IntType : ListOfDef<int, HashSet<int>>)
                collectionField "immutableArray" (ListOf IntType : ListOfDef<int, ImmutableArray<int>>)
                collectionField "readOnlyList" (ListOf IntType : ListOfDef<int, IReadOnlyList<int>>)
                collectionField "list" (ListOf IntType : ListOfDef<int, IList<int>>)
                collectionField "iSet" (ListOf IntType : ListOfDef<int, ISet<int>>)
                collectionField "bag" (ListOf IntType : ListOfDef<int, Bag>)
                Define.Field (
                    "collections",
                    StringType,
                    "",
                    [ Define.Input ("input", collectionsType) ],
                    fun ctx _ ->
                        let input = ctx.Arg<Collections> "input"
                        let optional =
                            input.Optional
                            |> Option.map describe
                            |> Option.defaultValue "None"
                        String.Join (
                            " | ",
                            [
                                describe input.ResizeArray
                                describe input.Array
                                describe input.Set
                                describe input.Immutable
                                optional
                            ]
                        )
                )
            ]
        )
    )

let private execute (query : string) (variables : string) =
    let variables =
        JsonDocument.Parse(variables).RootElement.Deserialize<ImmutableDictionary<string, JsonElement>>(serializerOptions)
    sync
    <| Executor(schema).AsyncExecute(parse query, getMockInputContext, variables = variables)

let private expectedCollections =
    NameValueLookup.ofList [
        "array", upcast "Int32[]: 3, 1, 2"
        "resizeArray", upcast "List`1: 3, 1, 2"
        "set", upcast "FSharpSet`1: 1, 2, 3"
        "hashSet", upcast "HashSet`1: 3, 1, 2"
        "immutableArray", upcast "ImmutableArray`1: 3, 1, 2"
        "readOnlyList", upcast "FSharpList`1: 3, 1, 2"
        "list", upcast "List`1: 3, 1, 2"
        "iSet", upcast "HashSet`1: 3, 1, 2"
        "bag", upcast "Bag: 3, 1, 2"
    ]

[<Fact>]
let ``Lists must be built as the collection types of their definitions from literals`` () =
    let query =
        """{
          array(values: [3, 1, 2])
          resizeArray(values: [3, 1, 2])
          set(values: [3, 1, 2])
          hashSet(values: [3, 1, 2])
          immutableArray(values: [3, 1, 2])
          readOnlyList(values: [3, 1, 2])
          list(values: [3, 1, 2])
          iSet(values: [3, 1, 2])
          bag(values: [3, 1, 2])
        }"""
    ensureDirect (execute query "{}")
    <| fun data errors ->
        empty errors
        data |> equals (upcast expectedCollections)

[<Fact>]
let ``Lists must be built as the collection types of their definitions from variables`` () =
    let query =
        """query Collections($values: [Int!]!) {
          array(values: $values)
          resizeArray(values: $values)
          set(values: $values)
          hashSet(values: $values)
          immutableArray(values: $values)
          readOnlyList(values: $values)
          list(values: $values)
          iSet(values: $values)
          bag(values: $values)
        }"""
    ensureDirect (execute query """{ "values": [3, 1, 2] }""")
    <| fun data errors ->
        empty errors
        data |> equals (upcast expectedCollections)

[<Fact>]
let ``Single value must be built as a collection of one item`` () =
    let query =
        "query Collections($values: [Int!]!) { resizeArray(values: $values) immutableArray(values: $values) }"
    ensureDirect (execute query """{ "values": 3 }""")
    <| fun data errors ->
        empty errors
        data
        |> equals (upcast NameValueLookup.ofList [ "resizeArray", upcast "List`1: 3"; "immutableArray", upcast "ImmutableArray`1: 3" ])

let private expectedInputObject =
    NameValueLookup.ofList [
        "collections", upcast "List`1: 3, 1 | Int32[]: 3, 1 | FSharpSet`1: 1, 3 | ImmutableArray`1: 3, 1 | List`1: 2"
    ]

[<Fact>]
let ``Input object fields must be copied into the collection types of their parameters from literals`` () =
    let query =
        "{ collections(input: { resizeArray: [3, 1], array: [3, 1], set: [3, 1], immutable: [3, 1], optional: [2] }) }"
    ensureDirect (execute query "{}")
    <| fun data errors ->
        empty errors
        data |> equals (upcast expectedInputObject)

[<Fact>]
let ``Input object fields must be copied into the collection types of their parameters from variables`` () =
    let query = "query Collections($input: Collections!) { collections(input: $input) }"
    let variables =
        """{ "input": { "resizeArray": [3, 1], "array": [3, 1], "set": [3, 1], "immutable": [3, 1], "optional": [2] } }"""
    ensureDirect (execute query variables)
    <| fun data errors ->
        empty errors
        data |> equals (upcast expectedInputObject)

[<Fact>]
let ``Collection factory must refuse types it cannot build`` () =
    Assert.True ((ReflectionHelper.tryCreateCollectionFactory typeof<string>).IsNone, "a string is not a collection")
    Assert.True (
        (ReflectionHelper.tryCreateCollectionFactory typeof<ReadOnlyCollection<int>>).IsNone,
        "ReadOnlyCollection has neither a constructor taking IEnumerable nor a collection initializer"
    )
    Assert.True (
        (ReflectionHelper.tryCreateCollectionFactory typeof<ImmutableBag>).IsNone,
        "an Add that returns a new collection does not fill the one it is called on"
    )
