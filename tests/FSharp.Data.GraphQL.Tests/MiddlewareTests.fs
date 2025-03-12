module FSharp.Data.GraphQL.Tests.MiddlewareTests

open System
open System.Collections.Generic
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Middleware
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Ast
open System.Collections.Immutable
open System.Text.Json

#nowarn "40"

type Root = { clientId : int }

and Subject =
    | A of A
    | B of B

and A = { id : int; value : string; subjects : int list }

and B = { id : int; value : string; subjects : int list }

let executor =
    let a1 : A = { id = 1; value = "A1"; subjects = [ 2; 6 ] }
    let a2 : A = { id = 2; value = "A2"; subjects = [ 1; 3; 5 ] }
    let a3 : A = { id = 3; value = "A3"; subjects = [ 1; 2; 4 ] }
    let b1 = { id = 4; value = "1000"; subjects = [ 1; 5 ] }
    let b2 = { id = 5; value = "2000"; subjects = [ 3; 4; 6 ] }
    let b3 = { id = 6; value = "3000"; subjects = [ 1; 3; 5 ] }
    let al = [ a1; a2; a3 ]
    let bl = [ b1; b2; b3 ]
    let getA id = al |> List.tryFind (fun a -> a.id = id)
    let getB id = bl |> List.tryFind (fun b -> b.id = id)
    let subjects = (al |> List.map A) @ (bl |> List.map B)
    let getSubject id =
        let matchesId id =
            function
            | A a -> a.id = id
            | B b -> b.id = id
        subjects |> List.tryFind (matchesId id)
    let rec SubjectType =
        Define.Union (
            name = "Subject",
            options = [ AType; BType ],
            resolveValue =
                (fun u ->
                    match u with
                    | A a -> box a
                    | B b -> box b),
            resolveType =
                (fun u ->
                    match u with
                    | A _ -> upcast AType
                    | B _ -> upcast BType)
        )
    and AType =
        DefineRec.Object<A> (
            name = "A",
            isTypeOf = (fun o -> o :? A),
            fieldsFn =
                fun () -> [
                    Define.Field ("id", IntType, resolve = (fun _ a -> a.id))
                    Define.Field ("value", StringType, resolve = (fun _ a -> a.value))
                    Define
                        .Field(
                            "subjects",
                            Nullable (ListOf (Nullable SubjectType)),
                            resolve = fun _ (a : A) -> a.subjects |> List.map getSubject |> List.toSeq |> Some
                        )
                        .WithQueryWeight (1.0)
                ]
        )
    and BType =
        DefineRec.Object<B> (
            name = "B",
            isTypeOf = (fun o -> o :? B),
            fieldsFn =
                fun () -> [
                    Define.Field ("id", IntType, resolve = (fun _ b -> b.id))
                    Define.Field ("value", StringType, resolve = (fun _ b -> b.value))
                    Define
                        .Field(
                            "subjects",
                            Nullable (ListOf (Nullable SubjectType)),
                            resolve = fun _ (b : B) -> b.subjects |> List.map getSubject |> List.toSeq |> Some
                        )
                        .WithQueryWeight (1.0)
                ]
        )
    let Query =
        Define.Object<Root> (
            name = "Query",
            fields = [
                Define.Field ("A", Nullable AType, "A Field", [ Define.Input ("id", IntType) ], resolve = (fun ctx _ -> getA (ctx.Arg ("id"))))
                Define.Field ("B", Nullable BType, "B Field", [ Define.Input ("id", IntType) ], resolve = (fun ctx _ -> getB (ctx.Arg ("id"))))
            ]
        )
    let schema = Schema (Query)
    let middleware = [
        Define.QueryWeightMiddleware (2.0, true)
        Define.ObjectListFilterMiddleware<A, Subject option> (true)
        Define.ObjectListFilterMiddleware<B, Subject option> (true)
    ]
    Executor (schema, middleware)

let execute (query : Document) = executor.AsyncExecute (query) |> sync

let executeWithVariables (query : Document, variables : ImmutableDictionary<string, JsonElement>) =
    executor.AsyncExecute (ast = query, variables = variables) |> sync

let expectedErrors : GQLProblemDetails list = [
    GQLProblemDetails.Create ("Query complexity exceeds maximum threshold. Please reduce query complexity and try again.")
]

[<Fact>]
let ``Simple query: Must pass when below threshold`` () =
    let query =
        parse
            """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects { ...Value }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }"""
    let expected =
        NameValueLookup.ofList [
            "A",
            upcast
                NameValueLookup.ofList [
                    "id", upcast 1
                    "value", upcast "A1"
                    "subjects",
                    upcast
                        [
                            NameValueLookup.ofList [ "id", upcast 2; "value", upcast "A2" ]
                            NameValueLookup.ofList [ "id", upcast 6; "value", upcast "3000" ]
                        ]
                ]
        ]
    let result = execute query
    match result with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQLResponse"
    result.Metadata.TryFind<float> ("queryWeightThreshold") |> equals (ValueSome 2.0)
    result.Metadata.TryFind<float> ("queryWeight") |> equals (ValueSome 1.0)

[<Fact>]
let ``Simple query: Must not pass when above threshold`` () =
    let query =
        parse
            """query testQuery {
                    A (id : 1) {
                        id
                        value
                        subjects { ...All }
                    }
        }

        fragment Value on Subject {
                    ...on A {
                        id
                        value
                    }
                    ...on B {
                        id
                        value
                    }
        }

        fragment Inner on Subject {
                    ...on A {
                        id
                        value
                        subjects { ...Value }
                    }
                    ...on B {
                        id
                        value
                        subjects { ...Value }
                    }
        }

        fragment AllA on A {
                    id
                    value
                    subjects { ...Inner }
        }

        fragment AllB on B {
                    id
                    value
                    subjects { ...Inner }
        }

        fragment All on Subject {
                    ...on A { ...AllA }
                    ...on B { ...AllB }
        }"""
    let result = execute query
    result |> ensureRequestError <| fun errors -> errors |> equals expectedErrors
    result.Metadata.TryFind<float> ("queryWeightThreshold") |> equals (ValueSome 2.0)
    result.Metadata.TryFind<float> ("queryWeight") |> equals (ValueSome 3.0)

[<Fact>]
let ``Deferred queries : Must pass when below threshold`` () =
    let query =
        parse
            """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects @defer { ...Value }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }"""
    let expected =
        NameValueLookup.ofList [
            "A", upcast NameValueLookup.ofList [ "id", upcast 1; "value", upcast "A1"; "subjects", upcast null ]
        ]
    let expectedDeferred =
        DeferredResult (
            [|
                NameValueLookup.ofList [ "id", upcast 2; "value", upcast "A2" ]
                NameValueLookup.ofList [ "id", upcast 6; "value", upcast "3000" ]
            |],
            [ "A"; "subjects" ]
        )
    let result = execute query
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expected)
        use sub = Observer.create deferred
        sub.WaitCompleted ()
        sub.Received |> single |> equals expectedDeferred
    result.Metadata.TryFind<float> ("queryWeightThreshold") |> equals (ValueSome 2.0)
    result.Metadata.TryFind<float> ("queryWeight") |> equals (ValueSome 2.0)

[<Fact>]
let ``Streamed queries : Must pass when below threshold`` () =
    let query =
        parse
            """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects @stream { ...Value }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }"""
    let expected =
        NameValueLookup.ofList [
            "A", upcast NameValueLookup.ofList [ "id", upcast 1; "value", upcast "A1"; "subjects", upcast [] ]
        ]
    let expectedDeferred1 =
        DeferredResult ([| NameValueLookup.ofList [ "id", upcast 2; "value", upcast "A2" ] |], [ "A"; "subjects"; 0 ])
    let expectedDeferred2 =
        DeferredResult ([| NameValueLookup.ofList [ "id", upcast 6; "value", upcast "3000" ] |], [ "A"; "subjects"; 1 ])
    let result = execute query
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expected)
        use sub = Observer.create deferred
        sub.WaitCompleted (2)
        sub.Received
        |> Seq.cast<GQLDeferredResponseContent>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> ignore
    result.Metadata.TryFind<float> ("queryWeightThreshold") |> equals (ValueSome 2.0)
    result.Metadata.TryFind<float> ("queryWeight") |> equals (ValueSome 2.0)

[<Fact>]
let ``Deferred and Streamed queries : Must not pass when above threshold`` () =
    let query =
        sprintf
            """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects @%s { ...All }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }

        fragment Inner on Subject {
                ...on A {
                    id
                    value
                    subjects { ...Value }
                }
                ...on B {
                    id
                    value
                    subjects { ...Value }
                }
        }

        fragment AllA on A {
                id
                value
                subjects { ...Inner }
        }

        fragment AllB on B {
                id
                value
                subjects { ...Inner }
        }

        fragment All on Subject {
                ...on A { ...AllA }
                ...on B { ...AllB }
        }"""
    asts query
    |> Seq.map execute
    |> Seq.iter (fun result ->
        ensureRequestError result <| fun errors -> errors |> equals expectedErrors
        result.Metadata.TryFind<float> ("queryWeightThreshold") |> equals (ValueSome 2.0)
        result.Metadata.TryFind<float> ("queryWeight") |> equals (ValueSome 3.0))

[<Fact>]
let ``Inline fragment query : Must pass when below threshold`` () =
    let query =
        parse
            """query testQuery {
            A (id : 1) {
                id
                value
                subjects {
                    ... on A {
                        id
                        value
                    }
                    ... on B {
                        id
                    }
                }
            }
        }"""
    let expected =
        NameValueLookup.ofList [
            "A",
            upcast
                NameValueLookup.ofList [
                    "id", upcast 1
                    "value", upcast "A1"
                    "subjects",
                    upcast
                        [
                            NameValueLookup.ofList [ "id", upcast 2; "value", upcast "A2" ]
                            NameValueLookup.ofList [ "id", upcast 6 ]
                        ]
                ]
        ]
    let result = execute query
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
    result.Metadata.TryFind<float> ("queryWeightThreshold") |> equals (ValueSome 2.0)
    result.Metadata.TryFind<float> ("queryWeight") |> equals (ValueSome 1.0)

[<Fact>]
let ``Inline fragment query : Must not pass when above threshold`` () =
    let query =
        parse
            """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects {
                        ... on A {
                            id
                            value
                        }
                        ... on B {
                            id
                            subjects { ...Inner }
                        }
                    }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }

        fragment Inner on Subject {
                ...on A {
                    id
                    value
                    subjects { ... Value }
                }
                ... on B {
                    id
                    value
                    subjects { ...Value }
                }
        }"""
    let result = execute query
    ensureRequestError result <| fun errors -> errors |> equals expectedErrors
    result.Metadata.TryFind<float> ("queryWeightThreshold") |> equals (ValueSome 2.0)
    result.Metadata.TryFind<float> ("queryWeight") |> equals (ValueSome 3.0)

[<Fact>]
let ``Object list filter: must return filter information in Metadata`` () =
    let query =
        parse
            """query testQuery {
                A (id : 1) {
                    id
                    value
                    s : subjects (filter : { value_starts_with: "A", id : 2 }) { ...Value }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }"""
    let expected =
        NameValueLookup.ofList [
            "A",
            upcast
                NameValueLookup.ofList [
                    "id", upcast 1
                    "value", upcast "A1"
                    "s",
                    upcast
                        [
                            NameValueLookup.ofList [ "id", upcast 2; "value", upcast "A2" ]
                            NameValueLookup.ofList [ "id", upcast 6; "value", upcast "3000" ]
                        ]
                ]
        ]
    let expectedFilter : KeyValuePair<obj list, ObjectListFilter> =
        KeyValuePair ([ "A"; "s" ], And (Equals { FieldName = "id"; Value = 2L }, StartsWith { FieldName = "value"; Value = "A" }))
    let result = execute query
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
    result.Metadata.TryFind<float> ("queryWeightThreshold") |> equals (ValueSome 2.0)
    result.Metadata.TryFind<float> ("queryWeight") |> equals (ValueSome 1.0)
    result.Metadata.TryFind<ObjectListFilters> ("filters")
    |> wantValueSome
    |> seqEquals [ expectedFilter ]

[<Fact>]
let ``Object list filter: Must return AND filter information in Metadata`` () =
    let query =
        parse
            """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects (filter : { and : [{ value_starts_with: "3"}, {id : 6 }]}) { ...Value }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }"""
    let expected =
        NameValueLookup.ofList [
            "A",
            upcast
                NameValueLookup.ofList [
                    "id", upcast 1
                    "value", upcast "A1"
                    "subjects",
                    upcast
                        [
                            NameValueLookup.ofList [ "id", upcast 2; "value", upcast "A2" ]
                            NameValueLookup.ofList [ "id", upcast 6; "value", upcast "3000" ]
                        ]
                ]
        ]
    let expectedFilter : KeyValuePair<obj list, ObjectListFilter> =
        KeyValuePair ([ "A"; "subjects" ], And (StartsWith { FieldName = "value"; Value = "3" }, Equals { FieldName = "id"; Value = 6L }))
    let result = execute query
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
    result.Metadata.TryFind<ObjectListFilters> ("filters")
    |> wantValueSome
    |> seqEquals [ expectedFilter ]

[<Fact>]
let ``Object list filter: Must return OR filter information in Metadata`` () =
    let query =
        parse
            """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects (filter : { or : [{value_starts_with: "3"}, {id : 6}] }) { ...Value }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }"""
    let expected =
        NameValueLookup.ofList [
            "A",
            upcast
                NameValueLookup.ofList [
                    "id", upcast 1
                    "value", upcast "A1"
                    "subjects",
                    upcast
                        [
                            NameValueLookup.ofList [ "id", upcast 2; "value", upcast "A2" ]
                            NameValueLookup.ofList [ "id", upcast 6; "value", upcast "3000" ]
                        ]
                ]
        ]
    let expectedFilter : KeyValuePair<obj list, ObjectListFilter> =
        KeyValuePair ([ "A"; "subjects" ], Or (StartsWith { FieldName = "value"; Value = "3" }, Equals { FieldName = "id"; Value = 6L }))
    let result = execute query
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
    result.Metadata.TryFind<ObjectListFilters> ("filters")
    |> wantValueSome
    |> seqEquals [ expectedFilter ]

[<Fact>]
let ``Object list filter: Must return NOT filter information in Metadata`` () =
    let query =
        parse
            """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects (filter : { not : {value_starts_with: "3"} }) { ...Value }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }"""
    let expected =
        NameValueLookup.ofList [
            "A",
            upcast
                NameValueLookup.ofList [
                    "id", upcast 1
                    "value", upcast "A1"
                    "subjects",
                    upcast
                        [
                            NameValueLookup.ofList [ "id", upcast 2; "value", upcast "A2" ]
                            NameValueLookup.ofList [ "id", upcast 6; "value", upcast "3000" ]
                        ]
                ]
        ]
    let expectedFilter : KeyValuePair<obj list, ObjectListFilter> =
        KeyValuePair ([ "A"; "subjects" ], Not (StartsWith { FieldName = "value"; Value = "3" }))
    let result = execute query
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
    result.Metadata.TryFind<ObjectListFilters> ("filters")
    |> wantValueSome
    |> seqEquals [ expectedFilter ]

[<Fact>]
let ``Object list filter: Must return filter information in Metadata when supplied as variable`` () =
    let jsonString = """{ "not": { "value_starts_with": "3" } }"""
    let jsonElement = JsonDocument.Parse(jsonString).RootElement

    let dict = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", jsonElement)
    let query =
        parse
            """query testQuery($filter: ObjectListFilter!) {
                A (id : 1) {
                    id
                    value
                    subjects (filter : $filter) { ...Value }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }"""
    let expected =
        NameValueLookup.ofList [
            "A",
            upcast
                NameValueLookup.ofList [
                    "id", upcast 1
                    "value", upcast "A1"
                    "subjects",
                    upcast
                        [
                            NameValueLookup.ofList [ "id", upcast 2; "value", upcast "A2" ]
                            NameValueLookup.ofList [ "id", upcast 6; "value", upcast "3000" ]
                        ]
                ]
        ]
    let expectedFilter : KeyValuePair<obj list, ObjectListFilter> =
        KeyValuePair ([ "A"; "subjects" ], Not (StartsWith { FieldName = "value"; Value = "3" }))
    let result = executeWithVariables (query, dict)
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
    result.Metadata.TryFind<ObjectListFilters> ("filters")
    |> wantValueSome
    |> seqEquals [ expectedFilter ]
