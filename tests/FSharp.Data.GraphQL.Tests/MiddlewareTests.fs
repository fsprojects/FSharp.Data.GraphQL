module FSharp.Data.GraphQL.Tests.MiddlewareTests

open System
open System.Linq
open System.Collections.Generic
open System.Collections.Immutable
open System.Text.Json
open Xunit
open FSharp
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Middleware
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Ast

#nowarn "40"

type Root = { clientId : int }

type GuidId = ValueObjectId of Guid

let private parseGuidId (value : string) =
    match Guid.TryParse value with
    | true, guid -> Ok (ValueObjectId guid)
    | false, _ ->
        Error [
            { new IGQLError with
                member _.Message = $"Cannot coerce '{value}' to GuidID"
            }
        ]

let private guidIdToString (ValueObjectId guidId) = guidId.ToString "D"

let ValueObjectType =
    Define.WrappedScalar (
        name = "ValueObject",
        coerceInput =
            (function
            | InputParameterValue.Variable value when value.ValueKind = JsonValueKind.String -> parseGuidId (value.GetString ())
            | InputParameterValue.InlineConstant (StringValue value) -> parseGuidId value
            | _ ->
                Error [
                    { new IGQLError with
                        member _.Message = "ValueObject must be provided as string"
                    }
                ]),
        coerceOutput =
            (function
            | :? GuidId as guid -> guidIdToString guid |> Some
            | _ -> None)
    )

type Subject =
    | A of A
    | B of B

and A = {
    Id : int
    Value : string
    GuidValue : Guid
    ValueObject : GuidId
    Subjects : int list
}

and B = {
    Id : int
    Value : string
    GuidValue : Guid
    ValueObject : GuidId
    Subjects : int list
}

type Complex = {
    Id : int
    Name : string
    Discriminator : string
    Communities : int list
    Buildings : int list
}
and Building = { Id : int; Name : string; Discriminator : string }
and Community = { Id : int; Name : string; Discriminator : string }

type Property =
    | Complex of Complex
    | Building of Building
    | Community of Community

let getExecutor (expectedFilter : ObjectListFilter voption) =
    let a1 : A = { Id = 1; Value = "A1"; GuidValue = Guid.Parse "11111111-1111-1111-1111-111111111111"; ValueObject = ValueObjectId (Guid.Parse "11111111-1111-1111-1111-111111111111"); Subjects = [ 2; 6 ] }
    let a2 : A = { Id = 2; Value = "A2"; GuidValue = Guid.Parse "22222222-2222-2222-2222-222222222222"; ValueObject = ValueObjectId (Guid.Parse "22222222-2222-2222-2222-222222222222"); Subjects = [ 1; 3; 5 ] }
    let a3 : A = { Id = 3; Value = "A3"; GuidValue = Guid.Parse "33333333-3333-3333-3333-333333333333"; ValueObject = ValueObjectId (Guid.Parse "33333333-3333-3333-3333-333333333333"); Subjects = [ 1; 2; 4 ] }
    let b1 = { Id = 4; Value = "1000"; GuidValue = Guid.Parse "44444444-4444-4444-4444-444444444444"; ValueObject = ValueObjectId (Guid.Parse "44444444-4444-4444-4444-444444444444"); Subjects = [ 1; 5 ] }
    let b2 = { Id = 5; Value = "2000"; GuidValue = Guid.Parse "55555555-5555-5555-5555-555555555555"; ValueObject = ValueObjectId (Guid.Parse "55555555-5555-5555-5555-555555555555"); Subjects = [ 3; 4; 6 ] }
    let b3 = { Id = 6; Value = "3000"; GuidValue = Guid.Parse "66666666-6666-6666-6666-666666666666"; ValueObject = ValueObjectId (Guid.Parse "66666666-6666-6666-6666-666666666666"); Subjects = [ 1; 3; 5 ] }
    let al = [ a1; a2; a3 ]
    let bl = [ b1; b2; b3 ]
    let p1 = Complex{ Id = 1; Name = "Complex 1"; Discriminator = "Complex"; Communities = [ 5 ]; Buildings = [ 3 ] }
    let p2 = Complex{ Id = 2; Name = "Complex 2"; Discriminator = "Complex"; Communities = [ 6 ]; Buildings = [ 4 ] }
    let p3 = Building { Id = 3; Name = "Building 1"; Discriminator = "Building" }
    let p4 = Building { Id = 4; Name = "Building 2"; Discriminator = "Building" }
    let p5 = Community { Id = 5; Name = "Community 1"; Discriminator = "Community" }
    let p6 = Community { Id = 6; Name = "Community 2"; Discriminator = "Community" }
    let pl = [ p1; p2; p3; p4; p5; p6 ]
    let getA id = al |> List.tryFind (fun a -> a.Id = id)
    let getB id = bl |> List.tryFind (fun b -> b.Id = id)
    let subjects = (al |> List.map A) @ (bl |> List.map B)
    let getSubject id =
        let matchesId id =
            function
            | A a -> a.Id = id
            | B b -> b.Id = id
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
                    Define.Field ("id", IntType, resolve = (fun _ a -> a.Id))
                    Define.Field ("value", StringType, resolve = (fun _ a -> a.Value))
                    Define.Field ("guidValue", GuidType, resolve = (fun _ a -> a.GuidValue))
                    Define.Field ("valueObject", ValueObjectType, resolve = (fun _ a -> a.ValueObject))
                    Define
                        .Field(
                            "subjects",
                            Nullable (ListOf (Nullable SubjectType)),
                            resolve =
                                fun ctx (a : A) ->
                                    expectedFilter
                                    |> ValueOption.iter (fun _ -> equals expectedFilter ctx.Filter)
                                    a.Subjects |> List.map getSubject |> List.toSeq |> Some
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
                    Define.Field ("id", IntType, resolve = (fun _ b -> b.Id))
                    Define.Field ("value", StringType, resolve = (fun _ b -> b.Value))
                    Define.Field ("guidValue", GuidType, resolve = (fun _ b -> b.GuidValue))
                    Define.Field ("valueObject", ValueObjectType, resolve = (fun _ b -> b.ValueObject))
                    Define
                        .Field(
                            "subjects",
                            Nullable (ListOf (Nullable SubjectType)),
                            resolve =
                                fun ctx (b : B) ->
                                    expectedFilter
                                    |> ValueOption.iter (fun _ -> equals expectedFilter ctx.Filter)
                                    b.Subjects |> List.map getSubject |> List.toSeq |> Some
                        )
                        .WithQueryWeight (1.0)
                ]
        )
    and ComplexType =
        DefineRec.Object<Complex> (
            name = "Complex",
            isTypeOf = (fun o -> o :? Complex),
            fieldsFn =
                fun () -> [
                    Define.Field ("id", IntType, resolve = (fun _ c -> c.Id))
                    Define.Field ("name", StringType, resolve = (fun _ c -> c.Name))
                    Define.Field ("discriminator", StringType, resolve = (fun _ c -> c.Discriminator))
                    Define.Field ("communities", ListOf IntType, resolve = (fun _ c -> c.Communities))
                    Define.Field ("buildings", ListOf IntType, resolve = (fun _ c -> c.Buildings))
                ]
        )
    and BuildingType =
        Define.Object<Building> (
            name = "Building",
            isTypeOf = (fun o -> o :? Building),
            fields = [
                Define.Field ("id", IntType, resolve = (fun _ b -> b.Id))
                Define.Field ("name", StringType, resolve = (fun _ b -> b.Name))
                Define.Field ("discriminator", StringType, resolve = (fun _ b -> b.Discriminator))
            ]
        )
    and CommunityType =
        Define.Object<Community> (
            name = "Community",
            isTypeOf = (fun o -> o :? Community),
            fields = [
                Define.Field ("id", IntType, resolve = (fun _ c -> c.Id))
                Define.Field ("name", StringType, resolve = (fun _ c -> c.Name))
                Define.Field ("discriminator", StringType, resolve = (fun _ c -> c.Discriminator))
            ]
        )
    and PropertyType =
        Define.Union<_, _> (
            name = "Property",
            options = [ ComplexType; BuildingType; CommunityType ],
            resolveValue =
                (function
                | Complex c -> box c
                | Building b -> box b
                | Community c -> box c),
            resolveType =
                (function
                | Complex _ -> upcast ComplexType
                | Building _ -> upcast BuildingType
                | Community _ -> upcast CommunityType)
        )
    let Query =
        Define.Object<Root> (
            name = "Query",
            fields = [
                Define.Field ("A", Nullable AType, "A Field", [ Define.Input ("id", IntType) ], resolve = (fun ctx _ -> getA (ctx.Arg ("id"))))
                Define.Field ("B", Nullable BType, "B Field", [ Define.Input ("id", IntType) ], resolve = (fun ctx _ -> getB (ctx.Arg ("id"))))
                Define.Field (
                    "Properties",
                    ListOf PropertyType,
                    description = "Properties Field",
                    resolve =
                        (fun ctx _ ->
                            // The main task here is to check if the filter is empty
                            // when all union cases are specified or no union case is specified
                            Assert.True (ctx.Filter.IsNone)
                            Assert.True (ctx.ExecutionInfo.ResolveAbstractionFilter(ctx.Schema.TypeMap).IsNone)
                            pl)
                )
            ]
        )
    let schema = Schema (Query)
    let middleware = [
        Define.QueryWeightMiddleware (2.0, true)
        Define.ObjectListFilterMiddleware<A, Subject option> (true)
        Define.ObjectListFilterMiddleware<B, Subject option> (true)
    ]
    Executor (schema, middleware)

let executor = getExecutor (ValueNone)

let execute (query : Document) = executor.AsyncExecute (query, getMockInputContext) |> sync

let executeWithVariables (query : Document, variables : ImmutableDictionary<string, JsonElement>) =
    executor.AsyncExecute (ast = query, getInputContext = getMockInputContext, variables = variables)
    |> sync

let executeAndVerifyFilter (query : Document, variables : ImmutableDictionary<string, JsonElement>, filterToVerify : ObjectListFilter) =
    let ex = getExecutor (ValueSome filterToVerify)
    ex.AsyncExecute (ast = query, getInputContext = getMockInputContext, variables = variables) |> sync

let expectedThresholdErrors : GQLProblemDetails list = [
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

    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
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

    ensureRequestError result <| fun errors -> errors |> equals expectedThresholdErrors
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
        ensureRequestError result <| fun errors -> errors |> equals expectedThresholdErrors
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

    ensureRequestError result <| fun errors -> errors |> equals expectedThresholdErrors
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
    let expectedFilter : KeyValuePair<obj list, _> =
        kvp ([ "A"; "s" ]) (And (Equals ({ FieldName = "id"; Value = 2L }, null), StartsWith ({ FieldName = "value"; Value = "A" }, StringComparer.CurrentCultureIgnoreCase)))
    let result = execute query

    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
    result.Metadata.TryFind<float> ("queryWeightThreshold") |> equals (ValueSome 2.0)
    result.Metadata.TryFind<float> ("queryWeight") |> equals (ValueSome 1.0)
    result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

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
    let expectedFilter : KeyValuePair<obj list, _> =
        kvp ([ "A"; "subjects" ]) (And (StartsWith ({ FieldName = "value"; Value = "3" }, StringComparer.CurrentCultureIgnoreCase), Equals ({ FieldName = "id"; Value = 6L }, null)))
    let result = execute query

    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
    result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

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
    let expectedFilter : KeyValuePair<obj list, _> =
        kvp ([ "A"; "subjects" ]) (Or (StartsWith ({ FieldName = "value"; Value = "3" }, StringComparer.CurrentCultureIgnoreCase), Equals ({ FieldName = "id"; Value = 6L }, null)))
    let result = execute query

    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
    result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

[<Fact>]
let ``Object list filter: Must return IN filter information in Metadata`` () =
    let query =
        parse
            """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects (filter : { value_in : ["3000", "A2"] }) { ...Value }
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
    let expectedFilter : KeyValuePair<obj list, _> =
        kvp ([ "A"; "subjects" ]) (In { FieldName = "value"; Value = [ "3000"; "A2" ] })
    let result = execute query

    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
    result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

[<Fact>]
let ``Object list filter: Must return Contains filter information in Metadata`` () =
    let query =
        parse
            """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects (filter : { value_contains : "3"}) { ...Value }
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
    let expectedFilter : KeyValuePair<obj list, _> =
        kvp ([ "A"; "subjects" ]) (Contains ({ FieldName = "value"; Value = "3" }, StringComparer.CurrentCultureIgnoreCase))
    let result = execute query

    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
    result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

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
    let expectedFilter : KeyValuePair<obj list, _> =
        kvp ([ "A"; "subjects" ]) (Not (StartsWith ({ FieldName = "value"; Value = "3" }, StringComparer.CurrentCultureIgnoreCase)))
    let result = execute query

    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
    result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

[<Fact>]
let ``Object list filter: Must return filter information in Metadata when supplied as variable and parse all filter operators`` () =
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
    do
        let notStartsFilter = """{ "not": { "value_starts_with": "3" } }""" |> JsonDocument.Parse |> _.RootElement
        let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", notStartsFilter)
        let filter = Not (StartsWith ({ FieldName = "value"; Value = "3" }, StringComparer.CurrentCultureIgnoreCase))
        let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) (filter)
        let result = executeAndVerifyFilter (query, variables, filter)

        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
        result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

    do
        let notEndsFilter = """{ "not": { "value_ends_with": "2" } }""" |> JsonDocument.Parse |> _.RootElement
        let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", notEndsFilter)
        let filter = Not (EndsWith ({ FieldName = "value"; Value = "2" }, StringComparer.CurrentCultureIgnoreCase))
        let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) (filter)
        let result = executeAndVerifyFilter (query, variables, filter)

        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
        result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

    do
        let notStartsFilter = """{ "not": { "value_sw": "3" } }""" |> JsonDocument.Parse |> _.RootElement
        let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", notStartsFilter)
        let filter = Not (StartsWith ({ FieldName = "value"; Value = "3" }, StringComparer.CurrentCultureIgnoreCase))
        let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) (filter)
        let result = executeAndVerifyFilter (query, variables, filter)

        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
        result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

    do
        let notEndsFilter = """{ "not": { "value_ew": "2" } }""" |> JsonDocument.Parse |> _.RootElement
        let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", notEndsFilter)
        let filter = Not (EndsWith ({ FieldName = "value"; Value = "2" }, StringComparer.CurrentCultureIgnoreCase))
        let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) (filter)
        let result = executeAndVerifyFilter (query, variables, filter)

        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
        result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

    do
        let notGreaterThanOrEqualFilter = """{ "not": { "id_greater_than_or_equal": 2 } }""" |> JsonDocument.Parse |> _.RootElement
        let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", notGreaterThanOrEqualFilter)
        let filter = Not (GreaterThanOrEqual { FieldName = "id"; Value = 2.0 })
        let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) (filter)
        let result = executeAndVerifyFilter (query, variables, filter)

        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
        result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

    do
        let notLessThanOrEqualFilter = """{ "not": { "id_less_than_or_equal": 4 } }""" |> JsonDocument.Parse |> _.RootElement
        let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", notLessThanOrEqualFilter)
        let filter = Not (LessThanOrEqual { FieldName = "id"; Value = 4.0 })
        let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) (filter)
        let result = executeAndVerifyFilter (query, variables, filter)

        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
        result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

    do
        let notGreaterThanFilter = """{ "not": { "id_greater_than": 2 } }""" |> JsonDocument.Parse |> _.RootElement
        let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", notGreaterThanFilter)
        let filter = Not (GreaterThan { FieldName = "id"; Value = 2.0 })
        let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) (filter)
        let result = executeAndVerifyFilter (query, variables, filter)

        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
        result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

    do
        let notLessThanFilter = """{ "not": { "id_less_than": 4 } }""" |> JsonDocument.Parse |> _.RootElement
        let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", notLessThanFilter)
        let filter = Not (LessThan { FieldName = "id"; Value = 4.0 })
        let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) (filter)
        let result = executeAndVerifyFilter (query, variables, filter)

        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
        result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

    do
        let notGreaterThanOrEqualFilter = """{ "not": { "id_gte": 2 } }""" |> JsonDocument.Parse |> _.RootElement
        let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", notGreaterThanOrEqualFilter)
        let filter = Not (GreaterThanOrEqual { FieldName = "id"; Value = 2.0 })
        let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) (filter)
        let result = executeAndVerifyFilter (query, variables, filter)

        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
        result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

    do
        let notLessThanOrEqualFilter = """{ "not": { "id_lte": 4 } }""" |> JsonDocument.Parse |> _.RootElement
        let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", notLessThanOrEqualFilter)
        let filter = Not (LessThanOrEqual { FieldName = "id"; Value = 4.0 })
        let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) (filter)
        let result = executeAndVerifyFilter (query, variables, filter)

        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
        result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

    do
        let notGreaterThanFilter = """{ "not": { "id_gt": 2 } }""" |> JsonDocument.Parse |> _.RootElement
        let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", notGreaterThanFilter)
        let filter = Not (GreaterThan { FieldName = "id"; Value = 2.0 })
        let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) (filter)
        let result = executeAndVerifyFilter (query, variables, filter)

        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
        result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

    do
        let notLessThanFilter = """{ "not": { "id_lt": 4 } }""" |> JsonDocument.Parse |> _.RootElement
        let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", notLessThanFilter)
        let filter = Not (LessThan { FieldName = "id"; Value = 4.0 })
        let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) (filter)
        let result = executeAndVerifyFilter (query, variables, filter)

        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
        result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

    do
        let notContainsFilter = """{ "not": { "value_contains": "A" } }""" |> JsonDocument.Parse |> _.RootElement
        let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", notContainsFilter)
        let filter = Not (Contains ({ FieldName = "value"; Value = "A" }, StringComparer.CurrentCultureIgnoreCase))
        let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) (filter)
        let result = executeAndVerifyFilter (query, variables, filter)

        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
        result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

    do
        let notEqualsFilter = """{ "not": { "value": "A2" } }""" |> JsonDocument.Parse |> _.RootElement
        let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", notEqualsFilter)
        let filter = Not (Equals ({ FieldName = "value"; Value = "A2" }, null))
        let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) (filter)
        let result = executeAndVerifyFilter (query, variables, filter)

        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
        result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

[<Fact>]
let ``Object list filter: Must parse filter that references variables`` () =
    let query =
        parse
            """query testQuery($filter: String) {
                A (id : 1) {
                    id
                    value
                    subjects (filter : { value_starts_with : $filter }) { ...Value }
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
    do
        let filterValue = "3" |> JsonDocument.Parse |> _.RootElement
        let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", filterValue)
        let filter = (StartsWith ({ FieldName = "value"; Value = "3" }, StringComparer.CurrentCultureIgnoreCase))
        let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) (filter)
        let result = executeAndVerifyFilter (query, variables, filter)

        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
        result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

[<Fact>]
let ``Object list filter: Must parse inline filter variable backed by Guid scalar`` () =
    let query =
        parse
            """query testQuery($filter: Guid!) {
                A (id : 1) {
                    id
                    value
                    subjects (filter : { guidValue : $filter }) { ...Value }
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

    let guidText = "22222222-2222-2222-2222-222222222222"
    let filterValue = $"\"{guidText}\"" |> JsonDocument.Parse |> _.RootElement
    let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("filter", filterValue)
    let filter = Equals ({ FieldName = "guidvalue"; Value = guidText }, null)
    let expectedFilter : KeyValuePair<obj list, _> = kvp ([ "A"; "subjects" ]) filter
    let result = executeAndVerifyFilter (query, variables, filter)

    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
    result.Metadata.TryFind<ObjectListFilters> ("filters") |> wantValueSome |> seqEquals [ expectedFilter ]

[<Fact>]
let ``Object list filter: Must parse inline filter variable backed by wrapped value object`` () =
    let query =
        parse
            """query testQuery($valueObject: ValueObject!) {
                A (id : 1) {
                    id
                    value
                    subjects (filter : { valueObject : $valueObject }) { ...Value }
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

    let valueObjectText = "22222222-2222-2222-2222-222222222222"
    let valueObjectVariable = $"\"{valueObjectText}\"" |> JsonDocument.Parse |> _.RootElement
    let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("valueObject", valueObjectVariable)
    let result = executeWithVariables (query, variables)

    ensureDirect result <| fun _ errors ->
        empty errors

[<Fact>]
let ``Object list filter: Must return empty filter when all discriminated union types are specified`` () =
    let query =
        parse
            """query testQuery() { Properties { ...Value } }

        fragment Value on Property {
                ...on Complex {
                    id
                    name
                    discriminator
                }
                ...on Building {
                    id
                    name
                    discriminator
                }
                ...on Community {
                    id
                    name
                    discriminator
                }
        }"""
    let result = execute query
    ensureDirect result <| fun _ errors -> empty errors

[<Fact>]
let ``Object list filter: Must return empty filter when no discriminated union types are specified`` () =
    let query = parse """query testQuery() { Properties { __typename } }"""
    let result = execute query
    ensureDirect result <| fun _ errors -> empty errors
