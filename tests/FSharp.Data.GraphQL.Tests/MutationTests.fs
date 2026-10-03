// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.MutationTests

open System
open System.Collections.Concurrent
open System.Threading.Tasks
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser

type NumberHolder = { mutable Number: int }
type Root =
    {
        NumberHolder: NumberHolder
    }
    member x.ChangeImmediatelly num =
        x.NumberHolder.Number <- num
        x.NumberHolder
    member x.AsyncChange num =
        async {
            x.NumberHolder.Number <- num
            return x.NumberHolder
        }
    member x.ChangeFail _: NumberHolder option =
        failwith "Cannot change number"
    member x.AsyncChangeFail _: Async<NumberHolder option> =
        async {
            return failwith "Cannot change number"
        }

let NumberHolder = Define.Object("NumberHolder", [ Define.Field("theNumber", IntType, fun _ x -> x.Number) ])
let schema =
  Schema(
    query = Define.Object("Query", [ Define.Field("numberHolder", NumberHolder, fun _ x -> x.NumberHolder) ]),
    mutation =
      Define.Object("Mutation",
      [
        Define.Field("immediatelyChangeTheNumber", NumberHolder, "", [ Define.Input("newNumber", IntType) ], fun ctx (x:Root) -> x.ChangeImmediatelly(ctx.Arg("newNumber")))
        Define.AsyncField("promiseToChangeTheNumber", NumberHolder, "", [ Define.Input("newNumber", IntType) ], fun ctx (x:Root) -> x.AsyncChange(ctx.Arg("newNumber")))
        Define.Field("failToChangeTheNumber", Nullable NumberHolder, "", [ Define.Input("newNumber", IntType) ], fun ctx (x:Root) -> x.ChangeFail(ctx.Arg("newNumber")))
        Define.AsyncField("promiseAndFailToChangeTheNumber", Nullable NumberHolder, "", [ Define.Input("newNumber", IntType) ], fun ctx (x:Root) -> x.AsyncChangeFail(ctx.Arg("newNumber")))
    ]))

[<Fact>]
let ``Execute handles mutation execution ordering: evaluates mutations serially`` () =
    let query = """mutation M {
      first: immediatelyChangeTheNumber(newNumber: 1) {
        theNumber
      },
      second: promiseToChangeTheNumber(newNumber: 2) {
        theNumber
      },
      third: immediatelyChangeTheNumber(newNumber: 3) {
        theNumber
      }
      fourth: promiseToChangeTheNumber(newNumber: 4) {
        theNumber
      },
      fifth: immediatelyChangeTheNumber(newNumber: 5) {
        theNumber
      }
    }"""

    let root = {NumberHolder = {Number = 6}}
    let mutationResult = sync <| Executor(schema).AsyncExecute(parse query, getMockInputContext, root)
    let expected =
      NameValueLookup.ofList [
        "first",  upcast NameValueLookup.ofList [ "theNumber", 1 :> obj]
        "second", upcast NameValueLookup.ofList [ "theNumber", 2 :> obj]
        "third",  upcast NameValueLookup.ofList [ "theNumber", 3 :> obj]
        "fourth", upcast NameValueLookup.ofList [ "theNumber", 4 :> obj]
        "fifth",  upcast NameValueLookup.ofList [ "theNumber", 5 :> obj]
    ]
    match mutationResult with
    | Direct(ValueSome data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | Direct(ValueNone, _) -> fail "Expected a 'Direct' GQLResponse with data but got null data"
    | response -> fail $"Expected a 'Direct' GQLResponse but got\n{response}"
    // Each field reads the number back right after setting it, so the data alone cannot tell the order the fields ran
    // in; the number left behind can, since only the field executed last leaves its own
    Assert.True (
      (root.NumberHolder.Number = 5),
      $"Expected the last mutation field to set the number last, leaving 5, but the number is %i{root.NumberHolder.Number}"
    )

[<Fact>]
let ``Execute handles mutation execution ordering: evaluates mutations correctly in the presense of failures`` () =
    let query = """mutation M {
      first: immediatelyChangeTheNumber(newNumber: 1) {
        theNumber
      },
      second: promiseToChangeTheNumber(newNumber: 2) {
        theNumber
      },
      third: failToChangeTheNumber(newNumber: 3) {
        theNumber
      }
      fourth: promiseToChangeTheNumber(newNumber: 4) {
        theNumber
      },
      fifth: immediatelyChangeTheNumber(newNumber: 5) {
        theNumber
      }
      sixth: promiseAndFailToChangeTheNumber(newNumber: 6) {
        theNumber
      }
    }"""

    let data = {NumberHolder = {Number = 6}}
    let mutationResult = sync <| Executor(schema).AsyncExecute(parse query, getMockInputContext, data)
    let expected =
      NameValueLookup.ofList [
        "first",  upcast NameValueLookup.ofList [ "theNumber", 1 :> obj]
        "second", upcast NameValueLookup.ofList [ "theNumber", 2 :> obj]
        "third",  null
        "fourth", upcast NameValueLookup.ofList [ "theNumber", 4 :> obj]
        "fifth",  upcast NameValueLookup.ofList [ "theNumber", 5 :> obj]
        "sixth",  null
    ]

    match mutationResult with
    | Direct(ValueSome data, errors) ->
      data |> equals (upcast expected)
      List.length errors |> equals 2
    | Direct(ValueNone, _) -> fail "Expected a 'Direct' GQLResponse with data but got null data"
    | response -> fail $"Expected a 'Direct' GQLResponse but got\n{response}"

/// <summary>
/// Records when the resolvers of an operation start and finish, and holds each gated resolver at its start until the
/// test releases it.
/// </summary>
/// <remarks>
/// The test starts the execution with <see cref="Async.StartImmediateAsTask"/> and releases one gate at a time, so a
/// resolver started while another one is still held is recorded before the held one finishes: the order of the
/// events depends only on the order the engine starts the resolvers in, never on timing.
/// </remarks>
type ResolutionLog () =
    let events = ConcurrentQueue<string> ()
    let gates = ConcurrentDictionary<string, struct (TaskCompletionSource * TaskCompletionSource)> (StringComparer.Ordinal)

    let gateOf (name : string) =
        gates.GetOrAdd (
            name,
            fun _ ->
                struct (TaskCompletionSource (TaskCreationOptions.RunContinuationsAsynchronously),
                        TaskCompletionSource (TaskCreationOptions.RunContinuationsAsynchronously))
        )

    /// The events recorded so far, in the order they happened.
    member _.Events = events |> Seq.toList

    /// Records an event.
    member _.Record (event : string) = events.Enqueue event

    /// Records the start of the resolution, then waits until the test releases it.
    member this.Enter (name : string) : Task =
        this.Record $"start %s{name}"
        let struct (started, released) = gateOf name
        started.SetResult ()
        released.Task

    /// Waits until the resolution has started, failing with the events recorded so far when it never does.
    member this.WaitStarted (name : string) : Task = task {
        let struct (started, _) = gateOf name
        try
            do! started.Task.WaitAsync (TimeSpan.FromSeconds 30.)
        with :? TimeoutException ->
            fail $"Expected the resolution of '%s{name}' to start, but it did not. Events so far: %A{this.Events}"
    }

    /// Waits until the resolution has started, then lets it finish.
    member this.Release (name : string) : Task = task {
        do! this.WaitStarted name
        let struct (_, released) = gateOf name
        released.SetResult ()
    }

type Account = { Name : string; Log : ResolutionLog }

/// A resolution through a task, which starts running as soon as the resolver is called.
let startTaskResolution (log : ResolutionLog) (name : string) : Task<Account> = task {
    do! log.Enter name
    log.Record $"finish %s{name}"
    return { Name = name; Log = log }
}

/// A resolution through a cold async computation, which starts running only once the engine runs it.
let asyncResolution (log : ResolutionLog) (name : string) = async {
    do! log.Enter name |> Async.AwaitTask
    log.Record $"finish %s{name}"
    return { Name = name; Log = log }
}

/// A synchronous resolution, which runs to completion as soon as the resolver is called.
let syncResolution (log : ResolutionLog) (name : string) =
    log.Record $"start %s{name}"
    log.Record $"finish %s{name}"
    { Name = name; Log = log }

/// A nested field resolved through a task, so the field selecting it completes only after it has.
let startDetailsResolution (account : Account) : Task<string> = task {
    let! details = startTaskResolution account.Log $"%s{account.Name}.details"
    return details.Name
}

let AccountType =
    Define.Object<Account> (
        "Account",
        [
            Define.Field ("name", StringType, fun _ account -> account.Name)
            Define.AsyncField ("details", StringType, fun _ account -> startDetailsResolution account |> Async.AwaitTask)
        ]
    )

/// Root fields of every resolver flavour; the root value is the log of the execution.
let resolutionFields : FieldDef<ResolutionLog> list = [
    Define.AsyncField (
        "taskBased",
        AccountType,
        [ Define.Input ("name", StringType) ],
        fun ctx log -> startTaskResolution log (ctx.Arg "name") |> Async.AwaitTask
    )
    Define.AsyncField ("asyncBased", AccountType, [ Define.Input ("name", StringType) ], fun ctx log -> asyncResolution log (ctx.Arg "name"))
    Define.Field ("syncBased", AccountType, [ Define.Input ("name", StringType) ], fun ctx log -> syncResolution log (ctx.Arg "name"))
]

let resolutionOrderSchema =
    Schema (query = Define.Object<ResolutionLog> ("Query", resolutionFields), mutation = Define.Object<ResolutionLog> ("Mutation", resolutionFields))

let private accountOf (name : string) : obj = upcast NameValueLookup.ofList [ "name", box name ]

let private accountWithDetailsOf (name : string) : obj =
    upcast NameValueLookup.ofList [ "name", box name; "details", box $"%s{name}.details" ]

[<Fact>]
let ``Execute resolves each mutation root field only after the previous one, nested fields included, has completed`` () : Task = task {
    let query = """mutation {
      first: taskBased(name: "first") { name }
      second: syncBased(name: "second") { name }
      third: asyncBased(name: "third") { name }
      fourth: syncBased(name: "fourth") { name details }
      fifth: taskBased(name: "fifth") { name details }
    }"""
    let log = ResolutionLog ()
    let execution = Executor(resolutionOrderSchema).AsyncExecute (parse query, getMockInputContext, log) |> Async.StartImmediateAsTask
    // Released in the order a serial execution reaches them; a resolver the engine starts early is recorded before the
    // previous field finishes
    for name in [ "first"; "third"; "fourth.details"; "fifth"; "fifth.details" ] do
        do! log.Release name
    let! result = execution

    Assert.Equal<string list> (
        [
            "start first"; "finish first"
            "start second"; "finish second"
            "start third"; "finish third"
            "start fourth"; "finish fourth"
            "start fourth.details"; "finish fourth.details"
            "start fifth"; "finish fifth"
            "start fifth.details"; "finish fifth.details"
        ],
        log.Events
    )
    let expected =
        NameValueLookup.ofList [
            "first", accountOf "first"
            "second", accountOf "second"
            "third", accountOf "third"
            "fourth", accountWithDetailsOf "fourth"
            "fifth", accountWithDetailsOf "fifth"
        ]
    match result with
    | Direct (ValueSome data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | Direct (ValueNone, errors) -> fail $"Expected a 'Direct' GQLResponse with data but got null data and errors %A{errors}"
    | response -> fail $"Expected a 'Direct' GQLResponse but got\n{response}"
}

[<Fact>]
let ``Execute resolves mutation root fields with disabled defer directives serially`` () : Task = task {
    // Validation allows @defer at the root of a mutation only when it is disabled; the fields are then executed in
    // place, still one after another
    let query = """mutation {
      first: taskBased(name: "first") @defer(if: false) { name }
      ... @defer(if: false) {
        second: taskBased(name: "second") { name }
        third: asyncBased(name: "third") { name }
      }
      fourth: taskBased(name: "fourth") { name }
    }"""
    let log = ResolutionLog ()
    let execution = Executor(resolutionOrderSchema).AsyncExecute (parse query, getMockInputContext, log) |> Async.StartImmediateAsTask
    for name in [ "first"; "second"; "third"; "fourth" ] do
        do! log.Release name
    let! result = execution

    Assert.Equal<string list> (
        [
            "start first"; "finish first"
            "start second"; "finish second"
            "start third"; "finish third"
            "start fourth"; "finish fourth"
        ],
        log.Events
    )
    let expected =
        NameValueLookup.ofList [
            "first", accountOf "first"
            "second", accountOf "second"
            "third", accountOf "third"
            "fourth", accountOf "fourth"
        ]
    match result with
    | Direct (ValueSome data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | Direct (ValueNone, errors) -> fail $"Expected a 'Direct' GQLResponse with data but got null data and errors %A{errors}"
    | response -> fail $"Expected a 'Direct' GQLResponse but got\n{response}"
}

[<Fact>]
let ``Execute resolves query root fields concurrently`` () : Task = task {
    let query = """query {
      first: taskBased(name: "first") { name }
      second: asyncBased(name: "second") { name }
    }"""
    let log = ResolutionLog ()
    let execution = Executor(resolutionOrderSchema).AsyncExecute (parse query, getMockInputContext, log) |> Async.StartImmediateAsTask
    // Both start while neither has been released, which a serial execution never does
    do! log.WaitStarted "first"
    do! log.WaitStarted "second"
    Assert.Equal<string list> ([ "start first"; "start second" ], log.Events |> List.sort)
    do! log.Release "first"
    do! log.Release "second"
    let! result = execution

    let expected = NameValueLookup.ofList [ "first", accountOf "first"; "second", accountOf "second" ]
    match result with
    | Direct (ValueSome data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | Direct (ValueNone, errors) -> fail $"Expected a 'Direct' GQLResponse with data but got null data and errors %A{errors}"
    | response -> fail $"Expected a 'Direct' GQLResponse but got\n{response}"
}

//[<Fact>]
//let ``Execute handles mutation with multiple arguments`` () =
//    let query = """mutation M ($arg2: Int!) {
//      immediatelyChangeTheNumber(newNumber: $arg2) {
//        theNumber
//      }
//    }"""

//    let mutationResult = sync <| Executor(schema).AsyncExecute(parse query, {NumberHolder = {Number = 6}}, Map.ofList [ "arg1", box 3; "arg2", box 33])
//    let expected =
//      NameValueLookup.ofList [
//        "immediatelyChangeTheNumber", upcast NameValueLookup.ofList [ "theNumber", box 33]
//        ]
//    match mutationResult with
//    | Direct(data, errors) ->
//      empty errors
//      data |> equals (upcast expected)
//    | response -> fail $"Expected a 'Direct' GQLResponse but got\n{response}"
