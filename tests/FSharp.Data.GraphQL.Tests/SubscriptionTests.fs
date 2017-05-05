module  FSharp.Data.GraphQL.Tests.SubscriptionTests

open System
open System.Collections.Generic
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

#nowarn "40"

type TestSubject = 
    { id: string
      a: string
      b: string
      o: string option
      mutable m: string option
    } 
    member this.mutate m' =
        this.m <- m'
        this



type Root = {
    id : string
}

let subjects = 
    [ { id = "1"
        a = "abc"
        b = "b"
        o = Some "e"
        m = Some "m"}
      { id = "2"
        a = "abc"
        b = "b"
        o = Some "e"
        m = Some "m"} ]
    
let getSubject id = subjects |> List.tryFind (fun s -> s.id = id)

let RootType =
    Define.Object<Root>(
        name = "Root",
        description = "The Root type to be passed to all our resolvers",
        isTypeOf = (fun o -> o :? Root),
        fieldsFn = fun () -> [
            Define.Field("a", String, "The ID of the client", fun _ r -> r.id)])

let TestType = 
    Define.Object<TestSubject>(
        name = "TestSubject",
        description = "A test subject",
        isTypeOf = (fun o -> o :? TestSubject),
        fieldsFn = fun () -> [
            Define.Field("id", String, "The id", fun _ t -> t.id)
            Define.Field("a", String, "The value a", fun _ t -> t.a)
            Define.Field("b", String, "The value b", fun _ t -> t.b)
            Define.Field("o", Nullable String, "The value o", fun _ t -> t.o)
            Define.Field("m", Nullable String, "The value m", fun _ t -> t.m)
        ])
let Query = 
    Define.Object<Root>(
        name = "Query",
        fields = [
            Define.Field("test", Nullable TestType, "Gets a test subject", [Define.Input("id", String)], fun ctx _ -> getSubject(ctx.Arg("id")))
        ])

let Mutation = 
    Define.Object<Root>(
        name = "Mutation",
        fields = [
            Define.Field(
                "setM", 
                Nullable TestType,
                "Sets the M field",
                [Define.Input("id", String); Define.Input("input", String)],
                fun ctx _ -> 
                    getSubject(ctx.Arg("id")) 
                    |> Option.map(fun s -> s.mutate(ctx.Arg("id")))
                    |> Option.map(fun s -> ctx.SubscriptionHandler.FireEvent TestType s;s))
        ])

let buildSubscription (args: InputFieldDef list) (callback: ResolveFieldContext -> 'Root -> IDictionary<string, obj> -> unit) (filter: ResolveFieldContext -> 'Root -> TestSubject -> bool) =
        Define.SubscriptionObject<Root>(
            name = "Subscription",
            fields = [
                Define.SubscriptionField(
                    "watchSubject",
                    RootType,
                    TestType,
                    "Watches to see if a subscription with a given id changes",
                    args,
                    callback,
                    filter)
            ])

[<Fact>]
let ``Test a simple subscription`` () =
    let subscription = buildSubscription [ Define.Input("id", String)](fun ctx root dict -> dict.["id"] |> equals (box "1")) (fun ctx root subject -> ctx.Arg("id") = subject.id)
    let schema = Schema(Query, Mutation, subscription)
    let ex = Executor(schema)
    ex.AsyncExecute("subscription WatchSubject { watchSubject(id:\"1\") {id}}") |> ignore
    ex.AsyncExecute("mutation SetM { setM(id:\"1\") {id}}") |> ignore

[<Fact>]
let ``Test a subscription using the root value`` () =
    let root = { id = "1"}
    let subscription = buildSubscription [] (fun ctx root dict -> dict.["id"] |> equals (box root.id) )(fun ctx root subject -> ctx.Arg("id") = root.id)
    let schema = Schema(Query, Mutation, subscription)
    let ex = Executor(schema)
    ex.AsyncExecute("subscription WatchSubject { watchSubject {id}}", data=root) |> ignore
    ex.AsyncExecute("mutation SetM { setM(id:\"1\") {id}}") |> ignore

[<Fact>]
let ``Test a subscription on a nullable value`` () =    
    let subscription = buildSubscription [ Define.Input("o", String)](fun ctx root dict -> dict.["o"] |> equals (box "e") )(fun ctx root subject -> ctx.Arg("o") = subject.o)
    let schema = Schema(Query, Mutation, subscription)
    let ex = Executor(schema)
    ex.AsyncExecute("subscription WatchSubject { watchSubject(o:\"e\") {id}}") |> ignore
    ex.AsyncExecute("mutation SetM { setM(id:\"1\") {id}}") |> ignore