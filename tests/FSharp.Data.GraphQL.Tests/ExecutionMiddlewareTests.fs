module FSharp.Data.GraphQL.Tests.ExecutionMiddlewareTests

open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Ast

#nowarn "40"

type TestSubject = {
    a: string
    b: string
    c: string
}

let data =
    { a = "Apple"
      b = "Banana"
      c = "Cookie" }

let DataType =
    Define.Object<TestSubject>(
        "Data",
        fieldsFn = fun () ->
        [
            Define.Field("a", String, resolve = fun _ dt -> dt.a)
            Define.Field("b", String, resolve = fun _ dt -> dt.b)
            Define.Field("c", String, resolve = fun _ dt -> dt.c)
        ])
let Query =
    Define.Object<TestSubject>(
        "Query",
        fieldsFn = fun () -> [ Define.Field("testData", DataType, (fun _ _ -> data)) ] )

let schema = Schema(Query)

let ast = parse """{
        testData {
            a
            b
            c
        }
    }"""

let middlewareFunc (args : ExecutionFuncArgs<TestSubject>) (next : ExecutionFunc<TestSubject>) =
    let (plan, data, variables, args) = args
    let chooserS set =
        set |> List.choose (fun x -> match x with Field f when f.Name <> "c" -> Some x | _ -> None)
    let chooserK kind =
        match kind with
        | SelectFields fields -> fields |> List.choose (fun f -> if f.Identifier <> "c" then Some f else None) |> SelectFields
        | other -> other
    let selection = 
        plan.Operation.SelectionSet
        |> List.map (fun x -> match x with Field f -> Field { f with SelectionSet = chooserS f.SelectionSet } | _ -> x)
    let fields =
        plan.Fields
        |> List.map (fun x -> { x with Ast = { x.Ast with SelectionSet = chooserS x.Ast.SelectionSet }; Kind = chooserK x.Kind })
    let plan' = { plan with Operation = { plan.Operation with SelectionSet = selection }; Fields = fields }
    next (plan', data, variables, args)

let middleware = { new IExecutionMiddleware<TestSubject> with member __.ExecuteAsync = middlewareFunc }

let doTest (executor : Executor<TestSubject>) =
    let result = sync <| executor.AsyncExecute(ast)
    let expected = 
            NameValueLookup.ofList 
                [ "testData", 
                    upcast NameValueLookup.ofList 
                        [ "a", upcast "Apple" 
                          "b", upcast "Banana" ] ]
    match result with
    | Direct (data, errors) ->
        empty errors
        data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQLResponse"

[<Fact>]
let ``Execution middleware 1: remove field from execution plan (as function)`` () =
    let executor = Executor(schema, [ middlewareFunc ])
    doTest executor

[<Fact>]
let ``Execution middleware 2: remove field from execution plan (as Interface)`` () =
    let executor = Executor(schema, [ middleware ])
    doTest executor