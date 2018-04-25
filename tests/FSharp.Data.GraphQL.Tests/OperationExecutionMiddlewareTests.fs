module FSharp.Data.GraphQL.Tests.OperationExecutionMiddlewareTests

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

let middlewareFunc (ctx : ExecutionContext) (next : ExecutionContext -> AsyncVal<GQLResponse>) =
    let chooserS set =
        set |> List.choose (fun x -> match x with Field f when f.Name <> "c" -> Some x | _ -> None)
    let chooserK kind =
        match kind with
        | SelectFields fields -> fields |> List.choose (fun f -> if f.Identifier <> "c" then Some f else None) |> SelectFields
        | other -> other
    let selection = 
        ctx.ExecutionPlan.Operation.SelectionSet
        |> List.map (fun x -> match x with Field f -> Field { f with SelectionSet = chooserS f.SelectionSet } | _ -> x)
    let fields =
        ctx.ExecutionPlan.Fields
        |> List.map (fun x -> { x with Ast = { x.Ast with SelectionSet = chooserS x.Ast.SelectionSet }; Kind = chooserK x.Kind })
    let operation = { ctx.ExecutionPlan.Operation with SelectionSet = selection }
    let plan = { ctx.ExecutionPlan with Operation = operation; Fields = fields  }
    let ctx' = { ctx with ExecutionPlan = plan }
    next ctx'

let middleware = { new IOperationExecutionMiddleware with member __.ExecuteOperationAsync = middlewareFunc }

let executorMiddleware =
    { new IExecutorMiddleware with
        member __.CompileSchema = None
        member __.PlanOperation = None
        member __.ExecuteOperationAsync = Some middleware }

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
let ``Execution middleware: remove field from execution plan`` () =
    let executor = Executor(schema, [ executorMiddleware ])
    doTest executor