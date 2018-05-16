module FSharp.Data.GraphQL.Tests.ExecutorMiddlewareTests

open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Ast
open System.Diagnostics

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

// On the schema compile phase, we hack the compiling to make the field a return the value of c
let compileMiddleware (ctx : SchemaCompileContext) (next : SchemaCompileContext -> unit) =
    let fieldDef = Define.Field("a", String, fun _ dt -> dt.c)
    ctx.FieldExecuteMap.SetExecute("Data", fieldDef)
    next ctx

// On the planning phase, we watch the time needed to do the operation
let planningMiddleware (ctx : PlanningContext) (next : PlanningContext -> ExecutionPlan) =
    let watch = Stopwatch()
    watch.Start()
    System.Threading.Thread.Sleep(5) // To make sure a minimum time will be needed to do this
    let result = next ctx
    watch.Stop()
    let metadata = result.Metadata.Add("planningTime", watch.ElapsedMilliseconds)
    { result with Metadata = metadata }

// On the execution phase, we remove the evaluation of the c field
let executionMiddleware (ctx : ExecutionContext) (next : ExecutionContext -> AsyncVal<GQLResponse>) =
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

let executorMiddleware =
    { new IExecutorMiddleware with
        member __.CompileSchema = Some compileMiddleware
        member __.PlanOperation = Some planningMiddleware
        member __.ExecuteOperationAsync = Some executionMiddleware }

let executor = Executor(schema, [ executorMiddleware ])

[<Fact>]
let ``Execution middleware: change fields and measure planning time`` () =
    let result = sync <| executor.AsyncExecute(ast)
    let expected = 
            NameValueLookup.ofList 
                [ "testData", 
                    upcast NameValueLookup.ofList 
                        [ "a", upcast "Cookie" 
                          "b", upcast "Banana" ] ]
    match result with
    | Direct (data, errors) ->
        empty errors
        data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQLResponse"
    match result.Metadata.TryFind<int64>("planningTime") with
    | Some time -> time |> greaterThanOrEqual 5L
    | None -> fail "Expected planning time on GQLResponse metadata, but it was not found"