module FSharp.Data.GraphQL.Tests.ExecutorMiddlewareTests

open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
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
            Define.Field("d", Boolean, "Returns its argument", [ Define.Input("input", Boolean) ], fun ctx _ -> ctx.Arg<bool> "input")
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
            d(input : true )
        }
    }"""

// On the schema compile phase, we hack the compiling to make the field a return the value of c
let compileMiddleware (ctx : SchemaCompileContext) (next : SchemaCompileContext -> unit) =
    let fieldDef = Define.Field("a", String, fun _ dt -> dt.c)
    ctx.FieldExecuteMap.SetExecute("Data", fieldDef)
    next ctx

// After the schema has been compiled, update the input fields to flip every boolean input
let postCompileMiddleware (schema : ISchema) (next : ISchema -> unit) =
    let flipBools execute value vars =
        match value with
        | BooleanValue b -> execute (BooleanValue (not b)) vars
        | _ -> execute value vars
    schema.TypeMap.ToSeq()
    |> Seq.iter(fun (n, def) ->
                    match def with
                    | Object obj ->
                        obj.Fields
                        |> Map.iter(fun _ f -> f.Args |> Array.iter(fun f -> f.ExecuteInput <- (flipBools f.ExecuteInput)))
                    | _ -> ())
    next schema

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
let executionMiddleware (ctx : ExecutionContext) (next : ExecutionContext -> AsyncVal<GQLExecutionResult>) =
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

let middleware =
    { new IExecutorMiddleware with
        member _.CompileSchema = Some compileMiddleware
        member _.PostCompileSchema = Some postCompileMiddleware
        member _.PlanOperation = Some planningMiddleware
        member _.ExecuteOperationAsync = Some executionMiddleware }

let executor = Executor(schema, [ middleware ])

[<Fact>]
let ``Executor middleware: change fields and measure planning time`` () =
    let result = sync <| executor.AsyncExecute(ast)
    let expected =
            NameValueLookup.ofList
                [ "testData",
                    upcast NameValueLookup.ofList
                        [ "a", upcast "Cookie"
                          "b", upcast "Banana"
                          "d", upcast false ] ]
    match result with
    | Direct (data, errors) ->
        empty errors
        data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQLResponse"
    match result.Metadata.TryFind<int64>("planningTime") with
    | Some time -> time |> greaterThanOrEqual 5L
    | None -> fail "Expected planning time on GQLResponse metadata, but it was not found"
