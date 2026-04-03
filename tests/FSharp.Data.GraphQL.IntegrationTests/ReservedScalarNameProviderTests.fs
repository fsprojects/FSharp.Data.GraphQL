module FSharp.Data.GraphQL.IntegrationTests.ReservedScalarNameProviderTests

open Xunit
open Helpers
open FSharp.Data.GraphQL

type ObjectDateProvider = GraphQLProvider<"reserved_scalar_object_date_introspection.json">
type InputDateProvider = GraphQLProvider<"reserved_scalar_input_date_introspection.json">

module ObjectDateSchema =
    type SchemaDate = ObjectDateProvider.Types.Date

    let operation =
        ObjectDateProvider.Operation<"""query Q {
            dateInfo {
              value
              category
            }
          }""">()

    let compileSmoke () =
        let schemaDate = SchemaDate(value = "2026-04-03", category = "default")
        let operationInstance : ObjectDateProvider.Operations.Q = operation
        schemaDate |> ignore
        operationInstance |> ignore

module InputDateSchema =
    type SchemaDate = InputDateProvider.Types.Date

    let operation =
        InputDateProvider.Operation<"""query Q($input: Date) {
            echoDate(input: $input)
          }""">()

    let compileSmoke () =
        let schemaDate = SchemaDate(value = "2026-04-03", category = "default")
        let deferredRun : unit -> _ =
            fun () -> operation.Run(Unchecked.defaultof<GraphQLProviderRuntimeContext>, schemaDate)
        schemaDate |> ignore
        deferredRun |> ignore

[<Fact>]
let ``Should allow object types that reuse reserved scalar names`` () =
    ObjectDateSchema.compileSmoke ()
    true |> equals true

[<Fact>]
let ``Should allow input object types that reuse reserved scalar names`` () =
    InputDateSchema.compileSmoke ()
    true |> equals true
