module FSharp.Data.GraphQL.Tests.AspNetCore.IncrementalPayloadSplittingTests

open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Server.AspNetCore.IncrementalPayloadSplitting

[<Fact>]
let ``BatchPath does not match a path already addressed by a single index`` () =
    match [ box "items"; box 0 ] with
    | BatchPath _ -> Assert.Fail "a single-index path was matched as a batch"
    | _ -> ()

[<Fact>]
let ``BatchPath matches a path ending in a list of indices`` () =
    match [ box "items"; box [ box 2; box 1 ] ] with
    | BatchPath (fieldPath, indices) ->
        fieldPath |> equals [ box "items" ]
        indices |> equals [ box 2; box 1 ]
    | _ -> fail "expected a BatchPath match"

[<Fact>]
let ``splitBatch addresses every item at its own index, out-of-order and preserving order`` () =
    // Regression test: a batch's path ends in the list of its items' indices (as produced by
    // Execution.collectItems), such as ["items"; [2; 1]], which no graphql-transport-ws client can merge -
    // no single index identifies where the payload belongs. Splitting must produce one independently
    // addressed payload per item, in the same relative order as the batch's own data array.
    let data = box [| box "Buffered 3"; box "Buffered 2" |]
    let split = splitBatch [ box "items" ] [ box 2; box 1 ] data []
    split
    |> List.map (fun (itemData, errors, path) -> (itemData :?> obj[]), errors, path)
    |> seqEquals [
        [| box "Buffered 3" |], [], [ box "items"; box 2 ]
        [| box "Buffered 2" |], [], [ box "items"; box 1 ]
    ]

[<Fact>]
let ``splitBatch attributes each error only to the item whose path it belongs to`` () =
    let itemError = GQLProblemDetails.CreateWithKind ("Boom", Execution, [ box "items"; box 0; box "value" ])
    let data = box [| box "zero"; box "one" |]
    let split = splitBatch [ box "items" ] [ box 0; box 1 ] data [ itemError ]
    let errorsOf index =
        split
        |> List.find (fun (_, _, path) -> path = [ box "items"; box index ])
        |> fun (_, errors, _) -> errors
    errorsOf 0 |> seqEquals [ itemError ]
    errorsOf 1 |> empty
