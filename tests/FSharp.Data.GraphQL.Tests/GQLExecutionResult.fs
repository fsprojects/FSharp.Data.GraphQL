[<AutoOpen>]
module FSharp.Data.GraphQL.GQLExecutionResultExtensions

open FSharp.Data.GraphQL

let (|RequestError|Direct|Deferred|Stream|) (response : GQLExecutionResult) =
    match response.Content with
    | RequestError errs -> RequestError errs
    | Direct (data, errors) -> Direct (data, errors)
    | Deferred (data, errors, deferred) -> Deferred (data, errors, deferred)
    | Stream data -> Stream data
