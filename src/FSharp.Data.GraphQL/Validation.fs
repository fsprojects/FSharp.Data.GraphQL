/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Validation

open FSharp.Data.GraphQL.Types

type ValidationErrorInfo = {
    ObjectName: string
    Description: string
}

type ValidationResult =
    | Success
    | Error of ValidationErrorInfo list

let detectCyclesInFragment fragmentDefintion visited = false