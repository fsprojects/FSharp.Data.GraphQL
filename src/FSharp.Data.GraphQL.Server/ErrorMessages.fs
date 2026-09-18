// The MIT License (MIT)

module FSharp.Data.GraphQL.ErrorMessages

open System

let variableNotFound variableName  = $"A variable '$%s{variableName}' was not provided"

let expectedEnumerableValue identifier ``type`` = $"Expected to have enumerable or asynchronous enumerable value in field '%s{identifier}' but got '%O{(``type``:Type)}'"
