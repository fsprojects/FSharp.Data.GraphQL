// The MIT License (MIT)

module FSharp.Data.GraphQL.ErrorMessages

open System

let variableNotFound variableName  = $"A variable '$%s{variableName}' was not provided"

let expectedEnumerableValue indetifier ``type`` = $"Expected to have enumerable value in field '%s{indetifier}' but got '%O{(``type``:Type)}'"
