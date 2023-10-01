// The MIT License (MIT)

module FSharp.Data.GraphQL.ErrorMessagess

let variableNotFound variableName  = $"A variable '$%s{variableName}' was not provided"
