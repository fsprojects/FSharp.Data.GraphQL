module FSharp.Data.GraphQL.Server.AspNetCore.Parser

open Microsoft.AspNetCore.Http
open FSharp.Data.GraphQL

let parseOrIResult instance =
    Parser.tryParse
    >> Result.mapError (fun errorMessage ->
        Results.Problem(errorMessage, instance, StatusCodes.Status400BadRequest, "Cannot parse GraphQL query"))
