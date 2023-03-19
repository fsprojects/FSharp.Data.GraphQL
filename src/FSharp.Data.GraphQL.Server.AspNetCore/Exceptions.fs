namespace FSharp.Data.GraphQL.Server.AspNetCore

type InvalidMessageException (explanation : string) =
  inherit System.Exception(explanation)