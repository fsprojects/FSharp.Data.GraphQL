namespace FSharp.Data.GraphQL.Server.AppInfrastructure

type InvalidMessageException (explanation : string) =
    inherit System.Exception (explanation)
