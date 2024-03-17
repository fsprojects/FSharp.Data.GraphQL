namespace FSharp.Data.GraphQL.Server.AspNetCore

type InvalidWebsocketMessageException (explanation : string) =
    inherit System.Exception (explanation)
