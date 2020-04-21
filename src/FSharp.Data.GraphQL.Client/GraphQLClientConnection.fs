/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Net.Http

/// The connection component for GraphQL client operations.
[<AllowNullLiteral>]
type GraphQLClientConnection(client : HttpClient, ownsClient : bool) =
    new(client) = new GraphQLClientConnection(client, false)

    new() = new GraphQLClientConnection(new HttpClient(), true)

    member internal __.Client = client
    
    interface IDisposable with
        member __.Dispose() = if ownsClient then client.Dispose()
