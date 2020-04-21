/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Net.Http

/// The connection component for GraphQL client operations.
[<AllowNullLiteral>]
type GraphQLClientConnection(client : HttpClient, ownsClient : bool) =
    /// Creates a new instance of GraphQLClientConnection, using a provided HttpClient to be wrapped inside.
    /// The provided HttpClient is not owned by the connection, so it must be disposed when it is not necessary anymore.
    new(client) = new GraphQLClientConnection(client, false)

    /// Creates a new instance of GraphQLClientConnection, using a fresh new instance of HttpClient internally.
    /// This instance is owned by the GraphQLClientConnection, being disposed when the connection itself is disposed.
    new() = new GraphQLClientConnection(new HttpClient(), true)

    member internal __.Client = client
    
    interface IDisposable with
        member __.Dispose() = if ownsClient then client.Dispose()
