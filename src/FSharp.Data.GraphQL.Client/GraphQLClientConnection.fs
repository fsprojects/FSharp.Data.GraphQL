/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Net.Http

/// The connection component for GraphQL client operations.
[<AllowNullLiteral>]
type GraphQLClientConnection(invoker : HttpMessageInvoker, ownsInvoker : bool) =
    /// Creates a new instance of GraphQLClientConnection, using a provided System.Net.Http.HttpMessageInvoker to be wrapped inside.
    /// The provided System.Net.Http.HttpMessageInvoker is not owned by the connection, so it must be disposed when it is not necessary anymore.
    new(invoker) = new GraphQLClientConnection(invoker, false)

    /// Creates a new instance of GraphQLClientConnection, using a fresh new instance of System.Net.Http.HttpClient as an System.Net.Http.HttpMessageInvoker.
    /// This instance is owned by the GraphQLClientConnection, being disposed when the connection itself is disposed.
    new() = new GraphQLClientConnection(new HttpClient(), true)

    member internal __.Invoker = invoker
    
    interface IDisposable with
        member __.Dispose() = if ownsInvoker then invoker.Dispose()
