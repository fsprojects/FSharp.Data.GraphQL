/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Net.Http

/// The connection component for GraphQL client operations.
[<AllowNullLiteral>]
type GraphQLClientConnection() =
    let client = new HttpClient()
    let mutable disposed = false

    member internal __.Client = 
        if not disposed
        then client
        else raise <| ObjectDisposedException("GraphQLClientConnection")
    
    member internal __.Disposed = disposed

    interface IDisposable with
        member __.Dispose() =
            if not disposed then
                disposed <- true
                client.Dispose()
