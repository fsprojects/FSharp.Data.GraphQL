/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Net.Http

/// Contains information about a GraphQLRuntimeContext.
type GraphQLRuntimeContextInfo =
    { ServerUrl : string
      HttpHeaders : seq<string * string> }

/// A context for running operations using the GraphQLProvider in runtime.
type GraphQLProviderRuntimeContext (httpClientFactory : unit -> HttpMessageInvoker) =
    member val HttpClientFactory = lazy httpClientFactory () with get,set
    /// Gets the connection component used to make calls to the server.
    member ctx.Client = ctx.HttpClientFactory.Value
    interface IDisposable with
        member ctx.Dispose() = (ctx.Client :> IDisposable).Dispose()
