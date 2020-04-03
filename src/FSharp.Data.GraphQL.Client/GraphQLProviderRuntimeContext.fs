/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Net.Http

/// Contains information about a GraphQLRuntimeContext.
type GraphQLRuntimeContextInfo =
    { Client : HttpMessageInvoker }

/// A context for running operations using the GraphQLProvider in runtime.
type GraphQLProviderRuntimeContext =
      /// Gets the HttpMessageInvoker that sends requests with headers to URL of the server.
    { Client : HttpMessageInvoker }
    /// Gets the connection component used to make calls to the server.
    interface IDisposable with
        member c.Dispose() = (c.Client :> IDisposable).Dispose()