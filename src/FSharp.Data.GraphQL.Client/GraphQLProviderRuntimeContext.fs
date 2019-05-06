namespace FSharp.Data.GraphQL

open System

/// Contains information about a GraphQLRuntimeContext.
type GraphQLRuntimeContextInfo =
    { ServerUrl : string
      HttpHeaders : seq<string * string> }

/// A context for running operations using the GraphQLProvider in runtime.
type GraphQLProviderRuntimeContext =
      /// Gets the URL of the server that this context refers to.
    { ServerUrl : string
      /// Gets the HTTP headers used for calls to the server that this context refers to.
      HttpHeaders : seq<string * string> }
    /// Gets the connection component used to make calls to the server.
    member __.Connection = new GraphQLClientConnection()
    interface IDisposable with
        member x.Dispose() = (x.Connection :> IDisposable).Dispose()