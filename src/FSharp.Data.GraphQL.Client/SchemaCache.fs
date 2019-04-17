/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System.Collections.Concurrent
open FSharp.Data.GraphQL.Types.Introspection
open System.Net
open System

type internal SchemaCache () =
    let dict = ConcurrentDictionary<string, IntrospectionSchema>()
    let client = new WebClient()

    member  __.Dispose() =
        client.Dispose()
        dict.Clear()

    member __.Get(serverUrl, customHeaders) =
        if not (dict.ContainsKey(serverUrl))
        then
            let schema = GraphQLClient.sendIntrospectionRequest client serverUrl customHeaders  |> Serialization.deserializeSchema
            dict.TryAdd(serverUrl, schema) |> ignore
        dict.[serverUrl]

    interface IDisposable with
        member this.Dispose() = this.Dispose()