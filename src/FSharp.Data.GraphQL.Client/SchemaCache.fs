/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System.Collections.Concurrent
open FSharp.Data.GraphQL.Types.Introspection
open System.Net
open System
open System.Threading
open System.Threading.Tasks

type internal SchemaCache (cacheExpiration : TimeSpan) =
    let dict = ConcurrentDictionary<string, DateTime * IntrospectionSchema>()
 
    let client = new WebClient()
 
    let tokenSource = new CancellationTokenSource()
    let token = tokenSource.Token
 
    let expirationChecker cacheExpiration (ct : CancellationToken) =
        ct.ThrowIfCancellationRequested()
        Thread.Sleep(1000)
        while true do
            dict
            |> Seq.map (|KeyValue|)
            |> Seq.iter (fun (key, (added, _)) ->
                if (DateTime.Now - added) > cacheExpiration
                then match dict.TryRemove(key) with _ -> ())

    do Task.Factory.StartNew((fun _ -> expirationChecker cacheExpiration token), token) |> ignore
    
    member  __.Dispose() =
        tokenSource.Cancel()
        client.Dispose()
        dict.Clear()
        tokenSource.Dispose()

    member __.Get(serverUrl, customHeaders) =
        if not (dict.ContainsKey(serverUrl))
        then
            let schema = GraphQLClient.sendIntrospectionRequest client serverUrl customHeaders |> Serialization.deserializeSchema
            dict.TryAdd(serverUrl, (DateTime.Now, schema)) |> ignore
        snd dict.[serverUrl]

    interface IDisposable with
        member this.Dispose() = this.Dispose()