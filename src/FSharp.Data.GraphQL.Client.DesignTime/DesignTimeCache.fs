/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open FSharp.Data.GraphQL.Client
open System.Collections.Concurrent
open ProviderImplementation.ProvidedTypes

type internal ProviderKey =
    { IntrospectionLocation : IntrospectionLocation
      CustomHttpHeadersLocation : StringLocation }

type internal CacheInvalidator (key : ProviderKey, invalidateFn : ProviderKey -> unit) =
    let lockObj = obj()
    let mutable remainingTime = 30000
    do
        async {
            while remainingTime > 0 do
                do! Async.Sleep(1000)
                lock lockObj (fun _ -> remainingTime <- remainingTime - 1000)
            invalidateFn key
        } |> Async.Start
    member __.Reset() = lock lockObj (fun _ -> remainingTime <- 30000)

module internal DesignTimeCache =
    let private cache = ConcurrentDictionary<ProviderKey, CacheInvalidator * ProvidedTypeDefinition>()
 
    let getOrAdd (key : ProviderKey) (defMaker : unit -> ProvidedTypeDefinition) =
        if not (cache.ContainsKey(key))
        then
            let def = defMaker()
            let invalidateFn location = cache.TryRemove(location) |> ignore
            let invalidator = CacheInvalidator(key, invalidateFn)
            cache.TryAdd(key, (invalidator, def)) |> ignore
            def
        else 
            let invalidator, def = cache.[key]
            invalidator.Reset()
            def