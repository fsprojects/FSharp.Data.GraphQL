/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System.Collections.Concurrent
open ProviderImplementation.ProvidedTypes

type internal CacheInvalidator (location : IntrospectionLocation, invalidateFn : IntrospectionLocation -> unit) =
    let lockObj = obj()
    let mutable remainingTime = 30000
    do
        async {
            while remainingTime > 0 do
                do! Async.Sleep(1000)
                lock lockObj (fun _ -> remainingTime <- remainingTime - 1000)
            invalidateFn location
        } |> Async.Start
    member __.Reset() = lock lockObj (fun _ -> remainingTime <- 30000)

module internal DesignTimeCache =
    let private cache = ConcurrentDictionary<IntrospectionLocation, CacheInvalidator * ProvidedTypeDefinition>()
 
    let getOrAdd (location : IntrospectionLocation) (defMaker : unit -> ProvidedTypeDefinition) =
        if not (cache.ContainsKey(location))
        then
            let def = defMaker()
            let invalidateFn location = cache.TryRemove(location) |> ignore
            let invalidator = CacheInvalidator(location, invalidateFn)
            cache.TryAdd(location, (invalidator, def)) |> ignore
            def
        else 
            let invalidator, def = cache.[location]
            invalidator.Reset()
            def