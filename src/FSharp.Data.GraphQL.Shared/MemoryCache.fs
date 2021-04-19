namespace FSharp.Data.GraphQL

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Timers

// Cache implementation based on http://www.fssnip.net/7UT/title/Threadsafe-Generic-MemoryCache-and-Memoize-Function

type internal CacheExpirationPolicy =
    | NoExpiration
    | AbsoluteExpiration of TimeSpan
    | SlidingExpiration of TimeSpan

type internal CacheEntryExpiration =
    | NeverExpires
    | ExpiresAt of DateTime
    | ExpiresAfter of TimeSpan

type internal CacheEntry<'key, 'value> =
    { Key: 'key
      Value: 'value
      Expiration: CacheEntryExpiration
      LastUsage: DateTime }

module internal CacheExpiration =
    let isExpired (entry: CacheEntry<_,_>) =
        match entry.Expiration with
        | NeverExpires -> false
        | ExpiresAt date -> DateTime.UtcNow > date
        | ExpiresAfter window -> (DateTime.UtcNow - entry.LastUsage) > window

type internal IMemoryCacheStore<'key, 'value> =
    inherit IEnumerable<CacheEntry<'key, 'value>>
    abstract member Add: CacheEntry<'key, 'value> -> unit
    abstract member GetOrAdd: 'key -> ('key -> CacheEntry<'key, 'value>) -> CacheEntry<'key, 'value>
    abstract member Remove: 'key -> unit
    abstract member Contains: 'key -> bool
    abstract member Update: 'key -> (CacheEntry<'key, 'value> -> CacheEntry<'key, 'value>) -> unit
    abstract member TryFind: 'key -> CacheEntry<'key, 'value> option

type internal MemoryCache<'key, 'value> (?cacheExpirationPolicy) =
    let policy = defaultArg cacheExpirationPolicy NoExpiration
    let store =
        let entries = ConcurrentDictionary<'key, CacheEntry<'key, 'value>>()
        let getEnumerator =
            let values = entries |> Seq.map (fun kvp -> kvp.Value)
            fun () -> values.GetEnumerator()
        { new IMemoryCacheStore<'key, 'value> with
            member __.Add entry = entries.AddOrUpdate(entry.Key, entry, fun _ _ -> entry) |> ignore
            member __.GetOrAdd key getValue = entries.GetOrAdd(key, getValue)
            member __.Remove key = entries.TryRemove key |> ignore
            member __.Contains key = entries.ContainsKey key
            member __.Update key update =
                match entries.TryGetValue(key) with
                | (true, entry) -> entries.AddOrUpdate(key, entry, fun _ entry -> update entry) |> ignore
                | _ -> ()
            member __.TryFind key =
                match entries.TryGetValue(key) with
                | (true, entry) -> Some entry
                | _ -> None
            member __.GetEnumerator () = getEnumerator ()
            member __.GetEnumerator () = getEnumerator () :> Collections.IEnumerator }

    let checkExpiration () =
        store |> Seq.iter (fun entry -> if CacheExpiration.isExpired entry then store.Remove entry.Key)

    let newCacheEntry key value =
        { Key = key
          Value = value
          Expiration = match policy with
                       | NoExpiration -> NeverExpires
                       | AbsoluteExpiration time -> ExpiresAt (DateTime.UtcNow + time)
                       | SlidingExpiration window -> ExpiresAfter window
          LastUsage = DateTime.UtcNow }

    let add key value =
        if key |> store.Contains
        then store.Update key (fun entry -> {entry with Value = value; LastUsage = DateTime.UtcNow})
        else store.Add <| newCacheEntry key value

    let remove key =
        store.Remove key

    let get key =
        store.TryFind key |> Option.bind (fun entry -> Some entry.Value)

    let getOrAdd key value =
        store.GetOrAdd key (fun _ -> newCacheEntry key value)
        |> fun entry -> entry.Value

    let getOrAddResult key f =
        store.GetOrAdd key (fun _ -> newCacheEntry key <| f())
        |> fun entry -> entry.Value

    let getTimer (expiration: TimeSpan) =
        if expiration.TotalSeconds < 1.0
        then TimeSpan.FromMilliseconds 100.0
        elif expiration.TotalMinutes < 1.0
        then TimeSpan.FromSeconds 1.0
        else TimeSpan.FromMinutes 1.0
        |> fun interval -> new Timer(interval.TotalMilliseconds)

    let timer =
       match policy with
       | NoExpiration -> None
       | AbsoluteExpiration time -> time |> getTimer |> Some
       | SlidingExpiration time -> time |> getTimer |> Some

    let _observer =
        match timer with
        | Some t ->
            let disposable = t.Elapsed |> Observable.subscribe (fun _ -> checkExpiration())
            t.Start()
            Some disposable
        | None -> None

    member __.Add key value = add key value
    member __.Remove key = remove key
    member __.Get key = get key
    member __.GetOrAdd key value = getOrAdd key value
    member __.GetOrAddResult key f = getOrAddResult key f