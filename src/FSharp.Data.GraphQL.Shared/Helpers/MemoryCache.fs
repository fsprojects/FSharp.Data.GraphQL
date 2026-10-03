namespace FSharp.Data.GraphQL

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.Threading


// Cache implementation originally based on http://www.fssnip.net/7UT/title/Threadsafe-Generic-MemoryCache-and-Memoize-Function

type internal CacheExpirationPolicy =
    | NoExpiration
    | AbsoluteExpiration of TimeSpan
    | SlidingExpiration of TimeSpan

module internal CacheClock =

    /// <summary>
    /// Creates a monotonic clock returning the time elapsed since its creation.
    /// </summary>
    /// <remarks>
    /// Unlike <see cref="DateTime.UtcNow"/>, it is not affected by adjustments of the system time, which would expire
    /// entries early or keep them too long.
    /// </remarks>
    let create () =
        let stopwatch = Stopwatch.StartNew ()
        fun () -> stopwatch.Elapsed

/// An entry of a memory cache: its value, produced at most once, its size, and when it was created and last used, in clock ticks.
[<Sealed>]
type internal CacheEntry<'value> (value : Lazy<'value>, size : int64, created : int64) =
    // Every cache hit refreshes it from whichever thread hits the entry, so it is read and written atomically
    let mutable lastUsage = created

    member _.Value = value
    member _.Size = size
    member _.Created = created
    member _.LastUsage = Interlocked.Read &lastUsage
    member _.Touch (now : int64) = Interlocked.Exchange (&lastUsage, now) |> ignore

/// <summary>
/// A thread-safe in-memory key/value cache with an expiration policy and a size limit.
/// </summary>
/// <remarks>
/// <para>
/// Concurrent requests for a key without an entry run the producer once and share its value: the entry holds a lazy value
/// forced by every requester, so no lock is held while the value is produced and requests for other keys are never
/// blocked. A producer that throws leaves no entry behind.
/// </para>
/// <para>
/// A cache hit refreshes the last usage of the entry, which a sliding expiration counts from, and which orders the
/// entries for eviction: when the total size exceeds the limit, the least recently used entries are evicted.
/// </para>
/// <para>
/// Expired entries are removed while the cache is used, at most once per sweep interval, instead of by a timer: a running
/// timer is rooted by the runtime and would keep the cache and all its entries alive after its owner is gone.
/// </para>
/// </remarks>
type internal MemoryCache<'key, 'value>
    /// <param name="policy">When the entries expire.</param>
    /// <param name="sizeLimit">The maximum total size of the entries.</param>
    /// <param name="getSize">Gets the size of the entry of a key.</param>
    /// <param name="comparer">Compares the keys.</param>
    /// <param name="clock">Returns the time elapsed since a fixed point; tests replace it to control expiration.</param>
    (policy : CacheExpirationPolicy, sizeLimit : int64, getSize : 'key -> int64, comparer : IEqualityComparer<'key>, clock : unit -> TimeSpan) =

    let entries = ConcurrentDictionary<'key, CacheEntry<'value>> (comparer)

    // Changed only by whoever adds or removes an entry, so that it always matches the entries in the dictionary
    let mutable totalSize = 0L

    let mutable lastSweep = (clock ()).Ticks

    // 1 while a thread sweeps or evicts entries, so that only one does at a time and the others never wait for it
    let mutable maintaining = 0

    let sweepInterval =
        let intervalOf (window : TimeSpan) =
            if window.TotalSeconds < 1.0 then TimeSpan.FromMilliseconds 100.0
            elif window.TotalMinutes < 1.0 then TimeSpan.FromSeconds 1.0
            else TimeSpan.FromMinutes 1.0
        match policy with
        | NoExpiration -> ValueNone
        | AbsoluteExpiration lifetime -> ValueSome (intervalOf lifetime).Ticks
        | SlidingExpiration window -> ValueSome (intervalOf window).Ticks

    let isExpired (now : int64) (entry : CacheEntry<'value>) =
        match policy with
        | NoExpiration -> false
        | AbsoluteExpiration lifetime -> now - entry.Created > lifetime.Ticks
        | SlidingExpiration window -> now - entry.LastUsage > window.Ticks

    /// Removes the entry of the key only if it is still this very entry, never a newer one that replaced it
    let tryRemove (key : 'key) (entry : CacheEntry<'value>) =
        // ConcurrentDictionary removes a pair through ICollection only when the value matches too, atomically;
        // entries have reference equality, so a newer entry of the key never matches
        if (entries :> ICollection<KeyValuePair<'key, CacheEntry<'value>>>).Remove (KeyValuePair (key, entry)) then
            Interlocked.Add (&totalSize, -entry.Size) |> ignore

    let removeExpired (now : int64) =
        for pair in entries do
            if isExpired now pair.Value then
                tryRemove pair.Key pair.Value

    let evictLeastRecentlyUsed () =
        // Evicting down to 90% of the limit instead of just below it lets many additions pass before the next scan,
        // so that a cache kept full by new keys does not sort its entries on every addition
        let target = sizeLimit - sizeLimit / 10L
        let byLastUsage = entries |> Seq.toArray |> Array.sortBy _.Value.LastUsage
        let mutable index = 0
        while Interlocked.Read &totalSize > target && index < byLastUsage.Length do
            let pair = byLastUsage[index]
            tryRemove pair.Key pair.Value
            index <- index + 1

    let maintain (now : int64) =
        let sweepDue =
            match sweepInterval with
            | ValueSome interval -> now - Interlocked.Read &lastSweep >= interval
            | ValueNone -> false
        if
            (sweepDue || Interlocked.Read &totalSize > sizeLimit)
            && Interlocked.CompareExchange (&maintaining, 1, 0) = 0
        then
            try
                if sweepDue then
                    Interlocked.Exchange (&lastSweep, now) |> ignore
                    removeExpired now
                if Interlocked.Read &totalSize > sizeLimit then
                    evictLeastRecentlyUsed ()
            finally
                Volatile.Write (&maintaining, 0)

    let force (key : 'key) (entry : CacheEntry<'value>) =
        try
            entry.Value.Value
        with _ ->
            // Lazy rethrows the exception of a failed producer to everyone forcing it later, so the entry is dropped:
            // the next request runs the producer again instead of failing until the entry expires
            tryRemove key entry
            reraise ()

    let getOrAdd (key : 'key) (producer : unit -> 'value) =
        let now = (clock ()).Ticks
        maintain now
        let cached =
            match entries.TryGetValue key with
            | true, entry when not (isExpired now entry) -> ValueSome entry
            | true, expired ->
                // An expired entry that no sweep has removed yet is replaced, never served
                tryRemove key expired
                ValueNone
            | _ -> ValueNone
        match cached with
        | ValueSome entry ->
            entry.Touch now
            force key entry
        | ValueNone ->
            let size = getSize key
            if size > sizeLimit then
                // An entry larger than the whole cache would evict all the others, so its value is not cached at all
                producer ()
            else
                let added = CacheEntry (Lazy<'value> (Func<'value> producer, LazyThreadSafetyMode.ExecutionAndPublication), size, now)
                // Concurrent requests for the key all get the entry added first and force its single lazy value
                let entry = entries.GetOrAdd (key, added)
                if obj.ReferenceEquals (entry, added) then
                    if Interlocked.Add (&totalSize, size) > sizeLimit then
                        maintain now
                else
                    entry.Touch now
                force key entry

    /// Creates a cache without a size limit, comparing the keys with their default equality.
    new (policy) = MemoryCache (policy, Int64.MaxValue, (fun _ -> 1L), EqualityComparer<'key>.Default, CacheClock.create ())

    /// The number of entries, including expired ones not removed yet.
    member _.Count = entries.Count

    /// The total size of the entries.
    member _.Size = Interlocked.Read &totalSize

    /// Returns the value of the key, running the producer when the key has no live entry.
    member _.GetOrAddResult key producer = getOrAdd key producer

    /// Removes the expired entries now instead of on a later access.
    member _.RemoveExpired () = removeExpired (clock ()).Ticks
