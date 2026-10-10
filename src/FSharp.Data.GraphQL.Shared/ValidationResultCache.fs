namespace FSharp.Data.GraphQL.Validation

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Security.Cryptography
open System.Threading
open Microsoft.Extensions.Caching.Memory

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types.Introspection

/// <summary>
/// Computes the hash codes by which validation results are cached, and the size of documents.
/// </summary>
/// <remarks>
/// The structural hash code of a document cannot be used: it is the same in every process and its literals collide
/// trivially, as <c>0</c> and <c>4294967297</c> do, so a client could send many different documents sharing one hash code
/// and make every cache lookup compare the document with all of them. Here every value is mixed into the hash code in
/// full, together with a secret seed chosen once per process.
/// </remarks>
module internal DocumentHashing =

    let private seed =
        let bytes = Array.zeroCreate<byte> 8
        use random = RandomNumberGenerator.Create ()
        random.GetBytes bytes
        BitConverter.ToUInt64 (bytes, 0)

    /// <summary>
    /// Mixes a value into a hash state.
    /// </summary>
    /// <remarks>
    /// The finalizer of SplitMix64 is a bijection in which every bit of the result depends on every bit of the state and
    /// of the value, so which documents share a hash code depends on the secret seed the state starts from.
    /// </remarks>
    let mix (state : uint64) (value : uint64) =
        let mutable z = (state ^^^ value) * 0x9E3779B97F4A7C15UL
        z <- (z ^^^ (z >>> 30)) * 0xBF58476D1CE4E5B9UL
        z <- (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
        z ^^^ (z >>> 31)

    /// The average memory of a node of a parsed document, without the characters of its strings, in bytes. On .NET 10,
    /// parsed documents take between 0.6 and 1.2 times the estimate it gives: more for plain fields, less for arguments
    /// and directives
    [<Literal>]
    let private NodeBytes = 128L

    [<Sealed>]
    type private Accumulator () =
        let mutable state = seed
        let mutable size = 0L

        member _.State = state
        member _.Size = size

        member _.Add (value : uint64) = state <- mix state value

        // String hash codes are themselves randomized per process on .NET; .NET Framework does not randomize them, but
        // only the client provider runs there, hashing documents written by the developer rather than sent by clients.
        // A string takes two bytes per character, which the size counts: a single literal can be as large as the document
        member this.Add (value : string) =
            size <- size + 2L * int64 value.Length
            this.Add (uint64 (uint32 (StringComparer.Ordinal.GetHashCode value)))

        /// Adds a node of the document, identified by its kind, and counts its memory
        member this.Node (kind : uint64) =
            size <- size + NodeBytes
            this.Add kind

    let private addOptional (acc : Accumulator) (value : string voption) =
        match value with
        | ValueSome value ->
            acc.Add 1UL
            acc.Add value
        | ValueNone -> acc.Add 0UL

    let private addList (acc : Accumulator) (add : 'T -> unit) (items : 'T list) =
        let mutable count = 0UL
        for item in items do
            add item
            count <- count + 1UL
        acc.Add count

    let rec private addType (acc : Accumulator) (inputType : InputType) =
        match inputType with
        | NamedType name ->
            acc.Node 1UL
            acc.Add name
        | ListType inner ->
            acc.Node 2UL
            addType acc inner
        | NonNullType inner ->
            acc.Node 3UL
            addType acc inner

    let rec private addValue (acc : Accumulator) (value : InputValue) =
        match value with
        | IntValue value ->
            acc.Node 4UL
            acc.Add (uint64 value)
        | FloatValue value ->
            acc.Node 5UL
            // Document equality holds 0.0 equal to -0.0, so they must hash alike; it never holds a document with a NaN
            // equal to any other, so hashing every NaN alike only saves telling their payloads apart
            let bits =
                if value = 0.0 then 0L
                elif Double.IsNaN value then BitConverter.DoubleToInt64Bits Double.NaN
                else BitConverter.DoubleToInt64Bits value
            acc.Add (uint64 bits)
        | BooleanValue value ->
            acc.Node 6UL
            acc.Add (if value then 1UL else 0UL)
        | StringValue value ->
            acc.Node 7UL
            acc.Add value
        | NullValue -> acc.Node 8UL
        | EnumValue name ->
            acc.Node 9UL
            acc.Add name
        | ListValue items ->
            acc.Node 10UL
            addList acc (addValue acc) items
        | ObjectValue fields ->
            acc.Node 11UL
            for KeyValue (name, field) in fields do
                acc.Add name
                addValue acc field
            acc.Add (uint64 fields.Count)
        | VariableName name ->
            acc.Node 12UL
            acc.Add name

    let private addArgument (acc : Accumulator) (argument : Argument) =
        acc.Node 13UL
        acc.Add argument.Name
        addValue acc argument.Value

    let private addDirective (acc : Accumulator) (directive : Directive) =
        acc.Node 14UL
        acc.Add directive.Name
        addList acc (addArgument acc) directive.Arguments

    let rec private addSelection (acc : Accumulator) (selection : Selection) =
        match selection with
        | Field field ->
            acc.Node 15UL
            addOptional acc field.Alias
            acc.Add field.Name
            addList acc (addArgument acc) field.Arguments
            addList acc (addDirective acc) field.Directives
            addList acc (addSelection acc) field.SelectionSet
        | FragmentSpread spread ->
            acc.Node 16UL
            acc.Add spread.Name
            addList acc (addDirective acc) spread.Directives
        | InlineFragment fragment ->
            acc.Node 17UL
            addFragment acc fragment

    and private addFragment (acc : Accumulator) (fragment : FragmentDefinition) =
        addOptional acc fragment.Name
        addOptional acc fragment.TypeCondition
        addList acc (addDirective acc) fragment.Directives
        addList acc (addSelection acc) fragment.SelectionSet

    let private addVariable (acc : Accumulator) (variable : VariableDefinition) =
        acc.Node 18UL
        acc.Add variable.VariableName
        addType acc variable.Type
        match variable.DefaultValue with
        | Some value ->
            acc.Add 1UL
            addValue acc value
        | None -> acc.Add 0UL

    let private addDefinition (acc : Accumulator) (definition : Definition) =
        match definition with
        | OperationDefinition operation ->
            acc.Node 19UL
            acc.Add (
                match operation.OperationType with
                | OperationType.Query -> 0UL
                | OperationType.Mutation -> 1UL
                | OperationType.Subscription -> 2UL
            )
            addOptional acc operation.Name
            addList acc (addVariable acc) operation.VariableDefinitions
            addList acc (addDirective acc) operation.Directives
            addList acc (addSelection acc) operation.SelectionSet
        | FragmentDefinition fragment ->
            acc.Node 20UL
            addFragment acc fragment

    /// Returns the hash code of the document, seeded per process, and its size: the estimated memory of the parsed
    /// document, in bytes
    let hashAndMeasure (document : Document) =
        let acc = Accumulator ()
        acc.Node 0UL
        addList acc (addDefinition acc) document.Definitions
        struct (acc.State, acc.Size)

/// <summary>
/// Identifies the result of validating a GraphQL document against a schema.
/// </summary>
/// <remarks>
/// <para>
/// Two keys are equal only when they hold the same schema instance and structurally equal documents, so a cache never
/// serves the result of one document for another, nor the result for one schema for another. Hash codes only bucket the
/// keys: the hash code of a key is computed once, when the key is created, from every value of the document and a secret
/// seed chosen per process, so that clients cannot craft documents sharing one.
/// </para>
/// <para>
/// A key keeps its document alive for as long as a cache holds the key; <see cref="ValidationResultKey.DocumentSize"/>
/// measures it, so that a cache can bound the memory it holds.
/// </para>
/// </remarks>
[<Struct; CustomEquality; NoComparison>]
type ValidationResultKey =
    /// The schema the document is validated against, identified by reference: hashing or comparing its structure would
    /// take time proportional to the size of the whole schema on every request.
    val Schema : IntrospectionSchema

    /// The validated document, compared structurally.
    val Document : Document

    /// The estimated memory of the parsed document, in bytes: a fixed amount for each of its definitions, selections,
    /// arguments, directives, values, variables and type references, and two bytes for each character of its names and
    /// strings.
    val DocumentSize : int64

    val private hashCode : int

    /// <summary>Creates the key of the result of validating the document against the schema.</summary>
    /// <param name="schema">The schema the document is validated against.</param>
    /// <param name="document">The validated document.</param>
    new (schema : IntrospectionSchema, document : Document) =
        let struct (documentHash, documentSize) = DocumentHashing.hashAndMeasure document
        let hash = DocumentHashing.mix documentHash (uint64 (RuntimeHelpers.GetHashCode schema))
        {
            Schema = schema
            Document = document
            DocumentSize = documentSize
            hashCode = int hash ^^^ int (hash >>> 32)
        }

    /// <summary>Creates a key with the given hash code, so that tests can make the keys of different documents collide.</summary>
    /// <param name="schema">The schema the document is validated against.</param>
    /// <param name="document">The validated document.</param>
    /// <param name="hashCode">The hash code of the key.</param>
    internal new (schema : IntrospectionSchema, document : Document, hashCode : int) =
        let struct (_, documentSize) = DocumentHashing.hashAndMeasure document
        {
            Schema = schema
            Document = document
            DocumentSize = documentSize
            hashCode = hashCode
        }

    /// <summary>Indicates whether both keys hold the same schema instance and structurally equal documents.</summary>
    /// <param name="other">The key to compare with.</param>
    member key.Equals (other : ValidationResultKey) =
        key.hashCode = other.hashCode
        && obj.ReferenceEquals (key.Schema, other.Schema)
        && (obj.ReferenceEquals (key.Document, other.Document)
            || EqualityComparer<Document>.Default.Equals (key.Document, other.Document))

    /// <inheritdoc />
    override key.Equals (other : obj) =
        match other with
        | :? ValidationResultKey as other -> key.Equals other
        | _ -> false

    /// <inheritdoc />
    override key.GetHashCode () = key.hashCode

    interface IEquatable<ValidationResultKey> with
        /// <inheritdoc />
        member key.Equals other = key.Equals other

/// Produces the result of a validation for a cache that holds no result for its key.
type ValidationResultProducer = unit -> ValidationResult<GQLProblemDetails>

/// <summary>
/// A cache of the results of validating documents against schemas.
/// </summary>
/// <remarks>
/// An implementation must serve a result only for a key equal to the one it was produced for, by
/// <see cref="ValidationResultKey.Equals(ValidationResultKey)"/>; a hash code alone never identifies a document.
/// </remarks>
type IValidationResultCache =
    /// <summary>Returns the cached result for the key, or caches and returns the result of the producer.</summary>
    /// <param name="producer">Validates the document of the key against its schema.</param>
    /// <param name="key">The document and schema to get the validation result for.</param>
    abstract GetOrAdd : producer : ValidationResultProducer -> key : ValidationResultKey -> ValidationResult<GQLProblemDetails>

/// <summary>
/// A cache of validation results held in an <see cref="IMemoryCache"/>.
/// </summary>
/// <remarks>
/// <para>
/// An entry expires once it was not used for the sliding expiration, counted from when its validation finished.
/// Concurrent requests for a key that is not cached share a single validation, whether its result ends up cached or not;
/// a validation that throws is run again by the next request.
/// </para>
/// <para>
/// The cache holds the documents of its keys, so every entry declares the estimated memory of its document in bytes
/// (<see cref="ValidationResultKey.DocumentSize"/>) as its size. A cache created without an <see cref="IMemoryCache"/>
/// owns one that limits the total of those sizes: an entry that does not fit is not cached, and the least recently used
/// entries are evicted to make room for the next ones. This bounds the memory clients can make the cache hold by sending
/// distinct documents; the validation results are not counted.
/// </para>
/// <para>
/// An <see cref="IMemoryCache"/> passed in is used as it is configured, so give it a
/// <see cref="MemoryCacheOptions.SizeLimit"/> in bytes: without one it grows with every distinct document clients send.
/// A memory cache with a size limit requires every entry to declare its size, and in the same unit, so pass one that
/// holds validation results only, such as a keyed service, rather than the cache the rest of the application shares.
/// </para>
/// </remarks>
type MemoryValidationResultCache private (cache : IMemoryCache, slidingExpiration : TimeSpan, ownSizeLimit : int64 voption) =

    static let createOwnCache (sizeLimit : int64) : IMemoryCache =
        if sizeLimit <= 0L then
            raise (ArgumentOutOfRangeException (nameof sizeLimit, sizeLimit, "The size limit must be positive."))
        // Evicting a tenth of the limit at a time, instead of the twentieth of the default, lets more additions pass
        // before the next compaction of a cache kept full by new documents
        new MemoryCache (MemoryCacheOptions (SizeLimit = Nullable sizeLimit, CompactionPercentage = 0.1))

    do
        match cache with
        | null -> nullArg (nameof cache)
        | _ -> ()
        if slidingExpiration <= TimeSpan.Zero then
            raise (ArgumentOutOfRangeException (nameof slidingExpiration, slidingExpiration, "The sliding expiration must be positive."))

    // The validations running now. A memory cache does not run the factory of a key once for concurrent requests, so the
    // requests for a key share its validation here, and only a finished result is put into the memory cache: a
    // validation in flight can neither expire nor be evicted, however long it takes and however full the cache is
    let flights = ConcurrentDictionary<ValidationResultKey, Lazy<ValidationResult<GQLProblemDetails>>> ()

    let tryGet (key : obj) =
        match cache.TryGetValue key with
        | true, (:? ValidationResult<GQLProblemDetails> as result) -> ValueSome result
        | _ -> ValueNone

    let fitsOwnCache (key : ValidationResultKey) =
        match ownSizeLimit with
        // A memory cache compacts itself whenever an entry does not fit, and a document larger than the whole limit
        // never fits, so it is not offered at all: it would evict the other documents on every request
        | ValueSome sizeLimit -> key.DocumentSize <= sizeLimit
        | ValueNone -> true

    let store (key : ValidationResultKey) (result : ValidationResult<GQLProblemDetails>) =
        if fitsOwnCache key then
            // The entry is added when it is disposed
            use entry = cache.CreateEntry (box key)
            entry.SlidingExpiration <- Nullable slidingExpiration
            entry.Size <- Nullable key.DocumentSize
            entry.Value <- result

    let validate (producer : ValidationResultProducer) (key : ValidationResultKey) =
        let flight =
            Lazy<ValidationResult<GQLProblemDetails>> (
                (fun () ->
                    // A flight that finished after this request looked the key up, and before it started its own, has
                    // stored the result already
                    match tryGet (box key) with
                    | ValueSome result -> result
                    | ValueNone ->
                        let result = producer ()
                        store key result
                        result),
                LazyThreadSafetyMode.ExecutionAndPublication
            )
        let shared = flights.GetOrAdd (key, flight)
        try
            shared.Value
        finally
            // The request that started the flight ends it, after the result is stored, so that a request finding no
            // flight finds the result instead. The flight of a validation that threw ends too: the requests sharing it
            // get its exception, and the next one validates again
            if obj.ReferenceEquals (shared, flight) then
                flights.TryRemove key |> ignore

    /// The sliding expiration of a cache created without one: 30 seconds.
    static member DefaultSlidingExpiration = TimeSpan.FromSeconds 30.0

    /// The size limit of a cache created without arguments: 16 MiB of estimated memory, a few thousand typical documents.
    static member DefaultSizeLimit = 16L * 1024L * 1024L

    /// <summary>
    /// Creates a cache that owns its memory cache, with the <see cref="MemoryValidationResultCache.DefaultSlidingExpiration"/>
    /// and the <see cref="MemoryValidationResultCache.DefaultSizeLimit"/>.
    /// </summary>
    new () = MemoryValidationResultCache (MemoryValidationResultCache.DefaultSlidingExpiration, MemoryValidationResultCache.DefaultSizeLimit)

    /// <summary>Creates a cache that owns its memory cache.</summary>
    /// <param name="slidingExpiration">How long an entry stays cached after it was last used.</param>
    /// <param name="sizeLimit">The maximum total size of the cached documents, in bytes of estimated memory.</param>
    new (slidingExpiration : TimeSpan, sizeLimit : int64) =
        MemoryValidationResultCache (createOwnCache sizeLimit, slidingExpiration, ValueSome sizeLimit)

    /// <summary>
    /// Creates a cache that holds its entries in the given memory cache, with the
    /// <see cref="MemoryValidationResultCache.DefaultSlidingExpiration"/>.
    /// </summary>
    /// <param name="cache">The memory cache to hold the validation results in, which should have a size limit in bytes.</param>
    new (cache : IMemoryCache) = MemoryValidationResultCache (cache, MemoryValidationResultCache.DefaultSlidingExpiration, ValueNone)

    /// <summary>Creates a cache that holds its entries in the given memory cache.</summary>
    /// <param name="cache">The memory cache to hold the validation results in, which should have a size limit in bytes.</param>
    /// <param name="slidingExpiration">How long an entry stays cached after it was last used.</param>
    new (cache : IMemoryCache, slidingExpiration : TimeSpan) = MemoryValidationResultCache (cache, slidingExpiration, ValueNone)

    interface IValidationResultCache with
        /// <inheritdoc />
        member _.GetOrAdd producer key =
            match tryGet (box key) with
            | ValueSome result -> result
            | ValueNone -> validate producer key
