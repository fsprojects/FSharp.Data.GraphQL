namespace FSharp.Data.GraphQL.Validation

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types.Introspection
open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Serialization

type ValidationResultKey =
    { DocumentId : string
      SchemaId : string }

type ValidationResultProducer =
    unit -> ValidationResult<GQLProblemDetails>

type IValidationResultCache =
    abstract GetOrAdd : ValidationResultProducer -> ValidationResultKey -> ValidationResult<GQLProblemDetails>

module SchemaId =

    let private formatByteAsLowerHex (value : byte) =
        value.ToString("x2", System.Globalization.CultureInfo.InvariantCulture)
    
    // Note: UnsafeRelaxedJsonEscaping is used here only for deterministic hashing,
    // not for output to untrusted contexts. The JSON is never exposed externally.
    let private jsonOptions = JsonSerializerOptions(
        WriteIndented = false,
        DefaultIgnoreCondition = JsonIgnoreCondition.Never,
        PropertyNamingPolicy = null,
        Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    )
    
    /// <summary>
    /// Computes a deterministic schema identifier from an introspection schema.
    /// </summary>
    /// <param name="introspectionSchema">The introspection schema to hash.</param>
    /// <returns>A lowercase hexadecimal SHA-256 hash string that uniquely identifies the schema structure.</returns>
    let fromIntrospectionSchema (introspectionSchema : IntrospectionSchema) =
        use stream = new MemoryStream()
        JsonSerializer.Serialize(stream, introspectionSchema, jsonOptions)
        stream.Position <- 0L
        // Note: Creating SHA256 instance per call is acceptable since schema ID computation
        // happens infrequently (typically once per schema during validation cache key creation)
        use sha256 = SHA256.Create()
        let hash = sha256.ComputeHash stream
        hash
        |> Seq.map formatByteAsLowerHex
        |> String.concat ""

/// An in-memory cache for the results of schema/document validations, with a lifetime of 30 seconds.
type MemoryValidationResultCache () =
    let expirationPolicy = CacheExpirationPolicy.SlidingExpiration(TimeSpan.FromSeconds 30.0)
    let internalCache = MemoryCache<ValidationResultKey, ValidationResult<GQLProblemDetails>>(expirationPolicy)
    interface IValidationResultCache with
        member _.GetOrAdd producer key =
            internalCache.GetOrAddResult key producer
