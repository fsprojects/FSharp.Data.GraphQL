namespace FSharp.Data.GraphQL.Validation

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types.Introspection
open System
open System.Security.Cryptography
open System.Text
open System.Text.Json

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
    
    /// <summary>
    /// Computes a deterministic schema identifier from an introspection schema.
    /// </summary>
    /// <param name="introspectionSchema">The introspection schema to hash.</param>
    /// <returns>A lowercase hexadecimal SHA-256 hash string that uniquely identifies the schema structure.</returns>
    let fromIntrospectionSchema (introspectionSchema : IntrospectionSchema) =
        let options = JsonSerializerOptions()
        options.WriteIndented <- false
        options.DefaultIgnoreCondition <- System.Text.Json.Serialization.JsonIgnoreCondition.Never
        let json = JsonSerializer.Serialize(introspectionSchema, options)
        let jsonBytes = Encoding.UTF8.GetBytes json
        use sha256 = SHA256.Create()
        let hash = sha256.ComputeHash jsonBytes
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
