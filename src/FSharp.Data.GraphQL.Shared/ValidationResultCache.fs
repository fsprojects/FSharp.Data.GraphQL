namespace FSharp.Data.GraphQL.Validation

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types.Introspection
open System

type ValidationResultKey =
    { DocumentId : string
      SchemaId : int }

type ValidationResultProducer =
    unit -> ValidationResult<GQLProblemDetails>

type IValidationResultCache =
    abstract GetOrAdd : ValidationResultProducer -> ValidationResultKey -> ValidationResult<GQLProblemDetails>

module SchemaId =
    /// <summary>
    /// Computes an in-memory schema identifier from an introspection schema instance.
    /// This identifier is valid only within the current process/runtime and is not deterministic across process restarts.
    /// </summary>
    /// <param name="introspectionSchema">The introspection schema.</param>
    /// <returns>The schema hash code for in-process cache keys.</returns>
    [<CompiledName("FromIntrospectionSchema")>]
    let fromIntrospectionSchema (introspectionSchema : IntrospectionSchema) =
        introspectionSchema.GetHashCode()

/// An in-memory cache for the results of schema/document validations, with a lifetime of 30 seconds.
type MemoryValidationResultCache () =
    let expirationPolicy = CacheExpirationPolicy.SlidingExpiration(TimeSpan.FromSeconds 30.0)
    let internalCache = MemoryCache<ValidationResultKey, ValidationResult<GQLProblemDetails>>(expirationPolicy)
    interface IValidationResultCache with
        member _.GetOrAdd producer key =
            internalCache.GetOrAddResult key producer
