namespace FSharp.Data.GraphQL.Validation

open FSharp.Data.GraphQL
open System

type ValidationResultKey =
    { DocumentId : int
      SchemaId : int }

type ValidationResultProducer =
    unit -> ValidationResult<GQLProblemDetails>

type IValidationResultCache =
    abstract GetOrAdd : ValidationResultProducer -> ValidationResultKey -> ValidationResult<GQLProblemDetails>


/// An in-memory cache for the results of schema/document validations, with a lifetime of 30 seconds.
type MemoryValidationResultCache () =
    let expirationPolicy = CacheExpirationPolicy.SlidingExpiration(TimeSpan.FromSeconds 30.0)
    let internalCache = MemoryCache<int, ValidationResult<GQLProblemDetails>>(expirationPolicy)
    interface IValidationResultCache with
        member _.GetOrAdd producer key =
            let internalKey = key.GetHashCode()
            internalCache.GetOrAddResult internalKey producer
