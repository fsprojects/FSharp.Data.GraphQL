// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

#if IS_DESIGNTIME

open System
open Microsoft.Extensions.Caching.Memory
open FSharp.Data.GraphQL.Client
open ProviderImplementation.ProvidedTypes
open FSharp.Data.GraphQL.Validation

type internal ProviderKey =
    { IntrospectionLocation : IntrospectionLocation
      CustomHttpHeadersLocation : StringLocation
      UploadInputTypeName : string option
      ResolutionFolder : string
      ClientQueryValidation: bool
      ExplicitOptionalParameters: bool }

module internal ProviderDesignTimeCache =
    let private slidingExpiration = TimeSpan.FromSeconds 30.0
    // The keys are the static arguments of the providers of a project, which the developer writes, so the cache needs no size limit
    let private cache = new MemoryCache (MemoryCacheOptions ())
    let getOrAdd (key : ProviderKey) (defMaker : unit -> ProvidedTypeDefinition) =
        cache.GetOrCreate (
            key,
            fun entry ->
                entry.SlidingExpiration <- Nullable slidingExpiration
                defMaker ()
        )

module internal QueryValidationDesignTimeCache =
    let cache : IValidationResultCache = upcast MemoryValidationResultCache()
    let getOrAdd (key : ValidationResultKey) (resMaker : unit -> ValidationResult<GQLProblemDetails>) =
        cache.GetOrAdd resMaker key

#endif
