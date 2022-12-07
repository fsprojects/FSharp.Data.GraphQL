// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

#if IS_DESIGNTIME

open System
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
    let private expiration = CacheExpirationPolicy.SlidingExpiration(TimeSpan.FromSeconds 30.0)
    let private cache = MemoryCache<ProviderKey, ProvidedTypeDefinition>(expiration)
    let getOrAdd (key : ProviderKey) (defMaker : unit -> ProvidedTypeDefinition) =
        cache.GetOrAddResult key defMaker

module internal QueryValidationDesignTimeCache =
    let cache : IValidationResultCache = upcast MemoryValidationResultCache()
    let getOrAdd (key : ValidationResultKey) (resMaker : unit -> ValidationResult<AstError>) =
        cache.GetOrAdd resMaker key

#endif
