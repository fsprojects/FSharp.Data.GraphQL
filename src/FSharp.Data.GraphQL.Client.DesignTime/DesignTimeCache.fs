/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

#if IS_DESIGNTIME

open System
open FSharp.Data.GraphQL.Client
open ProviderImplementation.ProvidedTypes
open FSharp.Data.GraphQL.Validation
open FSharp.Data.GraphQL.Validation.Ast

type internal ProviderKey =
    { IntrospectionLocation : IntrospectionLocation
      CustomHttpHeadersLocation : StringLocation
      UploadInputTypeName : string option
      ResolutionFolder : string
      ClientQueryValidation: bool }

module internal ProviderDesignTimeCache =
    let private expiration = CacheExpirationPolicy.SlidingExpiration(TimeSpan.FromSeconds 30.0)
    let private cache = MemoryCache<ProviderKey, ProvidedTypeDefinition>(expiration)
    let getOrAdd (key : ProviderKey) (defMaker : unit -> ProvidedTypeDefinition) =
        cache.GetOrAddResult key defMaker

module internal QueryValidationDesignTimeCache =
    let private expiration = CacheExpirationPolicy.SlidingExpiration(TimeSpan.FromSeconds 30.0)
    let private cache = MemoryCache<string, ValidationResult<Error>>(expiration)
    let getOrAdd (query : string) (validationResultMaker : unit -> ValidationResult<Error>) =
        let key = query.MD5Hash()
        cache.GetOrAddResult key validationResultMaker

#endif
