// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Collections.Immutable

type InvalidInputTypeException (msg, unmatchedOptionalFields) =
    inherit Exception(msg)

    member _.UnmatchedOptionalFields : string ImmutableHashSet = unmatchedOptionalFields

type MalformedGQLQueryException(msg) =
    inherit GraphQLException(msg)
