// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System

type GraphQLException(msg) =
    inherit Exception(msg)
    interface IGQLError with
        member _.Message = msg
