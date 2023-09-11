// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic

type GraphQLException(msg) =
    inherit Exception(msg)
    interface IGQLError with
        member _.Message = msg
    interface IGQLErrorExtensions with
        member _.Extensions =
            Dictionary<string, obj> 1
            |> GQLProblemDetails.SetErrorKind Execution
            |> ValueSome
