// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Collections.Immutable
open System.Collections.Generic
open System.Runtime.InteropServices

[<AbstractClass>]
type GraphQLException =
    inherit Exception

    new () = { inherit Exception () }
    new (msg) = { inherit Exception (msg) }
    new (info : Runtime.Serialization.SerializationInfo, context : Runtime.Serialization.StreamingContext) = { inherit Exception (info, context) }

[<AbstractClass>]
type GQLMessageExceptionBase (errorKind, msg, [<Optional>] extensions) =
    inherit GraphQLException (msg)
    interface IGQLError with
        member _.Message = msg
        member this.Exception = Some this
    interface IGQLErrorExtensions with
        member _.Extensions =
            match extensions with
            | null -> Dictionary<string, obj> 1
            | _ -> extensions
            |> GQLProblemDetails.SetErrorKind errorKind
            |> ValueSome

type GQLMessageException (msg) =
    inherit GQLMessageExceptionBase (Execution, msg)

type InvalidInputTypeException (msg, unmatchedOptionalFields) =
    inherit GQLMessageExceptionBase (InputCoercion, msg)

    member _.UnmatchedOptionalFields : string ImmutableHashSet = unmatchedOptionalFields

type MalformedGQLQueryException (msg) =
    inherit GQLMessageExceptionBase (Validation, msg)
