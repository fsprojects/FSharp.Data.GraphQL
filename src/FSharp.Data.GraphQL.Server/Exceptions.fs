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

[<AbstractClass>]
type GQLMessageExceptionBase (errorKind, msg, [<Optional>] extensions) =
    inherit GraphQLException (msg)
    interface IGQLError with
        member _.Message = msg
    interface IGQLExceptionError with
        member this.Exception = this
    interface IGQLErrorExtensions with
        member _.Extensions =
            match extensions with
            | null -> Dictionary<string, obj> 1
            | _ -> extensions
            |> GQLProblemDetails.SetErrorKind errorKind
            |> ValueSome

type GQLMessageException (msg, [<Optional>] extensions) =
    inherit GQLMessageExceptionBase (Execution, msg, extensions)

type InvalidInputTypeException (msg, unmatchedOptionalFields) =
    inherit GQLMessageExceptionBase (InputCoercion, msg)

    member _.UnmatchedOptionalFields : string ImmutableHashSet = unmatchedOptionalFields

type MalformedGQLQueryException (msg) =
    inherit GQLMessageExceptionBase (Validation, msg)
