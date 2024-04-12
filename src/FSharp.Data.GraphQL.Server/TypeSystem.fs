// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc
[<AutoOpen>]
module FSharp.Data.GraphQL.Types.ResolveFieldContextExtensions

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Extensions

type ResolveFieldContext with

    /// Returns an argument by provided name. If argument was not found a GraphQL exception will be thrown.
    /// <exception cref="GraphQLException">When argument with the name not found in the Args.</exception>
    member x.Arg(name : string) : 't =
        match Map.tryFind name x.Args with
        | Some found -> downcast found
        | None -> raise (GQLMessageException $"Argument '%s{name}' was not provided within context of a field '%s{x.ExecutionInfo.Identifier}'. Check if it was supplied within GraphQL query.")

