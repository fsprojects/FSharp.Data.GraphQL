/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types.Introspection

module GraphQLParser =
    let parseQueryResponse (schema : IntrospectionSchema) (json : string) =
        ()