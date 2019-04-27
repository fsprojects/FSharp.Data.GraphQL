/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open FSharp.Data.GraphQL.Types.Introspection

type internal GraphQLReply<'T> = {
    Data: 'T
    Errors: string [] option
}

type internal IntrospectionResult = {
    __schema: IntrospectionSchema
}