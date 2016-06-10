/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

//TODO: intermediate representation of object returned from graphql endpoint using introspectionQuery

open System
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Introspection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices

type IntrospectionResult = {
    __schema: IntrospectionSchema
}

module TypeCompiler =

    let genScalar itype = ()
    let genObject itype = ()
    let genInterface itype = ()
    let genInputObject itype = ()
    let genEnum itype = ()
    let genUnion itype = ()

    let genType (itype: IntrospectionType) = 
        match itype.Kind with
        | TypeKind.OBJECT -> genObject itype
        | TypeKind.INPUT_OBJECT -> genInputObject itype
        | TypeKind.SCALAR -> genScalar itype
        | TypeKind.UNION -> genUnion itype
        | TypeKind.ENUM -> genEnum itype
        | TypeKind.INTERFACE -> genInterface itype