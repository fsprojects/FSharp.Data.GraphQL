/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open FSharp.Data.GraphQL
open System

[<TypeProvider>]
type GraphQLTypeProvider (config) as this =
    inherit TypeProviderForNamespaces(config, addDefaultProbingLocation = true)

    let ns = "FSharp.Data.GraphQL"
    let asm = Assembly.GetExecutingAssembly()
    let cache = new SchemaCache()

    do this.AddNamespace(ns, [ProviderBase.MakeProvidedType(asm, ns, config.ResolutionFolder, cache)])

    member __.Dispose() = cache.Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()

[<assembly:TypeProviderAssembly>] 
do()