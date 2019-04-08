namespace FSharp.Data.GraphQL.Client

open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open FSharp.Data.GraphQL

[<TypeProvider>]
type GraphQLTypeProvider (config) as this =
    inherit TypeProviderForNamespaces(config, addDefaultProbingLocation = true)

    let ns = "FSharp.Data.GraphQL"
    let asm = Assembly.GetExecutingAssembly()

    do this.AddNamespace(ns, [ProviderBase.MakeProvidedType(asm, ns, config.ResolutionFolder)])

[<assembly:TypeProviderAssembly>] 
do()