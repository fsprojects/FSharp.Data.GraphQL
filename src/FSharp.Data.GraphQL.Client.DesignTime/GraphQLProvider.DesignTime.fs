/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open FSharp.Data.GraphQL

[<TypeProvider>]
type GraphQLTypeProvider (config) as this =
    inherit TypeProviderForNamespaces(config,
                                      assemblyReplacementMap = [
                                          "FSharp.Data.GraphQL.Client.DesignTime", "FSharp.Data.GraphQL.Client"
                                          "FSharp.Data.GraphQL.Shared", "FSharp.Data.GraphQL.Shared"
                                        ],
                                      addDefaultProbingLocation = true)

    let ns = "FSharp.Data.GraphQL"
    let asm = Assembly.GetExecutingAssembly()

    do this.AddNamespace(ns, [Provider.makeProvidedType(asm, ns, config.ResolutionFolder)])

    override this.ResolveAssembly args =
        if args.Name.Contains("FSharp.Data.GraphQL") then
            printfn "ResolveAssembly: %s" args.Name
            config.ReferencedAssemblies
            |> Array.filter(fun x -> x.Contains("FSharp.Data"))
            |> Array.iter (fun x ->  printfn "%s" x)
        base.ResolveAssembly args

[<assembly:TypeProviderAssembly>]
do()