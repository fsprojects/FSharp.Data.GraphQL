/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

module DependencyInjection =
    open System
    open System.Diagnostics
    open System.Collections.Generic
    
    let private cache = Dictionary<string,obj>()

    let inject<'T>(x: 'T) =
        let name = typeof<'T>.FullName
        if cache.ContainsKey name then
            failwithf "Cache already contains dependency: %s" name
        cache.Add(typeof<'T>.FullName, x)
        Debug.WriteLine("Added dependency: {0}", name)

    let resolve<'T>() =
        let name = typeof<'T>.FullName
        try
            cache.[name] :?> 'T
        with
        | _ -> failwithf "Cannot resolve dependency: %s" name

