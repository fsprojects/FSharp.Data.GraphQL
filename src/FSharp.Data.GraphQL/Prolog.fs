/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

[<AutoOpen>]
module FSharp.Data.GraphQL.Prolog

open System
open System.Collections.Generic

module Array =
    let distinctBy keyf (array:'T[]) =
            let temp = Array.zeroCreate array.Length
            let mutable i = 0 
            let hashSet = HashSet<_>(HashIdentity.Structural<_>)
            for v in array do
                if hashSet.Add(keyf v) then
                    temp.[i] <- v
                    i <- i + 1
            Array.sub temp 0 i

module Option =
    let toObj value =  match value with None -> null | Some x -> x