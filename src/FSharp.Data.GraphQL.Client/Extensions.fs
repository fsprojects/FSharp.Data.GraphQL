/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System

[<AutoOpen>]
module internal Extensions =
    type String with
        member this.FirstCharUpper() = 
            this.Substring(0, 1).ToUpperInvariant() + this.Substring(1)

        member this.FirstCharLower() =
            this.Substring(0, 1).ToLowerInvariant() + this.Substring(1)