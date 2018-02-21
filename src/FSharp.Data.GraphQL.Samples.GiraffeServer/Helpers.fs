namespace FSharp.Data.GraphQL.Samples.GiraffeServer

[<AutoOpen>]
module Helpers =
    let tee f x =
        f x
        x
