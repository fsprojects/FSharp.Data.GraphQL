module FSharp.Data.GraphQL.IntegrationTests.Helpers

open Xunit

let normalize (x : string) =
    x.Replace("\r\n", "\n").Split([|'\n'|])
    |> Array.map (fun x -> x.Trim())
    |> Array.reduce (fun x y -> x + "\n" + y)

let equals (expected : 'T) (actual : 'T) =
    Assert.Equal<'T>(expected, actual)

let hasItems (seq : seq<'T>) =
    Assert.True(Seq.length seq > 0)