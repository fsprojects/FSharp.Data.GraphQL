module FSharp.Data.GraphQL.Tests.NameValueLookupTests

open Helpers
open Xunit
open FSharp.Data.GraphQL.Execution

[<Fact>]
let ``Lookups containing different lists as inner items should not be equal`` () =
    let lookup1 = NameValueLookup.ofList [
        "items", upcast [
            box <| NameValueLookup.ofList [
                "item1", upcast "value1"
                "item2", upcast "value2"
            ]
            upcast NameValueLookup.ofList [
                "item3", upcast "value3"
                "item4", upcast "value4"]
        ]
    ]
    let lookup2 = NameValueLookup.ofList [
        "items", upcast [
            box <| NameValueLookup.ofList [
                "item1", upcast "value1"
                "item2", upcast "value2"
            ]
        ]
    ]
    lookup1 |> notEquals lookup2