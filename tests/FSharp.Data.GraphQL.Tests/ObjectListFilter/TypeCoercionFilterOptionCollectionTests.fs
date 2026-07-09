[<Xunit.Trait (Tests.TraitType.Category, Tests.TraitName.ObjectListFilter)>]
[<Xunit.Trait (Tests.TraitType.ObjectListFilterOperator, "in")>]
module FSharp.Data.GraphQL.Tests.ObjectListFilter.TypeCoercion.OptionCollectionTests

open System
open System.Linq
open System.Text.Json
open Xunit
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Server.Middleware

// ────────────────────────────────────────────────────────────────────────────────────
// Test types for option collection filters
// ────────────────────────────────────────────────────────────────────────────────────

/// Tags wrapped in an option — tests FilterField with optional collection.
type OptionalTagsEntity = {
    Id : int
    Name : string
    /// Optional list of tags — tests the edge case where the field itself is optional
    OptionalTags : string list option
}

// ────────────────────────────────────────────────────────────────────────────────────
// Test data
// ────────────────────────────────────────────────────────────────────────────────────

let filterOptions =
    ObjectListFilterLinqOptions<OptionalTagsEntity, obj> (Json.getSerializerOptions Seq.empty)

let testData = [|
    { Id = 1; Name = "Alice"; OptionalTags = Some [ "admin"; "user" ] }
    { Id = 2; Name = "Bob"; OptionalTags = Some [ "user" ] }
    { Id = 3; Name = "Charlie"; OptionalTags = None }
    { Id = 4; Name = "Diana"; OptionalTags = Some [] }
|]

let applyFilter (filter : ObjectListFilter) =
    filter.ApplyTo (testData.AsQueryable (), filterOptions)
    |> Seq.toList

// ────────────────────────────────────────────────────────────────────────────────────
// Tests for FilterField with optional collections
// ────────────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``FilterField on optional collection with In operator returns entities where tag matches`` () =
    // Regression test: FilterField on optional collection should unwrap the option
    // and correctly apply the nested filter to the contained collection
    let filter =
        FilterField {
            FieldName = "OptionalTags"
            Value = In { FieldName = "_"; Value = [ box "admin" ] }
        }
    let result = applyFilter filter

    Assert.Equal (1, result.Length)
    Assert.Equal<seq<string>> ([| "Alice" |] :> seq<_>, result |> List.map (fun e -> e.Name) |> List.toSeq)

[<Fact>]
let ``FilterField on optional collection with multiple values`` () =
    let filter =
        FilterField {
            FieldName = "OptionalTags"
            Value = In { FieldName = "_"; Value = [ box "admin"; box "moderator" ] }
        }
    let result = applyFilter filter

    Assert.Equal (1, result.Length)
    Assert.Equal<seq<string>> ([| "Alice" |] :> seq<_>, result |> List.map (fun e -> e.Name) |> List.toSeq)

[<Fact>]
let ``FilterField on optional collection skips None values`` () =
    let filter =
        FilterField {
            FieldName = "OptionalTags"
            Value = In { FieldName = "_"; Value = [ box "user" ] }
        }
    let result = applyFilter filter

    Assert.Equal (2, result.Length)
    Assert.Equal<seq<string>> ([| "Alice"; "Bob" |] :> seq<_>, result |> Seq.map (fun e -> e.Name) |> Seq.sort)

[<Fact>]
let ``FilterField on optional collection with empty list`` () =
    let filter =
        FilterField {
            FieldName = "OptionalTags"
            Value = In { FieldName = "_"; Value = [ box "user" ] }
        }
    let result = applyFilter filter

    let names = result |> List.map (fun e -> e.Name)
    Assert.False (names.Contains "Diana")

[<Fact>]
let ``FilterField on optional collection with Equals operator`` () =
    let filter =
        FilterField {
            FieldName = "OptionalTags"
            Value = Equals ({ FieldName = "_"; Value = "user" }, null)
        }
    let result = applyFilter filter

    Assert.Equal (2, result.Length)
    Assert.Equal<seq<string>> ([| "Alice"; "Bob" |] :> seq<_>, result |> Seq.map (fun e -> e.Name) |> Seq.sort)
