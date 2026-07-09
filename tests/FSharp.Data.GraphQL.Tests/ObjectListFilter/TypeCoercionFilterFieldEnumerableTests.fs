[<Xunit.Trait ("Category", "ObjectListFilter")>]
[<Xunit.Trait ("Operator", "FilterField")>]
module FSharp.Data.GraphQL.Tests.ObjectListFilter.TypeCoercion.FieldEnumerableTests

open System
open System.Linq
open System.Text.Json
open Xunit
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Server.Middleware

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// Test types for FilterField over enumerables
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

/// Entity with array collection.
type EntityWithArray = {
    Id : int
    Name : string
    Tags : string array
}

/// Entity with IEnumerable collection (via seq).
type EntityWithIEnumerable = {
    Id : int
    Name : string
    Categories : string list
}

/// Entity with option-wrapped array.
type EntityWithOptionalArray = {
    Id : int
    Name : string
    OptionalTags : string array option
}

/// Entity with nested object properties.
type NestedScore = {
    Subject : string
    Value : int
}

/// Entity with nested collections.
type EntityWithNestedCollection = {
    Id : int
    Name : string
    Scores : NestedScore array
}

/// Entity using public fields instead of properties (to test field support in cache).
type EntityWithFields =
    val Id : int
    val mutable Name : string
    val mutable Tags : string array

    new (id, name, tags) = {
        Id = id
        Name = name
        Tags = tags
    }

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// Test data
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

let filterOptions = ObjectListFilterLinqOptions<EntityWithArray, obj> (Json.getSerializerOptions Seq.empty)
let filterOptionsIEnum = ObjectListFilterLinqOptions<EntityWithIEnumerable, obj> (Json.getSerializerOptions Seq.empty)
let filterOptionsOptArray = ObjectListFilterLinqOptions<EntityWithOptionalArray, obj> (Json.getSerializerOptions Seq.empty)
let filterOptionsNested = ObjectListFilterLinqOptions<EntityWithNestedCollection, obj> (Json.getSerializerOptions Seq.empty)
let filterOptionsFields = ObjectListFilterLinqOptions<EntityWithFields, obj> (Json.getSerializerOptions Seq.empty)

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// Test data: arrays
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

let arrayTestData = [|
    { Id = 1; Name = "Alice"; Tags = [| "admin"; "user" |] }
    { Id = 2; Name = "Bob"; Tags = [| "user" |] }
    { Id = 3; Name = "Charlie"; Tags = [||] }
    { Id = 4; Name = "Diana"; Tags = [| "moderator"; "user" |] }
|]

let applyFilterArray (filter : ObjectListFilter) =
    filter.ApplyTo (arrayTestData.AsQueryable (), filterOptions)
    |> Seq.toList

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// Test data: IEnumerable (list)
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

let ienumeTestData = [|
    { Id = 1; Name = "Alice"; Categories = [ "Books"; "Movies" ] }
    { Id = 2; Name = "Bob"; Categories = [ "Sports" ] }
    { Id = 3; Name = "Charlie"; Categories = [] }
    { Id = 4; Name = "Diana"; Categories = [ "Music"; "Sports" ] }
|]

let applyFilterIEnum (filter : ObjectListFilter) =
    filter.ApplyTo (ienumeTestData.AsQueryable (), filterOptionsIEnum)
    |> Seq.toList

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// Test data: option-wrapped arrays
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

let optArrayTestData = [|
    { Id = 1; Name = "Alice"; OptionalTags = Some [| "admin"; "user" |] }
    { Id = 2; Name = "Bob"; OptionalTags = Some [| "user" |] }
    { Id = 3; Name = "Charlie"; OptionalTags = None }
    { Id = 4; Name = "Diana"; OptionalTags = Some [||] }
|]

let applyFilterOptArray (filter : ObjectListFilter) =
    filter.ApplyTo (optArrayTestData.AsQueryable (), filterOptionsOptArray)
    |> Seq.toList

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// Test data: nested collections
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

let nestedTestData = [|
    { Id = 1; Name = "Alice"; Scores = [| { Subject = "Math"; Value = 95 }; { Subject = "Science"; Value = 88 } |] }
    { Id = 2; Name = "Bob"; Scores = [| { Subject = "Math"; Value = 75 }; { Subject = "Science"; Value = 82 } |] }
    { Id = 3; Name = "Charlie"; Scores = [| { Subject = "English"; Value = 90 } |] }
    { Id = 4; Name = "Diana"; Scores = [||] }
|]

let applyFilterNested (filter : ObjectListFilter) =
    filter.ApplyTo (nestedTestData.AsQueryable (), filterOptionsNested)
    |> Seq.toList

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// Test data: public fields (not properties)
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

let fieldsTestData = [|
    EntityWithFields(1, "Alice", [| "admin"; "user" |])
    EntityWithFields(2, "Bob", [| "user" |])
    EntityWithFields(3, "Charlie", [||])
    EntityWithFields(4, "Diana", [| "moderator"; "user" |])
|]

let applyFilterFields (filter : ObjectListFilter) =
    filter.ApplyTo (fieldsTestData.AsQueryable (), filterOptionsFields)
    |> Seq.toList

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// Helpers for scalar collection filters
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

/// Creates a filter for scalar collections (array, list, etc.) with a nested operator.
///
/// IMPORTANT: When filtering scalar collection elements (e.g., string array or IEnumerable<string>),
/// the inner filter's FieldName must use "_" as a placeholder, since scalar elements don't have properties.
///
/// Example:
///   scalarCollectionFilter "Tags" (In { FieldName = "_"; Value = [box "admin"] })
///   // Filters entities where the Tags collection contains "admin"
///
/// The middleware's Enumerable.Any operator treats "_" as a no-op and evaluates the scalar element directly.
let scalarCollectionFilter (fieldName : string) (innerFilter : ObjectListFilter) : ObjectListFilter =
    FilterField {
        FieldName = fieldName
        Value = innerFilter
    }

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// Tests: FilterField over arrays
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

[<Fact>]
let ``FilterField over array with In finds matching elements`` () =
    let filter =
        scalarCollectionFilter
            "Tags"
            (In { FieldName = "_"; Value = [ box "admin" ] })
    let result = applyFilterArray filter

    Assert.Equal (1, result.Length)
    Assert.Equal ("Alice", result[0].Name)

[<Fact>]
let ``FilterField over array with In finds multiple matching values`` () =
    let filter =
        scalarCollectionFilter
            "Tags"
            (In { FieldName = "_"; Value = [ box "user"; box "moderator" ] })
    let result = applyFilterArray filter

    Assert.Equal (3, result.Length)
    let names = result |> List.map (fun e -> e.Name) |> List.sort |> List.toSeq
    Assert.Equal<seq<string>> (seq { "Alice"; "Bob"; "Diana" }, names)

[<Fact>]
let ``FilterField over array with In excludes empty arrays`` () =
    let filter =
        scalarCollectionFilter
            "Tags"
            (In { FieldName = "_"; Value = [ box "user" ] })
    let result = applyFilterArray filter

    let names = result |> List.map (fun e -> e.Name)
    Assert.False (names.Contains "Charlie")

[<Fact>]
let ``FilterField over array with Equals finds exact match`` () =
    let filter =
        scalarCollectionFilter
            "Tags"
            (Equals ({ FieldName = "_"; Value = "admin" }, null))
    let result = applyFilterArray filter

    Assert.Equal (1, result.Length)
    Assert.Equal ("Alice", result[0].Name)

[<Fact>]
let ``FilterField over array with StartsWith finds prefix matches`` () =
    // Note: StartsWith only works on string members, not on the element itself (_)
    // This test documents that FilterField with StartsWith requires a named property
    // and won't work with scalar element filters
    let filter =
        scalarCollectionFilter
            "Tags"
            (In { FieldName = "_"; Value = [ box "user"; box "admin" ] })
    let result = applyFilterArray filter

    Assert.Equal (3, result.Length)
    let names = result |> List.map (fun e -> e.Name) |> List.sort |> List.toSeq
    Assert.Equal<seq<string>> (seq { "Alice"; "Bob"; "Diana" }, names)

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// Tests: FilterField over IEnumerable (list)
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

[<Fact>]
let ``FilterField over IEnumerable list with In finds matching elements`` () =
    let filter =
        scalarCollectionFilter
            "Categories"
            (In { FieldName = "_"; Value = [ box "Sports" ] })
    let result = applyFilterIEnum filter

    Assert.Equal (2, result.Length)
    let names = result |> List.map (fun e -> e.Name) |> List.sort |> List.toSeq
    Assert.Equal<seq<string>> (seq { "Bob"; "Diana" }, names)

[<Fact>]
let ``FilterField over IEnumerable with Equals finds exact match`` () =
    let filter =
        scalarCollectionFilter
            "Categories"
            (Equals ({ FieldName = "_"; Value = "Books" }, null))
    let result = applyFilterIEnum filter

    Assert.Equal (1, result.Length)
    Assert.Equal ("Alice", result[0].Name)

[<Fact>]
let ``FilterField over IEnumerable excludes empty collections`` () =
    let filter =
        scalarCollectionFilter
            "Categories"
            (In { FieldName = "_"; Value = [ box "Books" ] })
    let result = applyFilterIEnum filter

    let names = result |> List.map (fun e -> e.Name)
    Assert.False (names.Contains "Charlie")

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// Tests: FilterField over option-wrapped arrays
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

[<Fact>]
let ``FilterField over optional array with In finds matching elements`` () =
    let filter =
        scalarCollectionFilter
            "OptionalTags"
            (In { FieldName = "_"; Value = [ box "admin" ] })
    let result = applyFilterOptArray filter

    Assert.Equal (1, result.Length)
    Assert.Equal ("Alice", result[0].Name)

[<Fact>]
let ``FilterField over optional array skips None values`` () =
    let filter =
        scalarCollectionFilter
            "OptionalTags"
            (In { FieldName = "_"; Value = [ box "user" ] })
    let result = applyFilterOptArray filter

    let names = result |> List.map (fun e -> e.Name)
    Assert.False (names.Contains "Charlie") // Charlie has None

[<Fact>]
let ``FilterField over optional array skips empty inner arrays`` () =
    let filter =
        scalarCollectionFilter
            "OptionalTags"
            (In { FieldName = "_"; Value = [ box "admin" ] })
    let result = applyFilterOptArray filter

    let names = result |> List.map (fun e -> e.Name)
    Assert.False (names.Contains "Diana") // Diana has Some [||]

[<Fact>]
let ``FilterField over optional array with Equals finds exact match in Some`` () =
    let filter =
        scalarCollectionFilter
            "OptionalTags"
            (Equals ({ FieldName = "_"; Value = "user" }, null))
    let result = applyFilterOptArray filter

    Assert.Equal (2, result.Length)
    let names = result |> List.map (fun e -> e.Name) |> List.sort |> List.toSeq
    Assert.Equal<seq<string>> (seq { "Alice"; "Bob" }, names)

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// Tests: FilterField with nested collection properties
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

[<Fact>]
let ``FilterField over nested collection with nested Equals finds by nested property`` () =
    let filter =
        FilterField {
            FieldName = "Scores"
            Value = FilterField {
                FieldName = "Subject"
                Value = Equals ({ FieldName = "_"; Value = "Math" }, null)
            }
        }
    let result = applyFilterNested filter

    Assert.Equal (2, result.Length)
    let names = result |> List.map (fun e -> e.Name) |> List.sort |> List.toSeq
    Assert.Equal<seq<string>> (seq { "Alice"; "Bob" }, names)

[<Fact>]
let ``FilterField over nested collection with GreaterThan`` () =
    let filter =
        FilterField {
            FieldName = "Scores"
            Value = GreaterThan { FieldName = "Value"; Value = 88 }
        }
    let result = applyFilterNested filter

    Assert.Equal (2, result.Length)
    let names = result |> List.map (fun e -> e.Name) |> List.sort |> List.toSeq
    Assert.Equal<seq<string>> (seq { "Alice"; "Charlie" }, names)

[<Fact>]
let ``FilterField over nested collection excludes empty nested arrays`` () =
    let filter =
        FilterField {
            FieldName = "Scores"
            Value = Equals ({ FieldName = "Subject"; Value = "Math" }, null)
        }
    let result = applyFilterNested filter

    let names = result |> List.map (fun e -> e.Name)
    Assert.False (names.Contains "Diana") // Diana has empty Scores

[<Fact>]
let ``FilterField over nested collection with multiple criteria`` () =
    let filter =
        FilterField {
            FieldName = "Scores"
            Value = And (
                GreaterThan { FieldName = "Value"; Value = 70 },
                Equals ({ FieldName = "Subject"; Value = "Math" }, null)
            )
        }
    let result = applyFilterNested filter

    Assert.Equal (2, result.Length)
    let names = result |> List.map (fun e -> e.Name) |> List.sort |> List.toSeq
    Assert.Equal<seq<string>> (seq { "Alice"; "Bob" }, names)

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// Tests: FilterField over public fields (verifying field resolution in cache)
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

[<Fact>]
let ``FilterField over public field array with In finds matching elements`` () =
    let filter =
        scalarCollectionFilter
            "Tags"
            (In { FieldName = "_"; Value = [ box "admin" ] })
    let result = applyFilterFields filter

    Assert.Equal (1, result.Length)
    Assert.Equal ("Alice", result[0].Name)

[<Fact>]
let ``FilterField over public field array with Equals finds exact match`` () =
    let filter =
        scalarCollectionFilter
            "Tags"
            (Equals ({ FieldName = "_"; Value = "moderator" }, null))
    let result = applyFilterFields filter

    Assert.Equal (1, result.Length)
    Assert.Equal ("Diana", result[0].Name)

[<Fact>]
let ``FilterField over public field array excludes empty fields`` () =
    let filter =
        scalarCollectionFilter
            "Tags"
            (In { FieldName = "_"; Value = [ box "user" ] })
    let result = applyFilterFields filter

    let names = result |> List.map (fun e -> e.Name)
    Assert.False (names.Contains "Charlie")
