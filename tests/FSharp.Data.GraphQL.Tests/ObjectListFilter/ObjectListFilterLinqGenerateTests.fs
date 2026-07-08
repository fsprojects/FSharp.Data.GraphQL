module FSharp.Data.GraphQL.Tests.ObjectListFilterLinqGenerateTests

open Xunit
open System
open System.Numerics
open Microsoft.Azure.Cosmos.Linq
open Microsoft.Azure.Cosmos
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Server.Middleware

[<Struct>]
type ValidStringStruct =
    internal
    | ValidStringStruct of string

    static member op_Explicit (ValidStringStruct str) = str
    static member op_Equality (ValidStringStruct left, ValidStringStruct right) = left = right
    static member op_Inequality (ValidStringStruct left, ValidStringStruct right) = left <> right

    static member internal op_Equality (ValidStringStruct left, right) = left = right
    static member internal op_Inequality (ValidStringStruct left, right) = left <> right
    static member internal op_GreaterThan (ValidStringStruct left, right) = left > right
    static member internal op_GreaterThanOrEqual (ValidStringStruct left, right) = left >= right
    static member internal op_LessThan (ValidStringStruct left, right) = left < right
    static member internal op_LessThanOrEqual (ValidStringStruct left, right) = left <= right
    member str.StartsWith (value : string) = let (ValidStringStruct str) = str in str.StartsWith (value)
    member str.EndsWith (value : string) = let (ValidStringStruct str) = str in str.EndsWith (value)
    member str.Contains (value : string) = let (ValidStringStruct str) = str in str.Contains (value)
    // Just for demo purposes
    interface IEqualityOperators<ValidStringStruct, ValidStringStruct, bool> with
        static member op_Equality (ValidStringStruct left, ValidStringStruct right) = left = right
        static member op_Inequality (ValidStringStruct left, ValidStringStruct right) = left <> right
    interface IComparisonOperators<ValidStringStruct, ValidStringStruct, bool> with
        static member op_GreaterThan (ValidStringStruct left, ValidStringStruct right) = left > right
        static member op_GreaterThanOrEqual (ValidStringStruct left, ValidStringStruct right) = left >= right
        static member op_LessThan (ValidStringStruct left, ValidStringStruct right) = left < right
        static member op_LessThanOrEqual (ValidStringStruct left, ValidStringStruct right) = left <= right

type ValidStringObject =
    internal
    | ValidStringObject of string

    static member op_Explicit (ValidStringObject str) = str
    static member op_Equality (ValidStringObject left, ValidStringObject right) = left = right
    static member op_Inequality (ValidStringObject left, ValidStringObject right) = left <> right

    static member internal op_Equality (ValidStringObject left, right) = left = right
    static member internal op_Inequality (ValidStringObject left, right) = left <> right
    static member internal op_GreaterThan (ValidStringObject left, right) = left > right
    static member internal op_GreaterThanOrEqual (ValidStringObject left, right) = left >= right
    static member internal op_LessThan (ValidStringObject left, right) = left < right
    static member internal op_LessThanOrEqual (ValidStringObject left, right) = left <= right

    // Just for demo purposes
    interface IEqualityOperators<ValidStringObject, ValidStringObject, bool> with
        static member op_Equality (ValidStringObject left, ValidStringObject right) = left = right
        static member op_Inequality (ValidStringObject left, ValidStringObject right) = left <> right
    interface IComparisonOperators<ValidStringObject, ValidStringObject, bool> with
        static member op_GreaterThan (ValidStringObject left, ValidStringObject right) = left > right
        static member op_GreaterThanOrEqual (ValidStringObject left, ValidStringObject right) = left >= right
        static member op_LessThan (ValidStringObject left, ValidStringObject right) = left < right
        static member op_LessThanOrEqual (ValidStringObject left, ValidStringObject right) = left <= right

[<Struct>]
type ValidIntStruct =
    internal
    | ValidIntStruct of Int64

    static member internal op_Equality (ValidIntStruct left, ValidIntStruct right) = left = right
    static member internal op_Inequality (ValidIntStruct left, ValidIntStruct right) = left <> right
    static member internal op_GreaterThan (ValidIntStruct left, right : Int64) = left > right

type ValidIntObject =
    internal
    | ValidIntObject of Int64

    static member internal op_Equality (ValidIntObject left, ValidIntObject right) = left = right
    static member internal op_Inequality (ValidIntObject left, ValidIntObject right) = left <> right
    static member internal op_GreaterThan (ValidIntObject left, right : Int64) = left > right

type FakeEntity = {
    ValueOptionString : string voption
    OptionString : string option
    ValueOptionInt : int voption
    OptionInt : int option
    ValidStringStruct : ValidStringStruct
    ValidStringObject : ValidStringObject
    ValidStringStructList : ValidStringStruct list
    ValidStringObjectList : ValidStringObject list
    string : string
    ValidIntStruct : ValidIntStruct
    ValidIntObject : ValidIntObject
    int : Int64
}

let jsonOptions = Json.getSerializerOptions Seq.empty
let cosmosClient =
    let options = CosmosClientOptions (UseSystemTextJsonSerializerWithOptions = jsonOptions)
    new CosmosClient ("https://localhost:8081/", "C2y6yDjf5/R+ob0N8A7Cgv30VRDJIWEHLM+4QDU5DE2nQ9nDuVTqobD4b8mGGyPMbIZnqyMsEcaGQy67XIw/Jw==", options)
let container = cosmosClient.GetContainer ("database", "container")
let filterOptions = ObjectListFilterLinqOptions<FakeEntity, obj>.None

[<Fact>]
let ``ObjectListFilter works with Equals operator for ValidStringStruct`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = Equals ({ FieldName = "validStringStruct"; Value = "Jonathan" }, null)
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText """SELECT VALUE root FROM root WHERE (root["validStringStruct"] = "Jonathan")"""

[<Fact>]
let ``ObjectListFilter works with not Equals operator for ValidStringStruct`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = Not (Equals ({ FieldName = "validStringStruct"; Value = "Jonathan" }, null))
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText """SELECT VALUE root FROM root WHERE (root["validStringStruct"] != "Jonathan")"""

[<Fact>]
let ``ObjectListFilter works with Equals operator for ValueOptionString`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = Equals ({ FieldName = "valueOptionString"; Value = "Jonathan" }, null)
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText """SELECT VALUE root FROM root WHERE (root["valueOptionString"] = "Jonathan")"""

[<Fact>]
let ``ObjectListFilter works with not Equals operator for ValueOptionString`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = Not (Equals ({ FieldName = "valueOptionString"; Value = "Jonathan" }, null))
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText """SELECT VALUE root FROM root WHERE (root["valueOptionString"] != "Jonathan")"""

[<Fact>]
let ``ObjectListFilter works with Equals operator for null ValueOptionString`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = Equals ({ FieldName = "valueOptionString"; Value = null }, null)
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["valueOptionString"] = null)"""

[<Fact>]
let ``ObjectListFilter works with Equals operator for ValueNone ValueOptionString`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = Equals ({ FieldName = "valueOptionString"; Value = (ValueNone : voption<string>) }, null)
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["valueOptionString"] = null)"""

[<Fact>]
let ``ObjectListFilter works with not Equals operator for ValueNone ValueOptionString`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = Not (Equals ({ FieldName = "valueOptionString"; Value = (ValueNone : voption<string>) }, null))
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["valueOptionString"] != null)"""

[<Fact>]
let ``ObjectListFilter works with Equals operator for OptionString`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = Equals ({ FieldName = "optionString"; Value = "Jonathan" }, null)
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText """SELECT VALUE root FROM root WHERE (root["optionString"] = "Jonathan")"""

[<Fact>]
let ``ObjectListFilter works with not Equals operator for OptionString`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = Not (Equals ({ FieldName = "optionString"; Value = "Jonathan" }, null))
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText """SELECT VALUE root FROM root WHERE (root["optionString"] != "Jonathan")"""

[<Fact>]
let ``ObjectListFilter works with Equals operator for null OptionString`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = Equals ({ FieldName = "optionString"; Value = null }, null)
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["optionString"] = null)"""

[<Fact>]
let ``ObjectListFilter works with not Equals operator for null OptionString`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = Not (Equals ({ FieldName = "optionString"; Value = null }, null))
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["optionString"] = null)"""

[<Fact>]
let ``ObjectListFilter works with StartsWith operator for ValidStringStruct`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = StartsWith ({ FieldName = "validStringStruct"; Value = "J" }, null)
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE STARTSWITH(root["validStringStruct"], "J")"""

[<Fact>]
let ``ObjectListFilter works with StartsWith case insensitive operator for ValidStringStruct`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = StartsWith ({ FieldName = "validStringStruct"; Value = "J" }, StringComparer.OrdinalIgnoreCase)
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE STARTSWITH(root["validStringStruct"], "J", true)"""

[<Fact>]
let ``ObjectListFilter works with EndsWith operator for ValidStringStruct`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = EndsWith ({ FieldName = "validStringStruct"; Value = "n" }, null)
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE ENDSWITH(root["validStringStruct"], "n")"""

[<Fact>]
let ``ObjectListFilter works with EndsWith case insensitive operator for ValidStringStruct`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = EndsWith ({ FieldName = "validStringStruct"; Value = "n" }, StringComparer.OrdinalIgnoreCase)
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE ENDSWITH(root["validStringStruct"], "n", true)"""

[<Fact>]
let ``ObjectListFilter works with Contains operator for ValidStringStruct`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = Contains ({ FieldName = "validStringStruct"; Value = "athan" }, null)
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE CONTAINS(root["validStringStruct"], "athan")"""

[<Fact>]
let ``ObjectListFilter works with Contains case insensitive operator for ValidStringStruct`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = Contains ({ FieldName = "validStringStruct"; Value = "athan" }, StringComparer.OrdinalIgnoreCase)
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE CONTAINS(root["validStringStruct"], "athan", true)"""

[<Fact>]
let ``ObjectListFilter works with Contains operator for ValidStringStruct list`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = Contains ({ FieldName = "validStringStructList"; Value = "athan" }, null)
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE ARRAY_CONTAINS(root["validStringStructList"], "athan")"""

[<Fact>]
let ``ObjectListFilter works with In operator for ValidStringStruct list`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = In { FieldName = "validStringStruct"; Value = [ "athan"; "gaja" ] }
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE ARRAY_CONTAINS([ "athan", "gaja" ], root["validStringStruct"])"""

[<Fact>]
let ``ObjectListFilter works with In operator for empty ValidStringStruct list`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = In { FieldName = "validStringStruct"; Value = [ ] }
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE false"""

[<Fact>]
let ``ObjectListFilter works with Equals operator for ValidStringObject`` () =
    let filter = Equals ({ FieldName = "validStringObject"; Value = ValidStringObject "Jonathan" }, null)
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["validStringObject"] = "Jonathan")"""

[<Fact>]
let ``ObjectListFilter works with not Equals operator for ValidStringObject`` () =
    let filter = Not (Equals ({ FieldName = "validStringObject"; Value = ValidStringObject "Jonathan" }, null))
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["validStringObject"] = "Jonathan")"""

[<Fact>]
let ``ObjectListFilter works with GreaterThan operator for ValidIntStruct`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = GreaterThan { FieldName = "validIntStruct"; Value = 6L }
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["validIntStruct"] > 6)"""

[<Fact>]
let ``ObjectListFilter works with GreaterThan operator for ValidIntObject`` () =
    let filter = GreaterThan { FieldName = "validIntObject"; Value = 6L }
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["validIntObject"] > 6)"""

[<Fact>]
let ``ObjectListFilter works with GreaterThan operator for ValueOptionInt`` () =
    let filter = GreaterThan { FieldName = "valueOptionInt"; Value = 1 }
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["valueOptionInt"] > 1)"""

[<Fact>]
let ``ObjectListFilter works with GreaterThan operator for OptionInt`` () =
    let filter = GreaterThan { FieldName = "optionInt"; Value = 1 }
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["optionInt"] > 1)"""

[<Fact>]
let ``ObjectListFilter works with LessThan operator for ValidIntStruct`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = LessThan { FieldName = "validIntStruct"; Value = 6L }
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["validIntStruct"] < 6)"""

[<Fact>]
let ``ObjectListFilter works with LessThan operator for ValidIntObject`` () =
    let filter = LessThan { FieldName = "validIntObject"; Value = 6L }
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["validIntObject"] < 6)"""

[<Fact>]
let ``ObjectListFilter works with LessThan operator for ValueOptionInt`` () =
    let filter = LessThan { FieldName = "valueOptionInt"; Value = 1 }
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["valueOptionInt"] < 1)"""

[<Fact>]
let ``ObjectListFilter works with LessThan operator for OptionInt`` () =
    let filter = LessThan { FieldName = "optionInt"; Value = 1 }
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["optionInt"] < 1)"""

[<Fact>]
let ``ObjectListFilter works with GreaterThanOrEqual operator for ValidIntStruct`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = GreaterThanOrEqual { FieldName = "validIntStruct"; Value = 6L }
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["validIntStruct"] >= 6)"""

[<Fact>]
let ``ObjectListFilter works with GreaterThanOrEqual operator for ValidIntObject`` () =
    let filter = GreaterThanOrEqual { FieldName = "validIntObject"; Value = 6L }
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["validIntObject"] >= 6)"""

[<Fact>]
let ``ObjectListFilter works with GreaterThanOrEqual operator for ValueOptionInt`` () =
    let filter = GreaterThanOrEqual { FieldName = "valueOptionInt"; Value = 1 }
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["valueOptionInt"] >= 1)"""

[<Fact>]
let ``ObjectListFilter works with GreaterThanOrEqual operator for OptionInt`` () =
    let filter = GreaterThanOrEqual { FieldName = "optionInt"; Value = 1 }
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["optionInt"] >= 1)"""

[<Fact>]
let ``ObjectListFilter works with LessThanOrEqual operator for ValidIntStruct`` () =
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filter = LessThanOrEqual { FieldName = "validIntStruct"; Value = 6L }
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["validIntStruct"] <= 6)"""

[<Fact>]
let ``ObjectListFilter works with LessThanOrEqual operator for ValidIntObject`` () =
    let filter = LessThanOrEqual { FieldName = "validIntObject"; Value = 6L }
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["validIntObject"] <= 6)"""

[<Fact>]
let ``ObjectListFilter works with LessThanOrEqual operator for ValueOptionInt`` () =
    let filter = LessThanOrEqual { FieldName = "valueOptionInt"; Value = 1 }
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["valueOptionInt"] <= 1)"""

[<Fact>]
let ``ObjectListFilter works with LessThanOrEqual operator for OptionInt`` () =
    let filter = LessThanOrEqual { FieldName = "optionInt"; Value = 1 }
    let queryable = container.GetItemLinqQueryable<FakeEntity> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    equals queryDefinition.QueryText, """SELECT VALUE root FROM root WHERE (root["optionInt"] <= 1)"""
