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
    static member internal op_Equality (ValidStringStruct left, ValidStringStruct right) = left = right
    static member internal op_Inequality (ValidStringStruct left, ValidStringStruct right) = left <> right

    static member internal op_Equality (ValidStringStruct left, right) = left = right
    static member internal op_Inequality (ValidStringStruct left, right) = left <> right
    static member internal op_GreaterThan (ValidStringStruct left, right) = left > right
    static member internal op_GreaterThanOrEqual (ValidStringStruct left, right) = left >= right
    static member internal op_LessThan (ValidStringStruct left, right) = left < right
    static member internal op_LessThanOrEqual (ValidStringStruct left, right) = left <= right
        // Just for demo purposes
    interface IEqualityOperators<ValidStringStruct, string, bool> with
        static member op_Equality (ValidStringStruct left, right) = left = right
        static member op_Inequality (ValidStringStruct left, right) = left <> right
    interface IComparisonOperators<ValidStringStruct, string, bool> with
        static member op_GreaterThan (ValidStringStruct left, right) = left > right
        static member op_GreaterThanOrEqual (ValidStringStruct left, right) = left >= right
        static member op_LessThan (ValidStringStruct left, right) = left < right
        static member op_LessThanOrEqual (ValidStringStruct left, right) = left <= right

type ValidStringObject =
    internal
    | ValidStringObject of string
    static member internal op_Equality (ValidStringObject left, ValidStringObject right) = left = right
    static member internal op_Inequality (ValidStringObject left, ValidStringObject right) = left <> right
    static member internal op_Equality (ValidStringObject left, right) = left = right
    static member internal op_Inequality (ValidStringObject left, right) = left <> right
    static member internal op_GreaterThan (ValidStringObject left, right) = left > right
    static member internal op_GreaterThanOrEqual (ValidStringObject left, right) = left >= right
    static member internal op_LessThan (ValidStringObject left, right) = left < right
    static member internal op_LessThanOrEqual (ValidStringObject left, right) = left <= right
        // Just for demo purposes
    interface IEqualityOperators<ValidStringObject, string, bool> with
        static member op_Equality (ValidStringObject left, right) = left = right
        static member op_Inequality (ValidStringObject left, right) = left <> right
    interface IComparisonOperators<ValidStringObject, string, bool> with
        static member op_GreaterThan (ValidStringObject left, right) = left > right
        static member op_GreaterThanOrEqual (ValidStringObject left, right) = left >= right
        static member op_LessThan (ValidStringObject left, right) = left < right
        static member op_LessThanOrEqual (ValidStringObject left, right) = left <= right

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

type Butafor = {
    ValidStringStruct : ValidStringStruct
    ValidStringObject : ValidStringObject
    string : string
    ValidIntStruct : ValidIntStruct
    ValidIntObject : ValidIntObject
    int : Int64
}

let jsonOptions = Json.getSerializerOptions Seq.empty
let cosmosClient =
    let options = CosmosClientOptions(UseSystemTextJsonSerializerWithOptions = jsonOptions)
    new CosmosClient ("https://localhost:8081/", "C2y6yDjf5/R+ob0N8A7Cgv30VRDJIWEHLM+4QDU5DE2nQ9nDuVTqobD4b8mGGyPMbIZnqyMsEcaGQy67XIw/Jw==", options)
let container = cosmosClient.GetContainer("database", "container")
let filterOptions =
    ObjectListFilterLinqOptions<Butafor, obj>.None

[<Fact>]
let ``ObjectListFilter works with Equals operator for ValidStringStruct`` () =
    let queryable = container.GetItemLinqQueryable<Butafor> ()
    let filter = Equals { FieldName = "validStringStruct"; Value = "Jonathan"}
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    ()

[<Fact>]
let ``ObjectListFilter works with Equals operator for ValidStringObject`` () =
    let filter = Equals { FieldName = "validStringObject"; Value = ValidStringObject "Jonathan" }
    let queryable = container.GetItemLinqQueryable<Butafor> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    ()

[<Fact>]
let ``ObjectListFilter works with GreaterThan operator for ValidIntStruct`` () =
    let queryable = container.GetItemLinqQueryable<Butafor> ()
    let filter = GreaterThan { FieldName = "validIntStruct"; Value = 6L }
    let filterQuery = queryable.Apply (filter, filterOptions)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    ()

[<Fact>]
let ``ObjectListFilter works with GreaterThan operator for ValidIntObject`` () =
    let filter = GreaterThan { FieldName = "validIntObject"; Value = 6L }
    let queryable = container.GetItemLinqQueryable<Butafor> ()
    let filterQuery = queryable.Apply (filter)
    let queryDefinition = CosmosLinqExtensions.ToQueryDefinition filterQuery
    ()


