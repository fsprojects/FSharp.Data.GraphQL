/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.LinqToSqlTests

open System
open System.Linq
open Xunit
open FSharp.Data.TypeProviders
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Linq

type Db = SqlDataConnection<"Server=.;Database=NORTHWND;Trusted_Connection=True;">

let Order = Define.Object("Order", [
    Define.Field("orderId", Int, fun _ (o: Db.ServiceTypes.Orders) -> o.OrderID)
    Define.Field("shipAddress", String, fun _ o -> o.ShipAddress)
    Define.Field("shipCity", String, fun _ o -> o.ShipCity)
    Define.Field("shipCountry", String, fun _ o -> o.ShipCountry) ])

let Customer = Define.Object("Customer", [
    Define.Field("customerID", ID, fun _ (c: Db.ServiceTypes.Customers) -> c.CustomerID)
    Define.Field("contactName", String, fun _ c -> c.ContactName)
    Define.Field("orders", ListOf Order, fun _ c -> c.Orders) ])
    
[<Fact>]
let ``LINQ: should create an executable flat SQL query`` () =
    let schema = Schema(Define.Object("RootQuery", [
        Define.Field("customers", ListOf Customer, fun ctx (dbContext: Db.ServiceTypes.SimpleDataContextTypes.NORTHWND) ->
            let query = ctx.ExecutionPlan.ToLinq(dbContext.Customers)
            query |> Seq.toList) ]))
    let query = parse """{
        customers {
            contactName
        }   
    }"""
    use db = Db.GetDataContext()
    let result = sync <| schema.AsyncExecute(query, db)
    ()