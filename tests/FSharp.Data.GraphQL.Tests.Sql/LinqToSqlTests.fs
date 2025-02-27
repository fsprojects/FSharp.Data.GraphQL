// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.LinqToSqlTests

open System
open System.Linq
open Xunit
open FSharp.Data.Sql
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Linq

// https://github.com/microsoft/sql-server-samples/blob/master/samples/databases/northwind-pubs/instnwnd.sql
type Db =
    SqlDataProvider<
        Common.DatabaseProviderTypes.MSSQLSERVER,
        "Data Source=(localdb)\MSSQLLocalDB;Initial Catalog=Northwnd;Integrated Security=True;Encrypt=False;"
     >

let OrderType =
    Define.Object<Db.dataContext.``dbo.OrdersEntity``> (
        "Order",
        [
            Define.Field ("orderId", IDType, resolve = (fun _ o -> o.OrderId |> string))
            Define.Field ("shipAddress", StringType, resolve = (fun _ o -> o.ShipAddress))
            Define.Field ("shipCity", StringType, resolve = (fun _ o -> o.ShipCity))
            Define.Field ("shipCountry", StringType, resolve = (fun _ o -> o.ShipCountry))
        ]
    )

let Customer =
    Define.Object<Db.dataContext.``dbo.CustomersEntity``> (
        "Customer",
        [
            Define.Field ("customerID", IDType, resolve = (fun _ c -> c.CustomerId))
            Define.Field ("contactName", StringType, resolve = (fun _ c -> c.ContactName))
            Define.Field ("orders", ListOf OrderType, resolve = (fun _ c -> c.``dbo.Orders by CustomerID``))
        ]
    )

[<Fact>]
let ``LINQ: should create an executable flat SQL query`` () =
    let schema =
        Schema (
            Define.Object (
                "RootQuery",
                [
                    Define.Field (
                        "customers",
                        ListOf Customer,
                        fun ctx (dbContext : Db.dataContext) ->
                            let query = dbContext.Dbo.Customers.Apply (ctx.ExecutionInfo, ctx.Variables)
                            query |> Seq.toList
                    )
                ]
            )
        )
    let query =
        parse
            """query {
        customers {
            contactName
        }
    }"""
    let db = Db.GetDataContext ()
    let executor = Executor<Db.dataContext> (schema)
    let result = sync <| executor.AsyncExecute (query, db)
    ()
