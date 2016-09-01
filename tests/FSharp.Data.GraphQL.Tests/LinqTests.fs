/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.LinqTests

open Xunit
open FsCheck
open System
open System.Linq
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Linq
open FSharp.Data.GraphQL.Execution

type Contact = { Email: string }
type Person = { ID: int; FirstName: string; LastName: string; Contact: Contact; Friends: Contact list }

let Contact = Define.Object("Contact", [
    Define.Field("email", String, fun _ x -> x.Email)])

let Person = Define.Object<Person>("Person", [
    Define.Field("id", ID, fun _ x -> x.ID)
    Define.AutoField("firstName", String)
    Define.Field("lastName", String, fun _ x -> x.LastName)
    Define.Field("contact", Contact, fun _ x -> x.Contact)
    Define.Field("friends", ListOf Contact, fun _ x -> x.Friends) ])

let data = [
    { ID = 4; 
      FirstName = "Ben"; 
      LastName = "Adams"; 
      Contact = { Email = "b.adams@gmail.com" }; 
      Friends = [ 
          { Email = "j.abrams@gmail.com" }; 
          { Email = "l.trif@gmail.com" } ]}
          
    { ID = 2; 
      FirstName = "Jonathan"; 
      LastName = "Abrams"; 
      Contact = { Email = "j.abrams@gmail.com"}; 
      Friends = []}
      
    { ID = 7; 
      FirstName = "Jeneffer"; 
      LastName = "Trif"; 
      Contact = { Email = "j.trif@gmail.com"}; 
      Friends = [ { Email = "j.abrams@gmail.com" } ]}
]

let undefined<'t> = Unchecked.defaultof<'t>

let resolveRoot ctx () =
    let info = ctx.ExecutionInfo
    let queryable = data.AsQueryable()
    let result = info.ToLinq(queryable) |> Seq.toList
    result

let linqArgs = [
    Define.Input("id", ID<int>)
    Define.Input("skip", Int)
    Define.Input("take", Int)
    Define.Input("orderBy", String)
    Define.Input("orderByDesc", String)
    Define.Input("first", Int)
    Define.Input("last", Int)
    Define.Input("before", String)
    Define.Input("after", String) ]

let schema = Schema(Define.Object("RootQuery", [
    Define.Field("people", ListOf Person, "", linqArgs, fun ctx () ->
        let info = ctx.ExecutionInfo
        let queryable = data.AsQueryable()
        let result = info.ToLinq(queryable) |> Seq.toList
        result )        
]))

[<Fact>]
let ``LINQ interpreter works with auto-fields`` () = 
    let plan = schema.CreateExecutionPlan """
    query Example {
        people {
            firstName
        }
    }
    """
    let info = plan.["people"]
    let people = info.ToLinq(data.AsQueryable()) |> Seq.toList
    List.length people |> equals 3
    let result = List.head people
    result.FirstName |> equals "Ben"
    result.LastName |> equals undefined
    result.Contact |> equals undefined 
    result.Friends |> equals undefined

[<Fact>]
let ``LINQ interpreter works with fields with defined resolvers`` () = 
    let plan = schema.CreateExecutionPlan """
    query Example {
        people {
            lastName
        }
    }
    """
    let info = plan.["people"]
    let people = info.ToLinq(data.AsQueryable()) |> Seq.toList
    List.length people |> equals 3
    let result = List.head people
    result.FirstName |> equals undefined
    result.LastName |> equals "Adams"; 
    result.Contact |> equals undefined 
    result.Friends |> equals undefined 

[<Fact>]
let ``LINQ interpreter works with fields referring to nested property resolver`` () = 
    let plan = schema.CreateExecutionPlan """
    query Example {
        people {
            contact { email }
        }
    }
    """
    let info = plan.["people"]
    let people = info.ToLinq(data.AsQueryable()) |> Seq.toList
    List.length people |> equals 3
    let result = List.head people
    result.FirstName |> equals undefined
    result.LastName |> equals undefined 
    result.Contact |> equals { Email = "b.adams@gmail.com" } 
    result.Friends |> equals undefined 

[<Fact>]
let ``LINQ interpreter works with nested collections`` () = 
    let plan = schema.CreateExecutionPlan """
    query Example {
        people {
            friends { email }
        }
    }
    """
    let info = plan.["people"]
    let people = info.ToLinq(data.AsQueryable()) |> Seq.toList
    List.length people |> equals 3
    let result = List.head people
    result.FirstName |> equals undefined
    result.LastName |> equals undefined 
    result.Contact |> equals undefined 
    result.Friends |> equals [
            { Email = "j.abrams@gmail.com" }
            { Email = "l.trif@gmail.com" } ]

[<Fact>]
let ``LINQ interpreter works with id arg`` () = 
    let plan = schema.CreateExecutionPlan """
    query Example {
        people(id: 2) {
            id
            firstName
        }
    }
    """
    let info = plan.["people"]
    let people = info.ToLinq(data.AsQueryable()) |> Seq.toList
    List.length people |> equals 1
    let result = List.head people
    result.ID |> equals 2
    result.FirstName |> equals "Jonathan"
    result.LastName |> equals undefined 
    result.Contact |> equals undefined 
    result.Friends |> equals undefined

[<Fact>]
let ``LINQ interpreter works with skip arg`` () = 
    let plan = schema.CreateExecutionPlan """
    query Example {
        people(skip: 2) {
            id
            firstName
        }
    }
    """
    let info = plan.["people"]
    let people = info.ToLinq(data.AsQueryable()) |> Seq.toList
    List.length people |> equals 1
    let result = List.head people
    result.ID |> equals 7
    result.FirstName |> equals "Jeneffer"
    result.LastName |> equals undefined 
    result.Contact |> equals undefined 
    result.Friends |> equals undefined

[<Fact>]
let ``LINQ interpreter works with take arg`` () = 
    let plan = schema.CreateExecutionPlan """
    query Example {
        people(take: 2) {
            id
            firstName
        }
    }
    """
    let info = plan.["people"]
    let people = info.ToLinq(data.AsQueryable()) |> Seq.toList
    List.length people |> equals 2
    let result = people |> List.map (fun p -> (p.ID, p.FirstName))
    result |> equals [ (4, "Ben"); (2, "Jonathan") ]
    
[<Fact>]
let ``LINQ interpreter works with orderBy arg`` () = 
    let plan = schema.CreateExecutionPlan """
    query Example {
        people(orderBy: "firstName") {
            id
            firstName
        }
    }
    """
    let info = plan.["people"]
    let people = info.ToLinq(data.AsQueryable()) |> Seq.toList
    List.length people |> equals 3
    let result = people |> List.map (fun p -> (p.ID, p.FirstName))
    result |> equals [ (4, "Ben"); (7, "Jeneffer"); (2, "Jonathan") ]
    
[<Fact>]
let ``LINQ interpreter works with orderByDesc arg`` () = 
    let plan = schema.CreateExecutionPlan """
    query Example {
        people(orderByDesc: "firstName") {
            id
            firstName
        }
    }
    """
    let info = plan.["people"]
    let people = info.ToLinq(data.AsQueryable()) |> Seq.toList
    List.length people |> equals 3
    let result = people |> List.map (fun p -> (p.ID, p.FirstName))
    result |> equals [ (2, "Jonathan"); (7, "Jeneffer"); (4, "Ben") ]

[<Fact(Skip="TODO")>]
let ``LINQ interpreter works with first/after args`` () = ()

[<Fact(Skip="TODO")>]
let ``LINQ interpreter works with last/before args`` () = ()

[<Fact(Skip="TODO")>]
let ``LINQ interpreter works with Relay Connection`` () = ()