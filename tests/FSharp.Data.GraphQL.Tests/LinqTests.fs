// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.LinqTests

open Xunit
open System
open System.Linq
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Linq
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Server.Middleware
open FSharp.Data.GraphQL.Server.Middleware.ObjectListFilter
open FSharp.Data.GraphQL.Server.Middleware.ObjectListFilterExtensions
open System.Linq.Expressions

type Contact =
    { Email : string }

type Person =
    { ID : int
      FirstName : string
      LastName : string
      Contact : Contact
      Friends : Contact list }

let Contact = Define.Object("Contact", [ Define.Field("email", StringType, fun _ x -> x.Email) ])

let Person =
    Define.Object<Person>("Person",
                          [ Define.Field("id", IDType, fun _ x -> string x.ID)
                            Define.AutoField("firstName", StringType)
                            Define.Field("lastName", StringType, fun _ x -> x.LastName)
                            Define.Field("fullName", StringType, fun _ x -> x.FirstName + " " + x.LastName)
                            Define.Field("contact", Contact, fun _ x -> x.Contact)
                            Define.Field("email", StringType, fun _ x -> x.Contact.Email)
                            Define.Field("friends", ListOf Contact, fun _ x -> x.Friends) ])

let data =
    [ { ID = 4
        FirstName = "Ben"
        LastName = "Adams"
        Contact = { Email = "b.adams@gmail.com" }
        Friends =
            [ { Email = "j.abrams@gmail.com" }
              { Email = "l.trif@gmail.com" } ] }
      { ID = 2
        FirstName = "Jonathan"
        LastName = "Abrams"
        Contact = { Email = "j.abrams@gmail.com" }
        Friends = [] }
      { ID = 7
        FirstName = "Jeneffer"
        LastName = "Trif"
        Contact = { Email = "j.trif@gmail.com" }
        Friends = [ { Email = "j.abrams@gmail.com" } ] } ]


let internal undefined<'t> = Unchecked.defaultof<'t>

let resolveRoot ctx () =
    let info = ctx.ExecutionInfo
    let queryable = data.AsQueryable()
    let result = queryable.Apply(info) |> Seq.toList
    result

let linqArgs =
    [ Define.Input("id", Nullable IDType)
      Define.Input("skip", Nullable IntType)
      Define.Input("take", Nullable IntType)
      Define.Input("orderBy", Nullable StringType)
      Define.Input("orderByDesc", Nullable StringType)
      Define.Input("first", Nullable IntType)
      Define.Input("last", Nullable IntType)
      Define.Input("before", Nullable StringType)
      Define.Input("after", Nullable StringType) ]

let schema =
    Schema(Define.Object("RootQuery",
                         [ Define.Field("people", ListOf Person, "", linqArgs,
                                        fun ctx () ->
                                            let info = ctx.ExecutionInfo
                                            let queryable = data.AsQueryable()
                                            let result = queryable.Apply(info) |> Seq.toList
                                            result) ]))

let schemaProcessor = Executor(schema)

[<Fact>]
let ``LINQ interpreter works with auto-fields``() =
    let plan = schemaProcessor.CreateExecutionPlanOrFail """
    query Example {
        people {
            firstName
        }
    }
    """
    let info = plan.["people"]
    let people = data.AsQueryable().Apply(info) |> Seq.toList
    List.length people |> equals 3
    let result = List.head people
    result.FirstName |> equals "Ben"
    result.LastName |> equals undefined
    result.Contact |> equals undefined
    result.Friends |> equals undefined

[<Fact>]
let ``LINQ interpreter works with fields with defined resolvers``() =
    let plan = schemaProcessor.CreateExecutionPlanOrFail """
    query Example {
        people {
            lastName
        }
    }
    """
    let info = plan.["people"]
    let people = data.AsQueryable().Apply(info) |> Seq.toList
    List.length people |> equals 3
    let result = List.head people
    result.FirstName |> equals undefined
    result.LastName |> equals "Adams"
    result.Contact |> equals undefined
    result.Friends |> equals undefined

[<Fact>]
let ``LINQ interpreter works with fields referring to nested property resolver``() =
    let plan = schemaProcessor.CreateExecutionPlanOrFail """
    query Example {
        people {
            contact { email }
        }
    }
    """
    let info = plan.["people"]
    let people = data.AsQueryable().Apply(info) |> Seq.toList
    List.length people |> equals 3
    let result = List.head people
    result.FirstName |> equals undefined
    result.LastName |> equals undefined
    result.Contact |> equals { Email = "b.adams@gmail.com" }
    result.Friends |> equals undefined

[<Fact>]
let ``LINQ interpreter works with nested collections``() =
    let plan = schemaProcessor.CreateExecutionPlanOrFail """
    query Example {
        people {
            friends { email }
        }
    }
    """
    let info = plan.["people"]
    let people = data.AsQueryable().Apply(info) |> Seq.toList
    List.length people |> equals 3
    let result = List.head people
    result.FirstName |> equals undefined
    result.LastName |> equals undefined
    result.Contact |> equals undefined
    result.Friends |> equals [ { Email = "j.abrams@gmail.com" }
                               { Email = "l.trif@gmail.com" } ]

[<Fact>]
let ``LINQ interpreter works with nested property getters in resolve function``() =
    let plan = schemaProcessor.CreateExecutionPlanOrFail """
    query Example {
        people {
            email
        }
    }
    """
    let info = plan.["people"]
    let people = data.AsQueryable().Apply(info) |> Seq.toList
    List.length people |> equals 3
    let result = List.head people
    result.FirstName |> equals undefined
    result.LastName |> equals undefined
    // this should be resolved, because email resolver is: fun _ x -> x.Contact.Email
    result.Contact |> equals { Email = "b.adams@gmail.com" }
    result.Friends |> equals undefined

[<Fact>]
let ``LINQ interpreter resolves multiple properties from complex resolvers``() =
    let plan = schemaProcessor.CreateExecutionPlanOrFail """
    query Example {
        people {
            fullName
        }
    }
    """
    let info = plan.["people"]
    let people = data.AsQueryable().Apply(info) |> Seq.toList
    List.length people |> equals 3
    let result = List.head people
    // both FirstName and LastName should be resolved, because
    // they are accessed from within fullName function resolver
    result.FirstName |> equals "Ben"
    result.LastName |> equals "Adams"
    result.Contact |> equals undefined
    result.Friends |> equals undefined

[<Fact>]
let ``LINQ interpreter works with id arg``() =
    let plan = schemaProcessor.CreateExecutionPlanOrFail """
    query Example {
        people(id: 2) {
            id
            firstName
        }
    }
    """
    let info = plan.["people"]
    let people = data.AsQueryable().Apply(info) |> Seq.toList
    List.length people |> equals 1
    let result = List.head people
    result.ID |> equals 2
    result.FirstName |> equals "Jonathan"
    result.LastName |> equals undefined
    result.Contact |> equals undefined
    result.Friends |> equals undefined

[<Fact>]
let ``LINQ interpreter works with skip arg``() =
    let plan = schemaProcessor.CreateExecutionPlanOrFail """
    query Example {
        people(skip: 2) {
            id
            firstName
        }
    }
    """
    let info = plan.["people"]
    let people = data.AsQueryable().Apply(info) |> Seq.toList
    List.length people |> equals 1
    let result = List.head people
    result.ID |> equals 7
    result.FirstName |> equals "Jeneffer"
    result.LastName |> equals undefined
    result.Contact |> equals undefined
    result.Friends |> equals undefined

[<Fact>]
let ``LINQ interpreter works with take arg``() =
    let plan = schemaProcessor.CreateExecutionPlanOrFail """
    query Example {
        people(take: 2) {
            id
            firstName
        }
    }
    """
    let info = plan.["people"]
    let people = data.AsQueryable().Apply(info) |> Seq.toList
    List.length people |> equals 2
    let result = people |> List.map (fun p -> (p.ID, p.FirstName))
    result |> equals [ (4, "Ben")
                       (2, "Jonathan") ]

[<Fact>]
let ``LINQ interpreter works with orderBy arg``() =
    let plan = schemaProcessor.CreateExecutionPlanOrFail """
    query Example {
        people(orderBy: "firstName") {
            id
            firstName
        }
    }
    """
    let info = plan.["people"]
    let people = data.AsQueryable().Apply(info) |> Seq.toList
    List.length people |> equals 3
    let result = people |> List.map (fun p -> (p.ID, p.FirstName))
    result |> equals [ (4, "Ben")
                       (7, "Jeneffer")
                       (2, "Jonathan") ]

[<Fact>]
let ``LINQ interpreter works with orderByDesc arg``() =
    let plan = schemaProcessor.CreateExecutionPlanOrFail """
    query Example {
        people(orderByDesc: "firstName") {
            id
            firstName
        }
    }
    """
    let info = plan.["people"]
    let people = data.AsQueryable().Apply(info) |> Seq.toList
    List.length people |> equals 3
    let result = people |> List.map (fun p -> (p.ID, p.FirstName))
    result |> equals [ (2, "Jonathan")
                       (7, "Jeneffer")
                       (4, "Ben") ]

[<Fact>]
let ``ObjectListFilter works with Equals operator``() =
    let filter =  Equals { FieldName = "firstName"; Value = "Jonathan"  } // :> IComparable
    let queryable = data.AsQueryable()
    let filteredData = filter.Apply(queryable) |> Seq.toList
    List.length filteredData |> equals 1
    let result = List.head filteredData
    result.ID |> equals 2
    result.FirstName |> equals "Jonathan"
    result.LastName |> equals "Abrams"
    result.Contact |> equals { Email = "j.abrams@gmail.com" }
    result.Friends |> equals []

[<Fact>]
let ``ObjectListFilter works with GreaterThan operator``() =
    let filter =  GreaterThan { FieldName = "id"; Value = 4  } // :> IComparable
    let queryable = data.AsQueryable()
    let filteredData = filter.Apply(queryable) |> Seq.toList
    List.length filteredData |> equals 1
    let result = List.head filteredData
    result.ID |> equals 7
    result.FirstName |> equals "Jeneffer"
    result.LastName |> equals "Trif"
    result.Contact |> equals { Email = "j.trif@gmail.com" }
    result.Friends |> equals [ { Email = "j.abrams@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with LessThan operator``() =
    let filter =  LessThan { FieldName = "id"; Value = 4  } // :> IComparable
    let queryable = data.AsQueryable()
    let filteredData = filter.Apply(queryable) |> Seq.toList
    List.length filteredData |> equals 1
    let result = List.head filteredData
    result.ID |> equals 2
    result.FirstName |> equals "Jonathan"
    result.LastName |> equals "Abrams"
    result.Contact |> equals { Email = "j.abrams@gmail.com" }
    result.Friends |> equals []

[<Fact>]
let ``ObjectListFilter works with StartsWith operator``() =
    let filter =  StartsWith { FieldName = "firstName"; Value = "J"  }
    let queryable = data.AsQueryable()
    let filteredData = filter.Apply(queryable) |> Seq.toList
    List.length filteredData |> equals 2
    let result = List.head filteredData
    result.ID |> equals 2
    result.FirstName |> equals "Jonathan"
    result.LastName |> equals "Abrams"
    result.Contact |> equals { Email = "j.abrams@gmail.com" }
    result.Friends |> equals []

[<Fact>]
let ``ObjectListFilter works with Contains operator``() =
    let filter =  Contains { FieldName = "firstName"; Value = "en"  }
    let queryable = data.AsQueryable()
    let filteredData = filter.Apply(queryable) |> Seq.toList
    List.length filteredData |> equals 2
    let result = List.head filteredData
    result.ID |> equals 4
    result.FirstName |> equals "Ben"
    result.LastName |> equals "Adams"
    result.Contact  |> equals { Email = "b.adams@gmail.com" }
    result.Friends  |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with EndsWith operator``() =
    let filter =  EndsWith { FieldName = "lastName"; Value = "ams"  }
    let queryable = data.AsQueryable()
    let filteredData = filter.Apply(queryable) |> Seq.toList
    List.length filteredData |> equals 2
    let result = List.head filteredData
    result.ID |> equals 4
    result.FirstName |> equals "Ben"
    result.LastName |> equals "Adams"
    result.Contact  |> equals { Email = "b.adams@gmail.com" }
    result.Friends  |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with AND operator``() =
    let filter =
        And (
            Contains { FieldName = "firstName"; Value = "en" },
            Equals { FieldName = "lastName"; Value = "Adams" }
        )
    let queryable = data.AsQueryable()
    let filteredData = filter.Apply(queryable) |> Seq.toList
    List.length filteredData |> equals 1
    let result = List.head filteredData
    result.ID |> equals 4
    result.FirstName |> equals "Ben"
    result.LastName |> equals "Adams"
    result.Contact  |> equals { Email = "b.adams@gmail.com" }
    result.Friends  |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with OR operator``() =
    let filter =
        Or (
            GreaterThan { FieldName = "id"; Value = 4 },
            Equals { FieldName = "lastName"; Value = "Adams" }
        )
    let queryable = data.AsQueryable()
    let filteredData = filter.Apply(queryable) |> Seq.toList
    List.length filteredData |> equals 2
    let result = List.head filteredData
    result.ID |> equals 4
    result.FirstName |> equals "Ben"
    result.LastName |> equals "Adams"
    result.Contact  |> equals { Email = "b.adams@gmail.com" }
    result.Friends  |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]

//[<Fact>]
//let ``ObjectListFilter works with FilterField operator``() =
//    let filter =
//        FilterField { FieldName = "Friends"; Value = Contains { FieldName = "Email"; Value = "l.trif@gmail.com" } }
//    let queryable = data.AsQueryable()
//    let filteredData = filter.Apply(queryable) |> Seq.toList
//    List.length filteredData |> equals 1
//    let result = List.head filteredData
//    result.ID |> equals 4
//    result.FirstName |> equals "Ben"
//    result.LastName |> equals "Adams"
//    result.Contact  |> equals { Email = "b.adams@gmail.com" }
//    result.Friends  |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with NOT operator``() =
    let filter =
        Not (Equals { FieldName = "lastName"; Value = "Adams" })
    let queryable = data.AsQueryable()
    let filteredData = filter.Apply(queryable) |> Seq.toList
    List.length filteredData |> equals 2
    let result1 = List.head filteredData
    result1.ID |> equals 2
    result1.FirstName |> equals "Jonathan"
    result1.LastName |> equals "Abrams"
    result1.Contact |> equals { Email = "j.abrams@gmail.com" }
    result1.Friends |> equals []

type Complex =
    { ID : int
      Name : string
      Discriminator : string }

type Building =
    { ID : int
      Name : string
      Discriminator : string }

type Community =
    { ID : int
      Name : string
      Discriminator : string
      Complexes : int list
      Buildings : int list }

type Property =
    | Complex of Complex
    | Building of Building
    | Community of Community


[<Fact>]
let ``ObjectListFilter works with getDiscriminator for Complex``() =
    let propertyData: Property list =
        [
            Complex { ID = 1; Name = "Complex A"; Discriminator = typeof<Complex>.FullName }
            Building { ID = 2; Name = "Building B"; Discriminator = typeof<Building>.FullName }
            Community { ID = 3; Name = "Community C"; Discriminator = typeof<Community>.FullName; Complexes = [1]; Buildings = [2] }
            Complex { ID = 4; Name = "Complex AA"; Discriminator =  typeof<Complex>.FullName }
            Building { ID = 5; Name = "Building BB"; Discriminator =  typeof<Building>.FullName }
            Community { ID = 6; Name = "Community CC"; Discriminator = typeof<Community>.FullName; Complexes = [4]; Buildings = [5] }
        ]
    let queryable = propertyData.AsQueryable()
    let filter = OfTypes [typeof<Complex>]
    let options =
        ObjectListFilterLinqOptions(
            (function
            | Complex c -> c.Discriminator
            | Building b -> b.Discriminator
            | Community c -> c.Discriminator))
    let filteredData = filter.Apply(queryable,options) |> Seq.toList
    List.length filteredData |> equals 2
    let result1 = List.head filteredData
    match result1 with
    | Complex c ->
        c.ID |> equals 1
        c.Name |> equals "Complex A"
    | _ -> failwith "Expected Complex"
    let result2 = List.last filteredData
    match result2 with
    | Complex c ->
        c.ID |> equals 4
        c.Name |> equals "Complex AA"
    | _ -> failwith "Expected Complex"


[<Fact>]
let ``ObjectListFilter works with getDiscriminator and getDiscriminatorValue for Complex``() =
    let propertyData: Property list =
        [
            Complex { ID = 1; Name = "Complex A"; Discriminator = typeof<Complex>.Name}
            Building { ID = 2; Name = "Building B"; Discriminator = typeof<Building>.Name }
            Community { ID = 3; Name = "Community C"; Discriminator = typeof<Community>.Name; Complexes = [1]; Buildings = [2] }
            Complex { ID = 4; Name = "Complex AA"; Discriminator =  typeof<Complex>.Name }
            Building { ID = 5; Name = "Building BB"; Discriminator =  typeof<Building>.Name }
            Community { ID = 6; Name = "Community CC"; Discriminator = typeof<Community>.Name; Complexes = [4]; Buildings = [5] }
        ]
    let queryable = propertyData.AsQueryable()
    let filter = OfTypes [typeof<Complex>]
    let options =
        ObjectListFilterLinqOptions(
            (function
            | Complex c -> c.Discriminator
            | Building b -> b.Discriminator
            | Community c -> c.Discriminator),
            (function
            | t when t = typeof<Complex> -> "Complex"
            | t when t = typeof<Building> -> "Building"
            | t when t = typeof<Community> -> "Community"
            | _ -> raise (NotSupportedException "Type not supported"))
        )
    let filteredData = filter.Apply(queryable,options) |> Seq.toList
    List.length filteredData |> equals 2
    let result1 = List.head filteredData
    match result1 with
    | Complex c ->
        c.ID |> equals 1
        c.Name |> equals "Complex A"
    | _ -> failwith "Expected Complex"
    let result2 = List.last filteredData
    match result2 with
    | Complex c ->
        c.ID |> equals 4
        c.Name |> equals "Complex AA"
    | _ -> failwith "Expected Complex"

type Cow =
    { ID : int
      Name : string
      __typename : string }

type Horse =
    { ID : int
      Name : string
      __typename : string }

let animalData =
    [
        { ID = 1; Name = "Cow A"; __typename = typeof<Cow>.Name }
        { ID = 2; Name = "Horse B"; __typename = typeof<Horse>.Name }
        { ID = 3; Name = "Cow C"; __typename = typeof<Cow>.Name }
        { ID = 4; Name = "Horse D"; __typename = typeof<Horse>.Name }
    ]

[<Fact>]
let ``ObjectListFilter works with getDiscriminatorValue for Horse``() =
    let queryable = animalData.AsQueryable()
    let filter = OfTypes [typeof<Horse>]
    let options =
        ObjectListFilterLinqOptions(
            getDiscriminatorValue = (function
            | t when t = typeof<Cow> -> t.Name
            | t when t = typeof<Horse> -> t.Name
            | _ -> raise (NotSupportedException "Type not supported"))
    )
    let filteredData = filter.Apply(queryable, options) |> Seq.toList
    List.length filteredData |> equals 2
    let result1 = List.head filteredData
    match result1 with
    | h ->
        h.ID |> equals 2
        h.Name |> equals "Horse B"
    | _ -> failwith "Expected Horse"
    let result2 = List.last filteredData
    match result2 with
    | h ->
        h.ID |> equals 4
        h.Name |> equals "Horse D"
