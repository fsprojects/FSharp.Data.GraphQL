module FSharp.Data.GraphQL.Tests.ObjectListFilterLinqTests

open Xunit
open System
open System.Linq
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Middleware
open FSharp.Data.GraphQL.Tests.LinqTests

[<Fact>]
let ``ObjectListFilter works with Equals operator``() =
    let filter =  Equals { FieldName = "firstName"; Value = "Jonathan"  } // :> IComparable
    let queryable = data.AsQueryable()
    let filteredData = queryable.Apply(filter) |> Seq.toList
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
    let filteredData = queryable.Apply(filter) |> Seq.toList
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
    let filteredData = queryable.Apply(filter) |> Seq.toList
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
    let filteredData = queryable.Apply(filter) |> Seq.toList
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
    let filteredData = queryable.Apply(filter) |> Seq.toList
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
    let filteredData = queryable.Apply(filter) |> Seq.toList
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
    let filteredData = queryable.Apply(filter) |> Seq.toList
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
    let filteredData = queryable.Apply(filter) |> Seq.toList
    List.length filteredData |> equals 2
    let result = List.head filteredData
    result.ID |> equals 4
    result.FirstName |> equals "Ben"
    result.LastName |> equals "Adams"
    result.Contact  |> equals { Email = "b.adams@gmail.com" }
    result.Friends  |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with IN operator for string type field``() =
    let filter =
        In { FieldName = "firstName"; Value = ["Jeneffer"; "Ben"] }
    let queryable = data.AsQueryable()
    let filteredData = queryable.Apply(filter) |> Seq.toList
    List.length filteredData |> equals 2
    let result = List.head filteredData
    result.ID |> equals 4
    result.FirstName |> equals "Ben"
    result.LastName |> equals "Adams"
    result.Contact  |> equals { Email = "b.adams@gmail.com" }
    result.Friends  |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]
    let result2 = List.last filteredData
    result2.ID |> equals 7
    result2.FirstName |> equals "Jeneffer"
    result2.LastName |> equals "Trif"
    result2.Contact |> equals { Email = "j.trif@gmail.com" }
    result2.Friends |> equals [ { Email = "j.abrams@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with IN operator for int type field``() =
    let filter =
        In { FieldName = "id"; Value = [4; 2; 7] }
    let queryable = data.AsQueryable()
    let filteredData = queryable.Apply(filter) |> Seq.toList
    List.length filteredData |> equals 3
    let result = List.head filteredData
    result.ID |> equals 4
    result.FirstName |> equals "Ben"
    result.LastName |> equals "Adams"
    result.Contact  |> equals { Email = "b.adams@gmail.com" }
    result.Friends  |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with FilterField operator``() =
    let filter =
        FilterField { FieldName = "Friends"; Value = Contains { FieldName = "Email"; Value = "l.trif@gmail.com" } }
    let queryable = data.AsQueryable()
    let filteredData = queryable.Apply(filter) |> Seq.toList
    List.length filteredData |> equals 1
    let result = List.head filteredData
    result.ID |> equals 4
    result.FirstName |> equals "Ben"
    result.LastName |> equals "Adams"
    result.Contact  |> equals { Email = "b.adams@gmail.com" }
    result.Friends  |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with NOT operator``() =
    let filter =
        Not (Equals { FieldName = "lastName"; Value = "Adams" })
    let queryable = data.AsQueryable()
    let filteredData = queryable.Apply(filter) |> Seq.toList
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
    let filteredData = queryable.Apply(filter, options) |> Seq.toList
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
    let filteredData = queryable.Apply(filter, options) |> Seq.toList
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
      Discriminator : string
      __typename : string }

type Horse =
    { ID : int
      Name : string
      Discriminator : string
      __typename : string }

type Hamster =
    { ID : int
      Name : string
      Discriminator : string
      __typename : string }

let animalData =
    [
        { ID = 1; Discriminator="Cow"; Name = "Cow A"; __typename = typeof<Cow>.Name  }
        { ID = 2; Discriminator="Horse"; Name = "Horse B"; __typename = typeof<Horse>.Name }
        { ID = 3; Discriminator="Cow"; Name = "Cow C"; __typename = typeof<Cow>.Name }
        { ID = 4; Discriminator="Horse"; Name = "Horse D"; __typename = typeof<Horse>.Name }
        { ID = 5; Discriminator="Hamster"; Name = "Hamster E"; __typename = typeof<Hamster>.Name }
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
    let filteredData = queryable.Apply(filter, options) |> Seq.toList
    List.length filteredData |> equals 2
    let result1 = List.head filteredData
    match result1 with
    | h ->
        h.ID |> equals 2
        h.Name |> equals "Horse B"
    let result2 = List.last filteredData
    match result2 with
    | h ->
        h.ID |> equals 4
        h.Name |> equals "Horse D"

[<Fact>]
let ``ObjectListFilter works with getDiscriminatorValue startsWith for Horse and Hamster``() =

    let queryable = animalData.AsQueryable()
    let filter = StartsWith { FieldName = "Discriminator"; Value = "H" }
    let options =
        ObjectListFilterLinqOptions (
            (fun entity (discriminator: string) ->
            entity.Discriminator.StartsWith discriminator),
            getDiscriminatorValue = (function
            | t when t = typeof<Cow> -> t.Name
            | t when t = typeof<Horse> -> t.Name
            | t when t = typeof<Hamster> -> t.Name
            | _ -> raise (NotSupportedException "Type not supported"))
        )
    let filteredData = queryable.Apply(filter, options) |> Seq.toList
    List.length filteredData |> equals 3
    let result1 = List.head filteredData
    match result1 with
    | c ->
        c.ID |> equals 2
        c.Name |> equals "Horse B"
    let result2 = List.last filteredData
    match result2 with
    | c ->
        c.ID |> equals 5
        c.Name |> equals "Hamster E"

type Product = {
    Name : string
    Tags : string list
}

let productList =
    [
        { Name = "Product A"; Tags = ["Tag1"; "Tag2"] }
        { Name = "Product B"; Tags = ["Tag2"; "Tag3"] }
        { Name = "Product C"; Tags = ["Tag3"; "Tag4"] }
        { Name = "Product D"; Tags = ["Tag4"; "Tag5"] }
    ]
let productArray = productList.ToArray()

[<Fact>]
let ``ObjectListFilter works with Contains operator collection type properties``() =
    let queryable = productList.AsQueryable()
    let filter = Contains { FieldName = "Tags"; Value = "Tag3" }
    let filteredData = queryable.Apply(filter) |> Seq.toList
    List.length filteredData |> equals 2
    let result1 = List.head filteredData
    result1.Name |> equals "Product B"
    let result2 = List.last filteredData
    result2.Name |> equals "Product C"


[<Fact>]
let ``ObjectListFilter works with Contains operator collection type properties with array``() =
    let queryable = productArray.AsQueryable()
    let filter = Contains { FieldName = "Tags"; Value = "Tag3" }
    let filteredData = queryable.Apply(filter) |> Seq.toList
    List.length filteredData |> equals 2
    let result1 = List.head filteredData
    result1.Name |> equals "Product B"
    let result2 = List.last filteredData
    result2.Name |> equals "Product C"

