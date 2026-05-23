module FSharp.Data.GraphQL.Tests.ObjectListFilterLinqTests

open Xunit
open System
open System.Linq
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Middleware
open FSharp.Data.GraphQL.Tests.LinqTests

[<Fact>]
let ``ObjectListFilter works with Equals operator`` () =
    let filter = Equals { FieldName = "firstName"; Value = "Jonathan" } // :> IComparable
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 1
    let result = List.head filteredData
    result.ID |> equals 2
    result.FirstName |> equals "Jonathan"
    result.LastName |> equals "Abrams"
    result.Contact |> equals { Email = "j.abrams@gmail.com" }
    result.Friends |> equals []

[<Fact>]
let ``ObjectListFilter works with GreaterThan operator`` () =
    let filter = GreaterThan { FieldName = "id"; Value = 4 } // :> IComparable
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 1
    let result = List.head filteredData
    result.ID |> equals 7
    result.FirstName |> equals "Jeneffer"
    result.LastName |> equals "Trif"
    result.Contact |> equals { Email = "j.trif@gmail.com" }
    result.Friends |> equals [ { Email = "j.abrams@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with GreaterThanOrEqual operator`` () =
    let filter = GreaterThanOrEqual { FieldName = "id"; Value = 4 } // :> IComparable
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 2
    do
        let result = List.head filteredData
        result.ID |> equals 4
        result.FirstName |> equals "Ben"
        result.LastName |> equals "Adams"
        result.Contact |> equals { Email = "b.adams@gmail.com" }
        result.Friends |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]
    do
        let result = List.last filteredData
        result.ID |> equals 7
        result.FirstName |> equals "Jeneffer"
        result.LastName |> equals "Trif"
        result.Contact |> equals { Email = "j.trif@gmail.com" }
        result.Friends |> equals [ { Email = "j.abrams@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with LessThan operator`` () =
    let filter = LessThan { FieldName = "id"; Value = 4 } // :> IComparable
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 1
    let result = List.head filteredData
    result.ID |> equals 2
    result.FirstName |> equals "Jonathan"
    result.LastName |> equals "Abrams"
    result.Contact |> equals { Email = "j.abrams@gmail.com" }
    result.Friends |> equals []

[<Fact>]
let ``ObjectListFilter works with LessThanOrEqual operator`` () =
    let filter = LessThanOrEqual { FieldName = "id"; Value = 4 } // :> IComparable
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 2
    do
        let result = List.head filteredData
        result.ID |> equals 4
        result.FirstName |> equals "Ben"
        result.LastName |> equals "Adams"
        result.Contact |> equals { Email = "b.adams@gmail.com" }
        result.Friends |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]
    do
        let result = List.last filteredData
        result.ID |> equals 2
        result.FirstName |> equals "Jonathan"
        result.LastName |> equals "Abrams"
        result.Contact |> equals { Email = "j.abrams@gmail.com" }
        result.Friends |> equals []

[<Fact>]
let ``ObjectListFilter works with StartsWith operator`` () =
    let filter = StartsWith { FieldName = "firstName"; Value = "J" }
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 2
    let result = List.head filteredData
    result.ID |> equals 2
    result.FirstName |> equals "Jonathan"
    result.LastName |> equals "Abrams"
    result.Contact |> equals { Email = "j.abrams@gmail.com" }
    result.Friends |> equals []

[<Fact>]
let ``ObjectListFilter works with Contains operator`` () =
    let filter = Contains { FieldName = "firstName"; Value = "en" }
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 2
    let result = List.head filteredData
    result.ID |> equals 4
    result.FirstName |> equals "Ben"
    result.LastName |> equals "Adams"
    result.Contact |> equals { Email = "b.adams@gmail.com" }
    result.Friends |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with EndsWith operator`` () =
    let filter = EndsWith { FieldName = "lastName"; Value = "ams" }
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 2
    let result = List.head filteredData
    result.ID |> equals 4
    result.FirstName |> equals "Ben"
    result.LastName |> equals "Adams"
    result.Contact |> equals { Email = "b.adams@gmail.com" }
    result.Friends |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with AND operator`` () =
    let filter =
        And (Contains { FieldName = "firstName"; Value = "en" }, Equals { FieldName = "lastName"; Value = "Adams" })
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 1
    let result = List.head filteredData
    result.ID |> equals 4
    result.FirstName |> equals "Ben"
    result.LastName |> equals "Adams"
    result.Contact |> equals { Email = "b.adams@gmail.com" }
    result.Friends |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with OR operator`` () =
    let filter =
        Or (GreaterThan { FieldName = "id"; Value = 4 }, Equals { FieldName = "lastName"; Value = "Adams" })
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 2
    let result = List.head filteredData
    result.ID |> equals 4
    result.FirstName |> equals "Ben"
    result.LastName |> equals "Adams"
    result.Contact |> equals { Email = "b.adams@gmail.com" }
    result.Friends |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with IN operator for string type field`` () =
    let filter = In { FieldName = "firstName"; Value = [ "Jeneffer"; "Ben" ] }
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 2
    do
        let result = List.head filteredData
        result.ID |> equals 4
        result.FirstName |> equals "Ben"
        result.LastName |> equals "Adams"
        result.Contact |> equals { Email = "b.adams@gmail.com" }
        result.Friends |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]
    do
        let result = List.last filteredData
        result.ID |> equals 7
        result.FirstName |> equals "Jeneffer"
        result.LastName |> equals "Trif"
        result.Contact |> equals { Email = "j.trif@gmail.com" }
        result.Friends |> equals [ { Email = "j.abrams@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with IN operator for int type field`` () =
    let filter = In { FieldName = "id"; Value = [ 4; 2; 7 ] }
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 3
    let result = List.head filteredData
    result.ID |> equals 4
    result.FirstName |> equals "Ben"
    result.LastName |> equals "Adams"
    result.Contact |> equals { Email = "b.adams@gmail.com" }
    result.Friends |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with Contains operator for array type field`` () =
    let filter = Contains { FieldName = "friends"; Value = { Email = "j.abrams@gmail.com" } }
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 2
    do
        let result = List.head filteredData
        result.ID |> equals 4
        result.FirstName |> equals "Ben"
        result.LastName |> equals "Adams"
        result.Contact |> equals { Email = "b.adams@gmail.com" }
        result.Friends |> equals [ { Email = "j.abrams@gmail.com" }; { Email = "l.trif@gmail.com" } ]
    do
        let result = List.last filteredData
        result.ID |> equals 7
        result.FirstName |> equals "Jeneffer"
        result.LastName |> equals "Trif"
        result.Contact |> equals { Email = "j.trif@gmail.com" }
        result.Friends |> equals [ { Email = "j.abrams@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with FilterField operator`` () =
    let filter =
        FilterField {
            FieldName = "Contact"
            Value = Contains { FieldName = "Email"; Value = "j.trif@gmail.com" }
        }
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 1
    let result = List.head filteredData
    result.ID |> equals 7
    result.FirstName |> equals "Jeneffer"
    result.LastName |> equals "Trif"
    result.Contact |> equals { Email = "j.trif@gmail.com" }
    result.Friends |> equals [ { Email = "j.abrams@gmail.com" } ]

[<Fact>]
let ``ObjectListFilter works with NOT operator`` () =
    let filter = Not (Equals { FieldName = "lastName"; Value = "Adams" })
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 2
    let result = List.head filteredData
    result.ID |> equals 2
    result.FirstName |> equals "Jonathan"
    result.LastName |> equals "Abrams"
    result.Contact |> equals { Email = "j.abrams@gmail.com" }
    result.Friends |> equals []

type Complex = { ID : int; Name : string; Discriminator : string }

type Building = { ID : int; Name : string; Discriminator : string }

type Community = {
    ID : int
    Name : string
    Discriminator : string
    Complexes : int list
    Buildings : int list
}

type Property =
    | Complex of Complex
    | Building of Building
    | Community of Community


[<Fact>]
let ``ObjectListFilter works with getDiscriminator for Complex`` () =
    let propertyData : Property list = [
        Complex { ID = 1; Name = "Complex A"; Discriminator = typeof<Complex>.FullName }
        Building { ID = 2; Name = "Building B"; Discriminator = typeof<Building>.FullName }
        Community {
            ID = 3
            Name = "Community C"
            Discriminator = typeof<Community>.FullName
            Complexes = [ 1 ]
            Buildings = [ 2 ]
        }
        Complex { ID = 4; Name = "Complex AA"; Discriminator = typeof<Complex>.FullName }
        Building { ID = 5; Name = "Building BB"; Discriminator = typeof<Building>.FullName }
        Community {
            ID = 6
            Name = "Community CC"
            Discriminator = typeof<Community>.FullName
            Complexes = [ 4 ]
            Buildings = [ 5 ]
        }
    ]
    let queryable = propertyData.AsQueryable ()
    let filter = OfTypes [ typeof<Complex> ]
    let options =
        ObjectListFilterLinqOptions (
            (function
            | Complex c -> c.Discriminator
            | Building b -> b.Discriminator
            | Community c -> c.Discriminator)
        )
    let filteredData = queryable.Apply (filter, options) |> Seq.toList
    List.length filteredData |> equals 2
    do
        let result = List.head filteredData
        match result with
        | Complex c ->
            c.ID |> equals 1
            c.Name |> equals "Complex A"
        | _ -> failwith "Expected Complex"
    do
        let result = List.last filteredData
        match result with
        | Complex c ->
            c.ID |> equals 4
            c.Name |> equals "Complex AA"
        | _ -> failwith "Expected Complex"


[<Fact>]
let ``ObjectListFilter works with getDiscriminator and getDiscriminatorValue for Complex`` () =
    let propertyData : Property list = [
        Complex { ID = 1; Name = "Complex A"; Discriminator = typeof<Complex>.Name }
        Building { ID = 2; Name = "Building B"; Discriminator = typeof<Building>.Name }
        Community {
            ID = 3
            Name = "Community C"
            Discriminator = typeof<Community>.Name
            Complexes = [ 1 ]
            Buildings = [ 2 ]
        }
        Complex { ID = 4; Name = "Complex AA"; Discriminator = typeof<Complex>.Name }
        Building { ID = 5; Name = "Building BB"; Discriminator = typeof<Building>.Name }
        Community {
            ID = 6
            Name = "Community CC"
            Discriminator = typeof<Community>.Name
            Complexes = [ 4 ]
            Buildings = [ 5 ]
        }
    ]
    let queryable = propertyData.AsQueryable ()
    let filter = OfTypes [ typeof<Complex> ]
    let options =
        ObjectListFilterLinqOptions (
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
    let filteredData = queryable.Apply (filter, options) |> Seq.toList
    List.length filteredData |> equals 2
    do
        let result = List.head filteredData
        match result with
        | Complex c ->
            c.ID |> equals 1
            c.Name |> equals "Complex A"
        | _ -> failwith "Expected Complex"
    do
        let result = List.last filteredData
        match result with
        | Complex c ->
            c.ID |> equals 4
            c.Name |> equals "Complex AA"
        | _ -> failwith "Expected Complex"

// Dummy types to be used in OfType filter
type Cow = class end
type Horse = class end
type Hamster = class end

type Animal = { ID : int; Name : string; Discriminator : string; __typename : string }

let animalData = [
    { ID = 1; Discriminator = "Cow"; Name = "Cow A"; __typename = "Cow" }
    {
        ID = 2
        Discriminator = "Horse"
        Name = "Horse B"
        __typename = "Horse"
    }
    { ID = 3; Discriminator = "Cow"; Name = "Cow C"; __typename = "Cow" }
    {
        ID = 4
        Discriminator = "Horse"
        Name = "Horse D"
        __typename = "Horse"
    }
    {
        ID = 5
        Discriminator = "Hamster"
        Name = "Hamster E"
        __typename = "Hamster"
    }
]

[<Fact>]
let ``ObjectListFilter works with getDiscriminatorValue for Horse`` () =
    let queryable = animalData.AsQueryable ()
    let filter = OfTypes [ typeof<Horse> ]
    let options =
        ObjectListFilterLinqOptions (
            getDiscriminatorValue =
                (function
                | t when t = typeof<Cow> -> t.Name
                | t when t = typeof<Horse> -> t.Name
                | _ -> raise (NotSupportedException "Type not supported"))
        )
    let filteredData = queryable.Apply (filter, options) |> Seq.toList
    List.length filteredData |> equals 2
    do
        let animal = List.head filteredData
        animal.ID |> equals 2
        animal.Name |> equals "Horse B"
    do
        let animal = List.last filteredData
        animal.ID |> equals 4
        animal.Name |> equals "Horse D"

[<Fact>]
let ``ObjectListFilter works with getDiscriminatorValue startsWith for Horse and Hamster`` () =
    let queryable = animalData.AsQueryable ()
    let filter = StartsWith { FieldName = "Discriminator"; Value = "H" }
    let options =
        ObjectListFilterLinqOptions (
            (fun entity (discriminator : string) -> entity.Discriminator.StartsWith discriminator),
            getDiscriminatorValue =
                (function
                | t when t = typeof<Cow> -> t.Name
                | t when t = typeof<Horse> -> t.Name
                | t when t = typeof<Animal> -> t.Name
                | _ -> raise (NotSupportedException "Type not supported"))
        )
    let filteredData = queryable.Apply (filter, options) |> Seq.toList
    List.length filteredData |> equals 3
    do
        let animal = List.head filteredData
        animal.ID |> equals 2
        animal.Name |> equals "Horse B"
    do
        let animal = List.last filteredData
        animal.ID |> equals 5
        animal.Name |> equals "Hamster E"

type ListTagsProduct = { Name : string; Tags : string list }

[<Fact>]
let ``ObjectListFilter works with Contains operator on list collection properties`` () =
    let productList = [
        { Name = "Product A"; Tags = [ "Tag1"; "Tag2" ] }
        { Name = "Product B"; Tags = [ "Tag2"; "Tag3" ] }
        { Name = "Product C"; Tags = [ "Tag3"; "Tag4" ] }
        { Name = "Product D"; Tags = [ "Tag4"; "Tag5" ] }
    ]
    let queryable = productList.AsQueryable ()
    let filter = Contains { FieldName = "Tags"; Value = "Tag3" }
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 2
    do
        let result = List.head filteredData
        result.Name |> equals "Product B"
    do
        let result = List.last filteredData
        result.Name |> equals "Product C"

type ArrayTagsProduct = { Name : string; Tags : string array }

[<Fact>]
let ``ObjectListFilter works with Contains operator on array collection properties`` () =

    let productArray = [
        { Name = "Product A"; Tags = [| "Tag1"; "Tag2" |] }
        { Name = "Product B"; Tags = [| "Tag2"; "Tag3" |] }
        { Name = "Product C"; Tags = [| "Tag3"; "Tag4" |] }
        { Name = "Product D"; Tags = [| "Tag4"; "Tag5" |] }
    ]
    let queryable = productArray.AsQueryable ()
    let filter = Contains { FieldName = "Tags"; Value = "Tag3" }
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 2
    do
        let result = List.head filteredData
        result.Name |> equals "Product B"
    do
        let result = List.last filteredData
        result.Name |> equals "Product C"

type SetTagsProduct = { Name : string; Tags : string Set }

[<Fact>]
let ``ObjectListFilter works with Contains operator on set collection properties`` () =

    let productArray = [
        { Name = "Product A"; Tags = [| "Tag1"; "Tag2" |] |> Set.ofArray }
        { Name = "Product B"; Tags = [| "Tag2"; "Tag3" |] |> Set.ofArray }
        { Name = "Product C"; Tags = [| "Tag3"; "Tag4" |] |> Set.ofArray }
        { Name = "Product D"; Tags = [| "Tag4"; "Tag5" |] |> Set.ofArray }
    ]
    let queryable = productArray.AsQueryable ()
    let filter = Contains { FieldName = "Tags"; Value = "Tag3" }
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 2
    do
        let result = List.head filteredData
        result.Name |> equals "Product B"
    do
        let result = List.last filteredData
        result.Name |> equals "Product C"

[<Fact>]
let ``ObjectListFilter OfTypes works with two or more types`` () =

    let queryable = animalData.AsQueryable ()
    let filter = OfTypes [ typeof<Cow>; typeof<Horse> ]
    let options =
        ObjectListFilterLinqOptions (
            getDiscriminatorValue =
                (function
                | t when t = typeof<Cow> -> t.Name
                | t when t = typeof<Horse> -> t.Name
                | _ -> raise (NotSupportedException "Type not supported"))
        )
    let filteredData = queryable.Apply (filter, options) |> Seq.toList
    List.length filteredData |> equals 4
    do
        let animal = List.head filteredData
        animal.ID |> equals 1
        animal.Name |> equals "Cow A"
    do
        let animal = List.last filteredData
        animal.ID |> equals 4
        animal.Name |> equals "Horse D"

[<Fact>]
let ``ObjectListFilter works with EqualsCI operator`` () =
    let filter = EqualsCI { FieldName = "firstName"; Value = "jonathan" }
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 1
    let result = List.head filteredData
    result.ID |> equals 2
    result.FirstName |> equals "Jonathan"
    result.LastName |> equals "Abrams"

[<Fact>]
let ``ObjectListFilter works with EqualsCI operator upper case`` () =
    let filter = EqualsCI { FieldName = "firstName"; Value = "JONATHAN" }
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 1
    let result = List.head filteredData
    result.ID |> equals 2
    result.FirstName |> equals "Jonathan"

[<Fact>]
let ``ObjectListFilter works with StartsWithCI operator`` () =
    let filter = StartsWithCI { FieldName = "firstName"; Value = "j" }
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 2
    let result = List.head filteredData
    result.ID |> equals 2
    result.FirstName |> equals "Jonathan"

[<Fact>]
let ``ObjectListFilter works with EndsWithCI operator`` () =
    let filter = EndsWithCI { FieldName = "lastName"; Value = "AMS" }
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 2
    let result = List.head filteredData
    result.ID |> equals 4
    result.LastName |> equals "Adams"
    let result = List.last filteredData
    result.ID |> equals 2
    result.LastName |> equals "Abrams"

[<Fact>]
let ``ObjectListFilter works with ContainsCI operator`` () =
    let filter = ContainsCI { FieldName = "firstName"; Value = "EN" }
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 2
    let result = List.head filteredData
    result.ID |> equals 4
    result.FirstName |> equals "Ben"
    let result = List.last filteredData
    result.ID |> equals 7
    result.FirstName |> equals "Jeneffer"

[<Fact>]
let ``ObjectListFilter case-insensitive operators do not match with case-sensitive filters`` () =
    // Exact case-sensitive StartsWith "j" (lowercase) should match nothing in the data
    let filter = StartsWith { FieldName = "firstName"; Value = "j" }
    let queryable = data.AsQueryable ()
    let filteredData = queryable.Apply (filter) |> Seq.toList
    List.length filteredData |> equals 0
