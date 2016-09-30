LINQ support
========================

## Tracking properties

One of the unique features, `ExecutionInfo` gives to FSharp.Data.GraphQL, is an ability to make more advanced analysis of the requested query ahead of its execution. One of them is an ability to create so called property trees, also known as **Tracker**s.

**Tracker** is a tree of all dependent members (properties or fields) accessed in given execution info resolver expression, as well as its continuations in all nested execution infos, that will be called further. ExecutionInfo resolve object parameter is considered as a tree root in this case.

Keep in mind that tracker is only able to resolve dependent properties within range of the given resolver expression - it won't be able to determine which members were accessed in subsequent method calls.

> WARNING: At the moment of current version (v0.2-beta) this feature is not working with abstract types either.

### Example: using property trees with Entity Framework

With this knowledge about how to construct property trees set in place, we can use them for example for things like optimization of SQL queries.

Think about using GraphQL for performing a database query using Entity Framework as part of the field resolution:

```fsharp
let schema = Schema(Define.Object("RootQuery", [
    Define.Field("people", ListOf Person, fun ctx db ->
        query {
            for person in db.People do
            select person
        } |> Seq.toList)
]))
```

While this is very simplistic query, it could expose potential problem - as we don't know what other entities beside `People` table will be queried as part of GraphQL query at this point, as subsequent objects may define their own fields with other relationships not visible from this function. This may potentially lead to problem of N+1 SELECTs, where related tables - which have not been mentioned in the original database query - could be a subjects of many subsequent queries.

However, given information lying in `ExecutionInfo` - and property `Tracker` tree as its extension - we are able to determine which entity relations will be called before actual execution of the query. Given that knowledge, we can construct a simple logic which will apply `Include("<Relationship>")` method to warn Entity Framework, which tables to prefetch when an actual LINQ query will be called. 

```fsharp
open System.Data.Objects
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Linq

type ObjectQuery<'t> with
    member this.Include(info: ExecutionInfo, ?variables) =
        let path prefix track =
            match prefix, track.Name with
            | None, Some name -> name
            | Some p, Some name -> p + "." + name
            | Some p, None -> p
        let rec incl (source: ObjectQuery<'t>) tracker prefix =
            match tracker with
            | Direct(_,_) -> source
            | Compose(track, _, children) ->
                let newPrefix = (path prefix track)
                let prefixOpt = Some newPrefix
                let partial = source.Include newPrefix
                children |> Set.fold (fun acc child -> incl acc child prefixOpt) partial
        let top = tracker (defaultArg variables Map.empty) info
        let (Compose(_,_, children)) = top
        children |> Set.fold (fun acc child -> incl acc child None) this
```

With that, we can simply modify the code from the beginning, to include all referenced tables - even thou the GraphQL query itself may still be constructed dynamically:

```fsharp
let schema = Schema(Define.Object("RootQuery", [
    Define.Field("people", ListOf Person, fun ctx db ->
        query {
            for person in db.People.Include(ctx.ExecutionInfo, ctx.Variables) do
            select person
        } |> Seq.toList)
]))
```

## LINQ generation

Beside being able to use trackers for custom use, FSharp.Data.GraphQL server offers an ability to apply that information in form of LINQ expression on top of an existing `IQueryable<>` collection. Such application is able to create subsequent selects if form of `A` &rArr; `A'{ list of field acessed from A }`, where it takes only those fields, which will be used during query execution. 

What it also does is free interpretation of field parameters into their LINQ "equivalent". This includes:

- `skip`, `take`, `orderBy` and `orderByDesc` will be translated into corresponding LINQ equivalent methods.
- `id` will be translated into `.Where(x => x.Id == <id>)`. This requires returned type to provide an Id member used as entity identifier.
- `first`/`after` and `last`/`before` combinations known to the [RelayJS](https://facebook.github.io/relay/) users will be translated into `.OrderBy(x => x.Id).Where(x => x.Id > <after>).Take(<first>)` and `.OrderByDescending(x => x.Id).Wherex(x => x.Id < <before>).Take(<last>)` equivalents. This also requires from queried elements to provide Id member used as entity identifier.

This list can be extended and overriden by custom user implementation.

To better illustrate lets use the example. Consider having following type definition:

```fsharp
type Contact = { Email: string; PhoneNumber: string }
type Person = { FirstName: string; LastName: string; Contacts: Contact list }

let Contact = Define.Object("Contact", [
    Define.Field("phoneNr", String, fun _ c -> c.PhoneNumber)
    Define.Field("email", String, fun _ c -> c.Email) ])

let Person = Define.Object("Person", [
    Define.Field("fullName", String, fun _ p -> p.FirstName + " " + p.LastName)
    Define.Field("contacts", ListOf Contact, fun _ p -> p.Contacts) ])

let schema = Schema(Define.Object("RootQuery", [
    Define.Field("people", ListOf Person, "", 
        [ Define.Input("skip", Int); Define.Input("take", Int) ], 
        fun ctx () ->  peopleList.AsQueryable().Apply(ctx.ExecutionInfo, ctx.Variables) |> Seq.toList) ]))
```

And query such as:

```graphql
{
    people(skip: 5, take: 10) {
        fullName
        contacts {
            email
        }
    }
}
```

Once executed, a LINQ interpreter will produce equivalent similar in form to:

```csharp
peopleList.AsQueryable()
    .Select(fun p -> { 
        Person.FirstName = p.FirstName;
        LastName = p.LastName;
        Contacts = p.Contacts.Select(c => {
            Contact.Email = c.Email;
            PhoneNumber = null;
            }) |> Seq.toList })
    .Skip(5)
    .Take(10)
```