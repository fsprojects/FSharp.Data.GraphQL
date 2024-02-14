Working with the GraphQL type system
========================

A central point of each GraphQL service is its schema. Schema defines a namespace for type definitions exposed to service consumers, that can be used later to generate things such as documentation, introspection queries and finally to execute and validate incoming GraphQL queries and mutations.

Each schema definition requires to have something called *root query* - it's a GraphQL object definition, which exposes all top level fields to be requested by the user. Aside of it there are also separate roots for mutations (queries are readonly requests) and subscriptions.

GraphQL type system categorizes several custom types, that can be defined by programmer, including:

- [Objects](#defining-an-object)
- [Interfaces](#defining-an-interface)
- [Unions](#defining-a-union)
- [Enums](#defining-an-enum)
- [Scalars](#defining-a-scalar)
- [InputObjects](#defining-an-input-object)

Beside them, FSharp.Data.GraphQL defines two others:

- Lists - which can be used to compose types defined above in context of collections.
- Nullables - which can be used to define potentially absent fields in form of the F# option types. This differs from GraphQL standard, where fields are nullable by default and can be optionally marked as non-null. Such approach however wouldn't fit the spirit of FSharp programming language.

One of the important distintions, that can save your time in the future is difference between **Input** and **Output** type definitions:

1. Output types define valid types that can be produced as GraphQL query response. This includes:
    - Objects
    - Interfaces
    - Unions
    - Enums
    - Scalars
    - Lists
    - Nullables

2. Input types define types, which can be used as valid values included in GraphQL query itself or as set of attached variables. This includes:
    - InputObjects
    - Enums
    - Scalars
    - Lists
    - Nullables

Please, note the distinction between those two - **not all type definitions are both valid inputs and outputs**.

## Defining an Object

Objects are the most common GraphQL type definitions. They describe a complex value with a set of well-defined fields, i.e:

```fsharp
let Person = Define.Object("Person", [
    Define.Field("lastName", String, fun _ person -> person.LastName)
    Define.AsyncField("picture", Uri, fun _ person -> getPictureUrl(person.Id)) ])
```

You can enhance most of the GraphQL components with description (this includes both object and fields definitions) - it later can be queried and used by tools like [graphiql](https://github.com/graphql/graphiql) to be used as documentation.

Fields can also be parametrized - by specifying a list of arguments in GraphQL field definition you can use them later to pass runtime parameters, that may differ with each query, even when query structure remains the same. Do define an argument use `Define.Input` helper method:

```fsharp
let pageSize = 20
Define.Field("people", ListOf Person, "Single page", [ Define.Input("page", Int) ], fun ctx db ->
    let page = ctx.Arg "page"
    query {
        for person in db.People do
        sortBy person.Id
        skip (page * pageSize)
        take pageSize
        select person
    } |> Seq.toList) 
```

### Defining recursive type references

Sometimes you may find hard to define a GraphQL types having recursive relationship to each other. Good example of that would be a `Person` object with field `friends` returning list of instances of type `Person` itself. It's hard to do so due to limitations of F# language. However you can still achieve that by using overloaded `Define.Object` method:

```fsharp
let rec Person = Define.Object(name = "Person", fieldsFn = fun () -> [
    // ... some person fields
    Define.Field("friends", ListOf Person, fun _ p -> p.Friends) ])
```

As you may see, we defined Person object definition using *rec* keyword and instead of defining fields as a list and we used a lazily evaluated function instead.

## Defining an Interface 

GraphQL interfaces are so called abstract types (along with unions). This means, that they can be used as part of the query, however query materialization must always be bound to some concrete Object type definition.

Object definitions must explicitly implement interfaces and all of their fields in order to use them: 

```fsharp
let rec Animal = Define.Interface<obj>("Animal", [
    Define.Field("name", String) ])

and Cat = Define.Object<Cat>(
    name = "Cat",
    interfaces = [Animal],
    fields = [
        Define.Field("name", String, fun _ cat -> cat.Name)
        Define.Field("meows", Boolean, fun _ cat -> cat.Meows) ])

and Dog = Define.Object<Dog>(
    name = "Dog",
    interfaces = [Animal],
    fields = [
        Define.Field("name", String, fun _ dog -> dog.Name)
        Define.Field("barks", Boolean, fun _ dog -> dog.Barks) ])
```

What's worth noticing here is that GraphQL interface definitions are not necessarily bound to .NET interfaces. You can create an interface definition (like the one above) which will be known only to GraphQL schema.

Once you've defined a schema, you can retrieve all definitions implementing target interface, by calling `schema.GetPossibleTypes(Animal)`.

## Defining a Union

GraphQL unions are basically enumerations of one of the object definitions. Unlike F# discriminated unions, they work on existing object types, that can be used on their own. This makes them initially heavy to work with.

However with FSharp.Data.GraphQL, it is possible combine existing F# types with discriminated unions under the GraphQL union definition:

```fsharp
type Cat = { Name: string; Meows: bool }
type Dog = { Name: string; Barks: bool }
type Pet = 
    | DogPet of Dog
    | CatPet of Cat

let Cat = Define.Object<Cat>("Cat", [
        Define.Field("name", String, fun _ cat -> cat.Name)
        Define.Field("meows", Boolean, fun _ cat -> cat.Meows) ])

let Dog = Define.Object<Dog>("Dog", [
        Define.Field("name", String, fun _ dog -> dog.Name)
        Define.Field("barks", Boolean, fun _ dog -> dog.Barks) ])

let Pet = Define.Union<Pet>(
    name = "Pet",
    options = [ Cat; Dog ],
    resolveType = function DogPet _ -> upcast Dog | CatPet _ -> upcast Cat,
    resolveValue = function DogPet d -> box d | CatPet c -> upcast c)
```

The example above shows, how you can use `Pet` discriminated union as a proxy to being able to define GraphQL union type and still being able to operate with .NET type system in safe manner.

## Defining an Enum

GraphQL enums can be quite closelly related to C# enums - they define primitive types (used as GraphQL leaf types) that have strictly defined set of possible values, i.e:

```fsharp
type Ord =
    | Gt = 1
    | Eq = 0
    | Lt = -1

let Ord = Define.Enum("Ord", [
    Define.EnumValue("Lesser", Ord.Lt)
    Define.EnumValue("Equal", Ord.Eq)
    Define.EnumValue("Greater", Ord.Greater)
])
```

The major difference from .NET here is that GraphQL expects, that enum values are serialized as **strings**. Therefore, upon serialization, given enum value will be projected using `ToString()` method.

## Defining a Scalar

Just like enums, GraphQL scalars are leaf types, that should be able to be serialized/deserialized into primitive types supported by format of your choice (which will be JSON in most of the cases).

```fsharp
let Guid = Define.Scalar(
    name = "Guid",
    coerceInput = // Returns Result<Guid, IGQLError list>
        (fun value ->
            let destinationType = "GUID"
            function
            // Handle variable value as JsonElement
            | Variable e when e.ValueKind = JsonValueKind.String ->
                let s = e.GetString()
                match Guid.TryParse(s) with
                | true, guid -> Ok guid
                | false, _ -> e.GetDeserializeError destinationType
            | Variable e -> e.GetDeserializeError destinationType
            // Handle inline value as AST discriminated union object
            | InlineConstant (StringValue s) ->
                match Guid.TryParse(s) with
                | true, guid -> Ok guid
                | false, _ -> getParseError destinationType s
            | InlineConstant value -> value.GetCoerceError destinationType),
    coerceValue = // Returns Guid option
        fun v -> match v with | :? Guid g -> Some g | _ -> None
)
```

This examples shows how to create a scalar definition for .NET `Guid` type. It requires two functions to be defined:

1. `coerceInput` function, which will be used to resolve your scalar value from variable or directly from value encoded in GraphQL query string (in this case StringValue is just a part of parsed query AST).
2. `coerceValue` function, which will return `Some` only if the value falls under the scalar's allowed values range, otherwise `None`. Applied while producing the return object graph from the resolver's output.

## Defining an Input Object

Just like objects, input objects describe a complex data types - however while Objects are designed to work as **output** types, Input Objects are **input** types only.

```fsharp
type CreateAccountData = { Email: string; Password: string }
let CreateAccountDataType = Define.InputObject("CreateAccountData", [
    Define.Input("email", String)
    Define.Input("password", String)
])
```

Unlike the objects, you neither define input object field resolver nor provide any arguments for it. They also don't work together with abstract types like interfaces or unions.

Validate individual properties at scalar definition level, but use validator to validate the whole input object or several dependant properties like this:

```fsharp
type InputAddress = { Country : string; ZipCode : string; City : string }
let InputAddressType =
    Define.InputObject<InputAddress> ("InputAddress", [
            Define.Input ("country", StringType)
            Define.Input ("zipCode", StringType)
            Define.Input ("city", StringType)
        ],
        fun inputAddress ->
            match inputAddress.Country with
            | "US" when inputAddress.ZipCode.Length <> 5 -> ValidationError <| createSingleError "ZipCode must be 5 characters for US"
            | "US" -> Success
            | _ -> ValidationError <| createSingleError "Unsupported country"
    )
```
