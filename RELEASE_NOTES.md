#### 0.0.1-beta - 2016-04-19
* Initial release

#### 0.0.2-beta - 2016-09-01
* Introduced intermediate representation of GraphQL operation execution.
* Introduced ability to convert GraphQL query to LINQ.
* Replaced current asynchronous API resolver internals.
* Ability to use records as complex input variables.
* Minor bugfixes and performance optimizations.

### 0.0.3-beta - 2018-03-22
* Introduced a sample for GraphQL over WebSocket Protocol
* Introduced support for Stream and Defer directives
* Minor bugfixes.

### 0.0.4-beta - 2018-05-31
* Introduced support for middlewares in the execution pipeline.
* Introduced new package, named FSharp.Data.GraphQL.Server.Middleware, with generic, built-in middlewares.
* Introduced QueryWeightMiddleware, a generic execution middleware to measure query complexity and optionally block query execution with a threshold.
* Introduced ObjectListFilterMiddleware, a generic schema compile middleware to add filter options to list fields of specific objects.
* Changed GQLResponse object to be a Record, which supports a Metadata bag object, that can be used to hold custom information per query request.
* Changed FieldDef interface to be able to support a Metadata bag object, that can be used to hold custom information per field.
* ISchema TypeMap is now a mutable object, to ease schema customization through schema compile middlewares.
* Fixing many minor errors when deferring or streaming results with Union and Enum types (especially when queried using fragments).
* Deferred and streamed error results now are handled.
* Added support for nested deferred and streamed results (currently does have a maximum degree of two nested operations).

### 0.0.4-beta01 - 2018-05-31
* Fix package dependency versions

### 0.0.5-beta - 2018-08-10
* Upgraded dependencies on Newtonsoft.Json to the latest version.
* Changing subscription field definitions to have a generic output type - the output is not required to be a GraphQL type anymore.
* Implemented experimental support for @live directive, through a subscription system.
* Fixed a bug that caused an exception when ToString function is called on an empty NameValueLookup.
* Added support for asynchronous subscription field definitions.

### 0.0.6-beta - 2018-08-10
* Fixes for introspection query
* Use string identifier to publish to subscription

### 0.0.7-beta - 2018-09-17
* **Breaking Change:** Async Pub/Sub methods for subscription and live directive handler interfaces
* Add Long Scalar definition

### 0.0.8-beta - 2018-10-27
* Add subscription field to introspection schema

### 0.0.8-beta01 - 2018-10-28
* Add subscription field to introspection schema

### 0.0.9 - 2018-11-02
* Fixed a bug where output def of a subscription field was not reachable for introspection.

### 0.0.10-beta - 2018-11-13
* Deferred and Streamed results now return their fields in the direct result as well. The value of those fields will be an empty list or null.
* Live results now send the immediate result in the direct result, instead of the deferred result.

### 0.0.11-beta - 2018-11-21
* Deferred and Streamed results fixes for GQL interfaces.

### 0.0.12-beta - 2018-11-21
* Deferred and Streamed results fixes for GQL interfaces when at the top-level of query.

### 0.0.16-beta - 2019-01-03
* **Breaking Change** The `ISubscriptionProvider` interface now has additional methods to publish to subscriptions by tag.
* **Breaking Change** Renamed interface methods in `ISubscriptionProvider` from XxAsync to AsyncXx to reserve the former for TPL extensions.
* Fixes for stream and defer execution.

### 0.0.17-beta - 2019-01-11
* Execute deferred results in parallel.

### 0.0.18-beta - 2019-01-16
* Fix dispose of deferred/stream responses.

### 1.0.0-beta - 2019-04-27
* **Breaking Change** New type provider API! see samples/client-provider for details.
* Fixes for #216 enum as a variable.
* Upgraded paket and dependencies.

### 1.0.0-beta2 - 2019-04-27
* **Breaking Change** GraphQLProvider now makes Nullable GraphQL inputs optional method arguments instead of options
* **Breaking Change** GraphQLProvider uses the operation name as the generated operation type's names. if no operation name is specified it uses "Operation" + the query string's hash
* Make the type provider cross-targeting
* **Breaking Change** Add postcompile phase to middleware interface

### 1.0.0-beta3 - 2019-05-10
* **Breaking Change** GraphQLProvider record types constructors now receive option arguments as optional arguments
* Revision on scalar types parsing - fixed some wrong behaviors such as parsing ints as floats

### 1.0.0-beta4 - 2019-05-13
* Fixing a bug when converting variable types to variables json in a query

### 1.0.0-beta5 - 2019-05-13
* Fixing a bug when parsing json objects with no fields in serialization

### 1.0.0-beta6 - 2019-05-17
* Changing constructors of provided types to have overloads instead of optional parameters (needed because of a limitation of the Type Provider SDK)
* Adding support for field aliases
* Operation result errors and custom data now are provided as their respective types instead of options (`Operation Error []` and `Map<string, obj>`)

### 1.0.0-beta7 - 2019-05-17
* Minor bug fixes
* Fixing an error on overload constructor definition

### 1.0.0-beta8 - 2019-05-22
* Performance improvements

### 1.0.0-beta9 - 2019-05-22
* Fixing dependency issue on Server component

### 1.0.0-beta90 - 2019-05-29
* Fixing a wrong cast for System.Uri type
* Adding support for file uploads through [GraphQL Multipart Request Spec](https://github.com/jaydenseric/graphql-multipart-request-spec)
* Changing variable parameters of Run/AsyncRun methods to work as overloaded methods instead of one having optional parameters
* Minor bug fixes

### 1.0.0 - 2019-07-04
* Changing the internal client of the Type provider (`System.Net.Http.HttpClient`)
* Fixing several minor bugs of the file upload system in the client provider
* Limiting the upload type of the client provider to be a scalar type (provider fails if it is not)
* If an upload type is specified and it is not used in a query, client provider defaults to standard HTTP protocol instead of the multipart request
* Implementing a validation system for queries based on the [GraphQL Spec](https://graphql.github.io/graphql-spec/June2018/#sec-Validation)
* Making validation mandatory on the server component
* Implemented a cache system for query validation on both server and client components
* Client component can optionally disable query validation
* Updating parser to support unnamed operations alongside shorthand queries
* Adding null value support for the AST Input Values (it was previously parsed as an Enum Value)
* **Breaking Change** Renaming `FSharp.Data.GraphQL.Server.Middlewares` package to `FSharp.Data.GraphQL.Server.Middleware`

### 1.0.1 - 2019-07-05
* Adjusting package dependencies (`FParsec`, `System.Net.Http`, and `FSharp.Data.GraphQL.Server` are not locked anymore)

### 1.0.2 - 2019-08-19
* fixed false positive validation errors related to variable usage.

### 1.0.3 - 2020-03-03
* Correct printing of queries without names
* Correct printing of ListValue and ObjectValue
* Traverse list values in argument lists
* Added test cases for object inputs for GraphQL functions


### 1.0.4 - 2020-03-22
* TypeProvider now treats custom scalars as strings
* The Guid scalar was mapped to a DateTime


### 1.0.5 - 2020-03-23
* Support opening static classes
* Add additional case in client to support failed requests that don't return a path.

### 1.0.6 - 2020-12-15
* TypeProvider accepts IHttpClientFactory as input

### 1.0.7 - 2020-12-30
* Add static TypeProvider parameter `explicitOptionalParameters`.

### 1.0.8 - 2021-04-18
* Remove Desktop build
* Update documentation and build tools
* Upgrade build scripts
* Migrate from netcoreapp2.0 to net5.0 for tests and samples
* Target netstandard2.0 exclusively for Type Providers
* Make FSharp.Data.GraphQL.Shared a NuGet package
* Fix parser bug. Thanks to @njlr

### 2.0.0 - 2024-03-24
* **Breaking Change** Migrated to .NET 6/7 and F# 7
* **Breaking Change** Implemented GraphQL error handling using `IGQLError` interface instead of exceptions
* **Breaking Change** Implemented parsing variables as `JsonElement`
* **Breaking Change** Added `Type` suffix to all built-in GraphQL type definitions
* **New Package** `FSharp.Data.GraphQL.Server.AspNetCore` for ASP.NET Core integration
* Migrated from Paket and FAKE CLI to FAKE build project
* Added `Path` to `ResolveFieldContext`
* Added ability to deprecate fields defined with any combination of parameters (more overloads to the `Define` static class)
* Added support of `Option` variable values for nullable input types
* Added mapping of input query/AST lists to .NET array type in addition to F# list
* Added input object validation with detailed error messages
* Added `Define.WrappedScalar` for value object scalar definitions
* Added coercion of nested input objects
* Added support for `ValueOption` in `AutoField`
* Improved WebSocket handling to eliminate large array allocations
* Fixed nullable enum input handling
* Fixed coercing enum type variables as F# discriminated unions
* Fixed various introspection and default value encoding issues

### 2.1.0 - 2024-04-13
* Improved server exception logging by including exception in log message
* Documentation and README improvements

### 2.2.0 - 2024-05-08
* Improved `AddGraphQLOptions`
* Added Altair and GraphiQL to sample projects

### 2.2.1 - 2024-06-16
* Fixed `JsonSerializerOptions` read-only instance error

### 3.0.0 - 2025-11-30
* **Breaking Change** Migrated to .NET 8 and F# 9.0 with `FSharp.Core` 9.0.x
* **Breaking Change** Updated scalar `CoerceOutput` signature to `objnull -> 'Primitive option`
* **Breaking Change** Moved GraphQL error extensions to standard `extensions` field according to specification
* **Breaking Change** Renamed `ObjectListFilter` type definition to `ObjectListFilterType`
* **Breaking Change** Removed `ObjectListFilter.NoFilter` case
* **Breaking Change** Reworked Relay types to enable async fetching and switched to `ValueOption`
* **Breaking Change** `Giraffe` web framework integration moved to separate package
* **New Package** `FSharp.Data.GraphQL.Server.Giraffe` for Giraffe web framework integration
* **New Package** `FSharp.Data.GraphQL.Server.Oxpecker` for Oxpecker web framework integration
* Removed unused Suave NuGet package
* Added `Oxpecker` web framework support
* Added per-field authorization sample using ASP.NET authorization policies
* Added file upload support via GraphQL Multipart Request Spec with `FileData` type
* Added `OfTypes` filter case for `ObjectListFilter` to filter union cases by type
* Added `ObjectListFilter` value parsing from variables in addition to supported inline value
* Added `ObjectListFilter` value parsing from inline object with variables
* Added `ObjectListFilter` additional operators (`In`, `Contains`, `StartsWith`, `EndsWith`)
* Added `StructNullable` to support `ValueOption` fields in input objects
* Added case-insensitive input object fields and constructor parameters matching
* Added ability to recognize if an input field is null or not present at all
* Added ability to add errors to resolved fields
* Added `FieldDef<'Val, 'Res>` type to support resolver-changing middlewares
* Added GraphQL extensions support via `GQLMessageException`
* Added more overloads to `AddGraphQLOptions` including additional converters
* Added support for optional properties usage in non-Enumerable LINQ queries
* Fixed `ID` type deserialization
* Fixed `ObjectListFilter` `Contains` and `In` operators implementation for collections
* Fixed generation of discriminator comparing expressions to be translatable to database queries
* Fixed `IInputExecutionContext` resolution
* Fixed `graphql-transport-ws` WebSocket implementation
* Fixed ability to override `GraphQLRequestHandler<'Root>`
* Fixed input object CLR property type validation against GraphQL scalar definitions
* Migrated all solutions to SLNX format
* Various performance optimizations and bug fixes

### 3.1.0 - 2026-02-04

* Added `TimeOnly` GraphQL type
* Added `map` function for Relay `Connection` and `Edge` types
* Excluded `GraphQLWebSocketMiddleware` from exception stack trace if request not a Web Socket
* Changed `GraphQLOptionsDefaults.WebSocketConnectionInitTimeoutInMs` const type to `double`
* Improved Relay XML documentation comments

### 3.1.1 - 2026-04-19

* Fixed planning phase crash when inline fragments reference types not included in union or interface definitions
* Fixed GraphQL client provider handling for schema types that reuse reserved scalar names such as `Date`, so introspection kind now takes precedence over built-in scalar mappings.
* Improved Relay XML documentation comments

### 4.0.0 - Unreleased

* **Breaking Change** Migrated to .NET 10