#### 0.0.1-beta - April 19 2016
* Initial release

#### 0.0.2-beta - September 1 2016
* Introduced intermediate representation of GraphQL operation execution.
* Introduced ability to convert GraphQL query to LINQ.
* Replaced current asynchronous API resolver internals.
* Ability to use records as complex input variables.
* Minor bugfixes and performance optimizations.

### 0.0.3-beta - March 22 2018
* Introduced a sample for GraphQL over WebSocket Protocol
* Introduced support for Stream and Defer directives
* Minor bugfixes.

### 0.0.4-beta - May 31 2018
* Introduced support for middlewares in the execution pipeline.
* Introduced new package, named FSharp.Data.GraphQL.Server.Middleware, with generic, built-in middlewares.
* Introduced QueryWeightMiddleware, a generic execution middleware to measure query complexity and optionally block qurey execution with a threshold.
* Introduced ObjectListFilterMiddleware, a generic schema compile middleware to add filter options to list fields of specific objects.
* Changed GQLResponse object to be a Record, which supports a Metadata bag object, that can be used to hold custom information per query request.
* Changed FieldDef interface to be able to support a Metadata bag object, that can be used to hold custom information per field.
* ISchema TypeMap is now a mutable object, to ease schema customization through schema compile middlewares.
* Fixing many minor errors when deferring or streaming results with Union and Enum types (specially when queried using fragments).
* Deferred and streamed error results now are handled.
* Added support for nested deferred and streamed results (currently does have a maximum degree of two nested operations).

### 0.0.4-beta01 - May 31 2018
* Fix package dependency versions

### 0.0.5-beta - August 10 2018
* Upgraded dependencies on Newtonsoft.Json to the latest version.
* Changing subscription field definitions to have a generic output type - the output is not required to be a GraphQL type anymore.
* Implemented experimental support for @live directive, through a subscription system.
* Fixed a bug that caused an exception when ToString function is called on an empty NameValueLookup.
* Added support for asynchronous subscription field definitions.

### 0.0.6-beta - August 10 2018
* Fixes for introspection query
* Use string identifier to publish to subscription

### 0.0.7-beta - September 17 2018
* **Breaking Change:** Async Pub/Sub methods for subscription and live directive handler interfaces
* Add Long Scalar definition

### 0.0.8-beta - October 27 2018
* Add subscription field to intospection schema

### 0.0.8-beta01 - October 28 2018
* Add subscription field to intospection schema

### 0.0.9 - November 2 2018
* Fixed a bug where output def of a subscription field was not reachable for introspection.

### 0.0.10-beta - November 13 2018
* Deferred and Streamed results now return their fields in the direct result as well. The value of those fields will be an empty list or null.
* Live results now send the immediate result in the direct result, instead of the deferred result.

### 0.0.11-beta - November 21 2018
* Deferred and Streamed results fixes for GQL interfaces.

### 0.0.12-beta - November 21 2018
* Deferred and Streamed results fixes for GQL interfaces when at the top-level of query.

### 0.0.16-beta - January 3 2019
* **Breaking Change** The `ISubscriptionProvider` interface now has additional methods to publish to subscriptions by tag.
* **Breaking Change** Renamed interface methods in `ISubscriptionProvider` from XxAsync to AsyncXx to reserve the former for TPL extensions.
* Fixes for stream and defer execution.

### 0.0.17-beta - January 11 2019
* Execute deferred results in parallel.

### 0.0.18-beta - January 16 2019
* Fix dispose of deferred/stream responses.

### 1.0.0-beta - April 27 2019
* **Breaking Change** New type provider api! see samples/client-provider for details.
* Fixes for #216 enum as a variable.
* Upgraded paket and dependencies.

### 1.0.0-beta2 - April 27 2019
* **Breaking Change** GraphQLProvider now makes Nullable graphql inputs optional method arguments instead of options
* **Breaking Change** GraphQLProvider uses the operation name as the generated operation type's names. if no operation name is specified it uses "Operation" + the query string's hash
* Make the type provider cross-targeting
* **Breaking Change** Add postcompile phase to middleware interface

### 1.0.0-beta3 - May 10 2019
* **Breaking Change** GraphQLProvider record types constructors now receives option arguments as optional arguments
* Revision on scalar types parsing - fixed some wrong behaviors such as parsing ints as floats

### 1.0.0-beta4 - May 13 2019
* Fixing an bug when converting variable types to variables json in a query

### 1.0.0-beta5 - May 13 2019
* Fixing an bug when parsing json objects with no fields in serialization

### 1.0.0-beta6 - May 17 2019
* Changing constructors of provided types to have overloads instead of optional parameters (needed because of a limitation of the Type Provider SDK)
* Adding support for field aliases
* Operation result errors and custom data now are provided as their respective types instead of options (`Operation Error []` and `Map<string, obj>`)

### 1.0.0-beta7 - May 17 2019
* Minor bug fixes
* Fixing an error on overload constructor definition

### 1.0.0-beta8 - May 22 2019
* Performance improvements

### 1.0.0-beta9 - May 22 2019
* Fixing dependency issue on Server component

### 1.0.0-beta90 - May 29 2019
* Fixing a wrong cast for System.Uri type
* Adding support for file uploads through [GraphQL Multipart Request Spec](https://github.com/jaydenseric/graphql-multipart-request-spec)
* Changing variable parameters of Run/AsyncRun methods to work as overloaded methods instead of one having optional parameters
* Minor bug fixes

### 1.0.0 - July 4 2019
* Changing the internal client of the Type provider (`System.Http.HttpClient`)
* Fixing several minor bugs of the file upload system in the client provider
* Limiting the upload type of the client provider to be a scalar type (provider fails if it is not)
* If an upload type is specified and it is not used in a query, client provider defaults to standard HTTP protocol instead of the multipart request
* Implementing a validation system for queries based on the [GraphQL Spec](https://graphql.github.io/graphql-spec/June2018/#sec-Validation)
* Making validation mandatory on the server component
* Implemented a cache system for query validation on both server and client components
* Client component can optionally disable query validation
* Updating parser to support unamed operations alongside short handed queries
* Adding null value support for the AST Input Values (it was previously parsed as an Enum Value)
* **Breaking Change** Renaming `FSharp.Data.GraphQL.Server.Middlewares` package to `FSharp.Data.GraphQL.Server.Middleware`

### 1.0.1 - July 5 2019
* Adjusting package dependencies (`FParsec`, `System.Net.Http`, and `FSharp.Data.GraphQL.Server` are not locked anymore)

### 1.0.2 - August 19 2019
* fixed false positive validation errors related to variable usage.

### 1.0.3 - March 3 2020
* Correct printing of queries without names
* Correct printing of ListValue and ObjectValue
* Traverse list values in argument lists
* Added test cases for object inputs for graphql functions


### 1.0.4 - March 22 2020
* TypeProvider now treats custom scalars as strings
* The Guid scalar was mapped to a DateTime
