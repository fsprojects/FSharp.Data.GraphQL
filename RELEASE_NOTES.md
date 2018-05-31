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
* Introduced new package, named FSharp.Data.GraphQL.Server.Middlewares, with generic, built-in middlewares.
* Introduced QueryWeightMiddleware, a generic execution middleware to measure query complexity and optionally block qurey execution with a threshold.
* Introduced ObjectListFilterMiddleware, a generic schema compile middleware to add filter options to list fields of specific objects.
* Changed GQLResponse object to be a Record, which supports a Metadata bag object, that can be used to hold custom information per query request.
* Changed FieldDef interface to be able to support a Metadata bag object, that can be used to hold custom information per field.
* ISchema TypeMap is now a mutable object, to ease schema customization through schema compile middlewares.