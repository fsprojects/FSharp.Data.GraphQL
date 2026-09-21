# Type Coercion in ObjectListFilter

## Overview

The `ApplyWithCoercion` extension method automatically converts JSON primitive values (strings, numbers, booleans) to rich CLR types before building LINQ expressions. This prevents type mismatch errors when filtering on properties with types like `Guid`, `DateTime`, `DateOnly`, or F# discriminated unions.

## Basic Usage

### Without Coercion (Old Behavior - Can Fail)
```fsharp
type User = { Id: Guid; Name: string }

// This would fail at runtime - comparing string "550e8400..." with Guid property
let filter = "id" === "550e8400-e29b-41d4-a716-446655440000"
let users = query.Apply filter  // ❌ Type mismatch error
```

### With Coercion (New Feature)
```fsharp
type User = { Id: Guid; Name: string }

// Automatically converts string to Guid before comparison
let filter = "id" === "550e8400-e29b-41d4-a716-446655440000"
let users = query.ApplyWithCoercion filter  // ✅ Works!
```

## Supported Types (Built-in)

- **Guid** - from string via `Guid.Parse`
- **DateTime** - from string via `DateTime.Parse`
- **DateTimeOffset** - from string via `DateTimeOffset.Parse`
- **DateOnly** - from string via `DateOnly.Parse` (.NET 6+)
- **F# Single-Case DUs** - e.g., `type UserId = UserId of Guid`
- **F# Multi-Case Fieldless DUs** - e.g., `type Status = Active | Inactive | Pending`

## F# Discriminated Union Examples

### Single-Case DU (Wrapper Types)
```fsharp
type UserId = UserId of Guid
type User = { Id: UserId; Name: string }

// String is converted to Guid, then wrapped in UserId
let filter = "id" === "550e8400-e29b-41d4-a716-446655440000"
let users = query<User>.ApplyWithCoercion filter  // ✅ Works!
```

### Multi-Case Fieldless DU (Enums)
```fsharp
type Status = Active | Inactive | Pending
type User = { Id: Guid; Status: Status }

// String "active" is case-insensitively matched to Status.Active case
let filter = "status" === "active"
let users = query<User>.ApplyWithCoercion filter  // ✅ Works!

// In operator with multiple values
let filter = "status" =~= ["active"; "pending"]  // Converted to: status == Active OR status == Pending
let users = query<User>.ApplyWithCoercion filter  // ✅ Works!
```

## Custom Type Coercers

For types not supported out-of-the-box (e.g., NodaTime, custom value objects), you can provide custom coercion logic:

### Example: NodaTime.Instant Support

```fsharp
open NodaTime
open NodaTime.Text

// Define a custom coercer
let nodaTimeCoercer : FilterValueCoercer = fun targetType value ->
	if targetType = typeof<Instant> then
		match value with
		| :? string as s ->
			let parsed = InstantPattern.ExtendedIso.Parse s
			if parsed.Success then 
				ValueSome (box parsed.Value)
			else 
				ValueNone
		| _ -> ValueNone
	else 
		ValueNone

// Use it with options
let options = ObjectListFilterLinqOptions<Event, string>([nodaTimeCoercer])
let filter = "eventTime" >>> "2024-01-01T00:00:00Z"
let events = query<Event>.ApplyWithCoercion(filter, options)  // ✅ Works!
```

### Example: Custom Value Object

```fsharp
type EmailAddress = private EmailAddress of string
	with static member TryCreate(s: string) = 
			if s.Contains("@") then Some (EmailAddress s) else None

let emailCoercer : FilterValueCoercer = fun targetType value ->
	if targetType = typeof<EmailAddress> then
		match value with
		| :? string as s ->
			EmailAddress.TryCreate s 
			|> Option.map box 
			|> ValueOption.ofOption
		| _ -> ValueNone
	else 
		ValueNone

let options = ObjectListFilterLinqOptions<User, string>([emailCoercer])
let filter = "email" === "user@example.com"
let users = query<User>.ApplyWithCoercion(filter, options)
```

## Validation Errors

When coercion fails for multi-case DU fields, a descriptive `ObjectListFilterValidationException` is raised:

```fsharp
type Status = Active | Inactive | Pending
type User = { Status: Status }

// Invalid status value
let filter = "status" === "unknown"  
let users = query<User>.ApplyWithCoercion filter  
// ❌ Throws: Invalid value 'unknown' for filter field 'status' of type 'Status'. 
//           Valid values: Active, Inactive, Pending.
```

## Operator Suffix Handling

The middleware appends suffixes like `_eq`, `_gte`, `_starts_with` to field names during parsing. These are automatically stripped before property lookup:

```fsharp
type User = { Age: int }

// Middleware creates: "age_gte" field name
let filter = "age" ===> 18  // >= operator

// Coercion strips "_gte" suffix and finds "Age" property correctly
let users = query<User>.ApplyWithCoercion filter  // ✅ Works!
```

## Backward Compatibility

The original `.Apply()` method remains unchanged. Use `.ApplyWithCoercion()` explicitly when you need automatic type conversion:

```fsharp
// Old code continues to work
let filter = buildFilter()
let results = query.Apply(filter)  // No coercion

// Opt-in to coercion
let results = query.ApplyWithCoercion(filter)  // With coercion
```

## Performance Considerations

Type coercion adds a preprocessing pass over the filter tree. For optimal performance:
- Use `.Apply()` when your filter values already match property types
- Use `.ApplyWithCoercion()` only when needed (e.g., GraphQL input from JSON)
- Custom coercers are called in order until one succeeds - keep the list short

## API Reference

### Extension Methods

```fsharp
type IQueryable<'T> with
	/// Applies filter with automatic type coercion
	member ApplyWithCoercion : 
		filter:ObjectListFilter * 
		[<Optional>] options:ObjectListFilterLinqOptions<'T, 'D> 
		-> IQueryable<'T>

type ObjectListFilter with
	/// Applies filter to query with automatic type coercion  
	member ApplyToWithCoercion :
		query:IQueryable<'T> * 
		[<Optional>] options:ObjectListFilterLinqOptions<'T, 'D> 
		-> IQueryable<'T>
```

### Types

```fsharp
/// Function signature for custom value coercion
type FilterValueCoercer = Type -> obj -> obj voption

/// Options for filter application with custom coercers
type ObjectListFilterLinqOptions<'T, 'D> =
	new : customCoercers:FilterValueCoercer list -> ObjectListFilterLinqOptions<'T, 'D>
	member CustomCoercers : FilterValueCoercer list

/// Exception raised when filter validation fails during coercion
type ObjectListFilterValidationException =
	inherit GQLMessageExceptionBase
	new : message:string * ?extensions:Dictionary<string, obj> -> ObjectListFilterValidationException
```
