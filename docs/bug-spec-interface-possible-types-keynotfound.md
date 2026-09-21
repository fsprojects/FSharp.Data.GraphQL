# Bug Spec: KeyNotFoundException in Interface Possible Types Resolution

## Summary
Schema introspection may crash with `System.Collections.Generic.KeyNotFoundException` when resolving possible types for an interface with no registered object implementations in the computed implementations map.

## Observed Runtime Evidence
- Exception type: `System.Collections.Generic.KeyNotFoundException`
- Message: `The given key was not present in the dictionary.`
- Top failing library frame: `FSharp.Data.GraphQL.Schema<'Root>.getPossibleTypes`
- Failing code path (`src/FSharp.Data.GraphQL.Server/Schema.fs`):
  - `| Interface i -> Map.find i.Name (implementations.Force()) |> Array.ofList`

## Exact Failure Location
File: `src/FSharp.Data.GraphQL.Server/Schema.fs`
- `getImplementations`: builds `Map<string, ObjectDef list>` from `objdef.Implements`
- `getPossibleTypes`: uses `Map.find` for interfaces
- `introspectType` for `Interface` calls `getPossibleTypes` and crashes before graceful validation/error reporting

## Root Cause
`Map.find` assumes every interface name exists as a key in `implementations` map. This assumption is false when at least one schema interface has zero object implementations in the discovered type map.
In that case, lookup throws immediately, producing infrastructure exception instead of structured GraphQL/type validation feedback.

## Why This Is Problematic
1. Hard crash during schema startup/introspection.
2. No actionable validation message identifying which interface is orphaned.
3. Behavior differs from expected robust validation (should return deterministic `ValidationError` or safe empty set depending on policy).

## Reproduction (Generic, Domain-Agnostic)
1. Define interface `IParentInfo` with at least one field.
2. Register the interface in schema type map.
3. Ensure no object type in type map includes this interface in `interfaces = [ ... ]`.
4. Trigger schema introspection or schema initialization path that builds introspection metadata.
5. Observe `KeyNotFoundException` at `Map.find i.Name (implementations.Force())`.

## Expected Behavior
One of the following (explicitly chosen policy):
- **Preferred**: do not throw; treat no implementations as empty set for possible types, and surface a validation error later if this is invalid by policy.
- **Alternative**: immediately return structured validation error: `Interface <name> has no implementing object types.`

No raw `KeyNotFoundException` should escape from schema construction/introspection.

## Proposed Fix
### Safe Lookup Change
Replace unsafe lookup in `getPossibleTypes` with safe lookup:
- from: `Map.find i.Name (implementations.Force()) |> Array.ofList`
- to: `implementations.Force() |> Map.tryFind i.Name |> Option.defaultValue [] |> Array.ofList`

### Validation Enhancement
Add explicit validation for orphaned interfaces in type-map validation layer:
- detect interfaces with zero implementing object types
- return deterministic `ValidationError` with interface name

This keeps runtime stable and preserves strict schema diagnostics.

## Test Specification
Create dedicated tests in `tests/FSharp.Data.GraphQL.Tests` (new file recommended: `InterfacePossibleTypesValidationTests.fs`).

### Test 1: Regression Repro (pre-fix behavior)
- Build schema with one interface and no implementors.
- Assert old code throws `KeyNotFoundException` (documented regression test, can be skipped/removed after fix depending policy).

### Test 2: Safe Introspection (post-fix)
- Same schema as Test 1.
- Assert no `KeyNotFoundException` is thrown during introspection/schema init.

### Test 3: Validation Error for Orphan Interface
- Same schema as Test 1.
- Run type-map validation entry point.
- Assert deterministic error contains interface name and orphaned-implementation message.

### Test 4: Normal Interface Implementations
- Interface with one object implementation.
- Assert introspection returns that object in possible types.

### Test 5: Multiple Implementations
- Interface with two object implementations.
- Assert introspection returns both possible types.

### Test 6: Mixed Schema Stability
- Include additional unrelated interfaces/unions/objects.
- Assert no crashes and correct possible type resolution across all abstract types.

## Acceptance Criteria
1. No `KeyNotFoundException` from `getPossibleTypes` for missing interface key.
2. Orphan interface case yields controlled behavior (empty set + validation error, or direct structured validation error per chosen policy).
3. Existing interface/union introspection behavior remains unchanged for valid schemas.
4. Tests cover single/multiple/no implementations and pass consistently.

## Backward Compatibility Notes
- Safe lookup is non-breaking for valid schemas.
- Invalid schemas move from low-level exception to explicit, actionable diagnostics.

## Implementation Notes
- Keep error text stable for test assertions.
- Prefer adding tests before/with fix to prevent future regressions.
