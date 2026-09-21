# Covariance Validation Test Specification

## Purpose
Define exhaustive tests for GraphQL interface implementation covariance and nullability compatibility in FSharp.Data.GraphQL type validation.

## Problem Statement (Current Bug)
During schema initialization, executor calls `Validation.Types.validateTypeMap schema.TypeMap` and throws `GQLMessageException` when validation returns errors.

Observed runtime error pattern:
- `'<Object>.<field>' field signature does not match it's definition in interface <Interface>`

Exact failure location in library:
- `src/FSharp.Data.GraphQL.Server/Executor.fs` (schema startup validation)
- `src/FSharp.Data.GraphQL.Shared/Validation.fs`, function `validateImplements`

Current implementation in `validateImplements` uses strict equality:
- `Some objf when objf = f -> acc`
- otherwise reports signature mismatch

This equality-based check is stricter than GraphQL spec subtyping rules for interface field return types and nullability covariance.

## GraphQL Compatibility Rules to Validate
For object type `O implements I`, each interface field `f` must satisfy:
1. Field exists on object with same name.
2. Arguments are compatible (same required args; extra args on object must be optional).
3. Return type on object is equal to or a valid subtype of interface return type.
4. Non-null covariance: `T!` is subtype of `T` (allowed).
5. List/null wrappers must be compared structurally by spec subtyping rules.
6. If interface return type is interface/union, object return type may be a concrete implementing/member type (covariance).

## Nullable / StructNullable Coverage
In this codebase:
- `Nullable X` and `StructNullable X` both produce nullable GraphQL wrappers.
- Non-wrapper `X` is non-null GraphQL type.

Tests must cover both wrappers equivalently for compatibility decisions:
- `Nullable InterfaceType` vs concrete non-null implementor type.
- `StructNullable InterfaceType` vs concrete non-null implementor type.
- `Nullable T` vs `Nullable T` exact match.
- `StructNullable T` vs `StructNullable T` exact match.
- Negative cases where nested wrappers are incompatible (e.g., list item nullability mismatch).

## Generic Test Model (No domain-specific names)
Use neutral names only:

Interfaces:
- `IParentView`
- `IChildView`

GraphQL interfaces:
- `IChildInfo`
- `IParentInfo` with field `child: IChildInfo`

Concrete object types:
- `ChildAInfo implements IChildInfo`
- `ChildBInfo implements IChildInfo`
- `ParentAInfo implements IParentInfo` with `child: ChildAInfo`
- `ParentBInfo implements IParentInfo` with `child: ChildBInfo`

This model must be reused for all covariance and nullability test cases.

## Test Matrix (Must Cover All Cases)
### A. Positive covariance cases (must pass)
1. Interface field type `IChildInfo`, object field type `ChildAInfo` (implements `IChildInfo`).
2. Same as A1 for second implementation (`ChildBInfo`).
3. Interface field `Nullable IChildInfo`, object field non-null `ChildAInfo`.
4. Interface field `StructNullable IChildInfo`, object field non-null `ChildAInfo`.
5. Interface field non-null `IChildInfo`, object field same non-null `IChildInfo` (exact).
6. Interface field list `List<IChildInfo>`, object field list `List<ChildAInfo>` where library supports list covariance by member subtype.
7. Deep wrappers: interface `Nullable(List(Nullable(IChildInfo)))`, object `List(ChildAInfo)` where valid by non-null covariance.

### B. Negative covariance cases (must fail)
1. Interface field `IChildInfo`, object field unrelated object type `OtherInfo` (not implementing).
2. Interface field non-null `IChildInfo`, object field nullable `Nullable IChildInfo` (wider, invalid).
3. Interface field list `List<IChildInfo>`, object field scalar `ChildAInfo`.
4. Interface field `List<NonNull IChildInfo>`, object field `List<Nullable ChildAInfo>` (invalid nullability widening).
5. Interface field arguments mismatch (missing required arg, type mismatch, extra required arg).

### C. Nullable vs StructNullable parity (must pass/fail identically)
For each scenario A3, A4, B2, B4 create paired tests:
- one with `Nullable`
- one with `StructNullable`
Expected result must be identical for semantic-equivalent wrappers.

### D. Existing strict-equality regression (must reproduce old bug)
Create a test where only difference is:
- interface field type = interface def
- object field type = implementing concrete object def

Expected by spec: Success.
Current behavior before fix: ValidationError with signature mismatch message.
This test documents the bug and prevents reintroduction.

## Test File Placement
- Extend `tests/FSharp.Data.GraphQL.Tests/TypeValidationTests.fs` for focused unit cases, or
- create `tests/FSharp.Data.GraphQL.Tests/TypeValidationCovarianceTests.fs` if separation is preferred.

## Assertion Style
- Use `validateImplements` for unit-level behavior.
- Use `validateTypeMap` for end-to-end schema-level validation with multiple types registered.
- Verify exact error strings for negative tests where stable, otherwise verify error contains object+field+interface identifiers.

## Proposed Fix in Validation Engine
Replace strict `objf = f` signature equality with structural GraphQL compatibility check:
1. Compare field names and argument compatibility by spec rules.
2. Compare return types via `isOutputSubtype(objectType, interfaceType)`.
3. Implement recursive wrapper-aware subtype check:
   - `NonNull(A)` subtype of `A`
   - `List(A)` subtype of `List(B)` iff `A` subtype of `B`
   - object subtype of interface if object implements interface
   - object subtype of union if object is a union member
   - named scalars/enums require exact type identity

Pseudo-contract:
- `isFieldImplementationCompatible(objectField, interfaceField) -> bool`
- used by `validateImplements` instead of direct equality.

## Acceptance Criteria
1. All positive covariance tests pass.
2. All negative compatibility tests fail with deterministic errors.
3. Nullable/StructNullable parity tests pass.
4. No regressions in existing `TypeValidationTests.fs`.
5. Schema initialization no longer throws for valid covariance implementations.

## Notes for Reviewers
- This is a spec-driven validation correction, not a domain-model workaround.
- Goal is GraphQL spec compliance at type-system validation layer.
