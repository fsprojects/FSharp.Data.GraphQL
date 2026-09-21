# FSharp.Data.GraphQL exposes `File` and `ObjectListFilter` as `INPUT_OBJECT` with zero fields (spec-illegal)

## Summary

The server libraries `FSharp.Data.GraphQL.Server.AspNetCore` and `FSharp.Data.GraphQL.Server.Middleware` publish two input types via introspection that violate the GraphQL specification:

- `input File` — from `FSharp.Data.GraphQL.Server.AspNetCore` (multipart file upload input).
- `input ObjectListFilter` — from `FSharp.Data.GraphQL.Server.Middleware` (generic list-filter middleware).

Both are surfaced as **`INPUT_OBJECT`** types with an **empty `inputFields` array**. Per [GraphQL spec §3.10 Input Objects](https://spec.graphql.org/October2021/#sec-Input-Objects):

> An Input Object type must define one or more input fields.

As a result, **any standards-compliant client that validates the introspected schema rejects the entire schema**, even though the server can execute queries against it. This blocks GraphQL Inspector, GraphQL Code Generator, Apollo tooling, Relay Compiler, `graphql-js`-based validation in general.

## Environment

| Item | Value |
|------|-------|
| Library | `FSharp.Data.GraphQL.Server.AspNetCore`, `FSharp.Data.GraphQL.Server.Middleware`, `FSharp.Data.GraphQL.Shared` |
| Version observed | `4.0.0-ci-31608744053` (also present in earlier 4.x CI builds) |
| Host | ASP.NET Core on .NET 10 |
| Endpoint | `POST /GraphQL` |
| Failing client | `@graphql-inspector/cli` (any version using `graphql-js` ≥ 15) |

## Reproduction

1. Register a server with either the multipart upload middleware or the `ObjectListFilter` middleware (both are default in `FSharp.Data.GraphQL.Server.AspNetCore` / `.Middleware`).
2. Run:

   ```powershell
   graphql-inspector introspect https://localhost:5003/GraphQL --write schema.graphql
   ```

3. Observe:

   ```
   Error: Input Object type File must define one or more fields.
   Input Object type ObjectListFilter must define one or more fields.
       at assertValidSchema (…/graphql-js/type/validate.js:91:11)
       at assertValidExecutionArguments (…/graphql-js/execution/execute.js:419:35)
       at introspectionFromSchema (…/graphql-js/utilities/introspectionFromSchema.js:93:43)
   ```

The wire-level introspection response contains, among other types:

```json
{ "kind": "INPUT_OBJECT", "name": "File",             "inputFields": [] }
{ "kind": "INPUT_OBJECT", "name": "ObjectListFilter", "inputFields": [] }
```

`graphql-js` runs `assertValidSchema` on the reconstructed schema and refuses it because `inputFields` is empty for both types.

## Expected behavior

For every `INPUT_OBJECT` type published via introspection, `inputFields` must be non-empty. Equivalently, the type must not be modeled as an input object if it has no static fields — it should be modeled as a **custom scalar** or as an input object with explicit fields.

## Impact

- Any client that calls `buildClientSchema` / `assertValidSchema` from `graphql-js` throws — this is the reference implementation used by essentially every JS/TS GraphQL tool.
- Confirmed broken:
  - `graphql-inspector introspect | diff | validate | similar` against a live URL.
  - `@graphql-codegen/cli` with a URL schema source.
  - Apollo Rover / Apollo CLI introspection.
  - Relay Compiler when fed the introspected schema.
- The server itself continues to execute queries successfully because it doesn't apply strict spec validation to its own schema; the problem only surfaces in downstream tooling.
- Playground / Altair / GraphiQL / Nitro happen to keep working because they render the raw introspection JSON without going through `assertValidSchema`.

## Root cause (two separate cases)

### 1. `File`

Source: `FSharp.Data.GraphQL.Server.AspNetCore` — the multipart file upload input.

Semantically this is a **scalar** value: an out-of-band multipart part referenced from a variables JSON blob. The de-facto community standard is [`graphql-multipart-request-spec`](https://github.com/jaydenseric/graphql-multipart-request-spec), which models uploads as a **custom `Upload` scalar**. Every JS client (Apollo Upload Client, `graphql-request`, urql, Relay upload adapters) expects `scalar Upload`, not `input File { … }`.

Modeling it as an empty `INPUT_OBJECT` is both spec-illegal (must have ≥ 1 field) and interoperability-breaking (no client understands `input File`).

### 2. `ObjectListFilter`

Source: `FSharp.Data.GraphQL.Server.Middleware` — the generic list-filter middleware.

The runtime accepts a recursive tree of operators (`_and`, `_or`, `_not`, `_eq`, `_neq`, `_in`, `_nin`, `_gt`, `_gte`, `_lt`, `_lte`, `_starts_with`, `_ends_with`, `_contains`, etc.) whose *field* names depend on the element type. Because the shape is dynamic, the middleware currently registers the type without declaring any static input fields, leaving `inputFields: []` in introspection.

## Suggested fixes

### For `File`

Rename to **`Upload`** and publish it as a **custom scalar** rather than an input object:

```graphql
scalar Upload
```

- Aligns with the multipart request spec used by the wider ecosystem.
- Fixes the spec violation (scalars have no field requirement).
- Enables all existing JS/TS upload clients to work without server-side changes.
- Server-side: the resolver already receives an opaque value from the multipart form; that value can be surfaced as a scalar just as easily.

Provide a compatibility shim / opt-in flag for the legacy `File` name if backwards compatibility matters.

### For `ObjectListFilter`

Two acceptable options:

**Option A (preferred): emit explicit operator fields per element type.**
For each list field the middleware attaches to, generate a concrete `<TypeName>ListFilter` input with the fully-typed `_and: [<TypeName>ListFilter!]`, `_or: […]`, `_eq: <TypeName>`, `_in: [<TypeName>!]`, etc. This is what Hasura, PostGraphile, and Marten's own OData-style filters do, and it plays perfectly with codegen.

**Option B (fallback): expose the filter as a `scalar ObjectListFilter`.**
Carries a JSON value in transit. Simplest patch, loses typed autocompletion in tooling, but restores spec compliance and unblocks every downstream client.

Whichever route is chosen, do **not** keep the current `INPUT_OBJECT` with an empty `inputFields` list.

### Regression test

Add a schema-validation test that reconstructs the introspection output using `graphql-js` (or an F# port) and calls `assertValidSchema`. Minimal repro:

```js
import { buildClientSchema, assertValidSchema } from 'graphql';

const res  = await fetch(url, { method: 'POST', headers: { 'content-type': 'application/json' },
                                body: JSON.stringify({ query: getIntrospectionQuery() }) });
const json = await res.json();
assertValidSchema(buildClientSchema(json.data));   // must not throw
```

## References

- GraphQL spec, [§3.10 Input Objects](https://spec.graphql.org/October2021/#sec-Input-Objects) — input object types must define ≥ 1 field.
- [`graphql-multipart-request-spec`](https://github.com/jaydenseric/graphql-multipart-request-spec) — the de-facto multipart upload spec, based on a custom `Upload` scalar.
- graphql-js [`assertValidSchema`](https://github.com/graphql/graphql-js/blob/main/src/type/validate.ts) — the validator every JS client runs.

## Related upstream report

See also the sibling report [`fsharp-data-graphql-parser-fragment-whitespace.md`](./fsharp-data-graphql-parser-fragment-whitespace.md), which covers the parser bug rejecting `}fragment` in minified introspection queries. That bug is fixed in `4.0.0-ci-31608744053`; the two `INPUT_OBJECT` issues documented here are still present in the same build.
