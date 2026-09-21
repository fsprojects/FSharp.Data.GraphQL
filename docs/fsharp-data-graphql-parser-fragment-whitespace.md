# FSharp.Data.GraphQL parser rejects `}fragment` without whitespace

## Summary

The query parser in `FSharp.Data.GraphQL` fails to parse a **valid, spec-compliant GraphQL document** when a selection set's closing brace `}` is immediately followed by the `fragment` keyword with no whitespace between them (as is standard in minified queries emitted by virtually every GraphQL client).

This breaks live introspection from all standards-compliant tooling (GraphQL Inspector, GraphQL Code Generator, Apollo CLI, urql, Relay, `graphql-request`, etc.), because they all send a minified introspection query in which the trailing `}}}}` of the `__schema` selection set is directly adjacent to `fragment FullType on __Type`.

## Environment

| Item | Value |
|------|-------|
| Library | `FSharp.Data.GraphQL.Server` / `FSharp.Data.GraphQL.Server.AspNetCore` |
| Branch observed | `dev` |
| Host | ASP.NET Core on .NET 10 |
| Endpoint | `POST /GraphQL` (configured in `GraphQLStartup.cs`) |
| Client | `@graphql-inspector/cli` (also reproducible with plain `curl` / `Invoke-WebRequest`) |

## Reproduction

### Minimal repro payload

```http
POST /GraphQL
Content-Type: application/json

{"query":"{__typename}fragment X on Query{__typename}"}
```

### Full introspection repro (as sent by `graphql-inspector introspect`)

```json
{"query":"query IntrospectionQuery{__schema{queryType{name kind}mutationType{name kind}subscriptionType{name kind}types{...FullType}directives{name description locations args{...InputValue}}}}fragment FullType on __Type{kind name description fields(includeDeprecated:true){name description args{...InputValue}type{...TypeRef}isDeprecated deprecationReason}inputFields{...InputValue}interfaces{...TypeRef}enumValues(includeDeprecated:true){name description isDeprecated deprecationReason}possibleTypes{...TypeRef}}fragment InputValue on __InputValue{name description type{...TypeRef}defaultValue}fragment TypeRef on __Type{kind name ofType{name kind ofType{name kind ofType{name kind ofType{name kind ofType{name kind ofType{name kind ofType{name kind ofType{name kind ofType{name kind}}}}}}}}}}"}
```

## Observed behavior

The server returns **HTTP 400** with:

```
{
  "type":   "https://tools.ietf.org/html/rfc9110#section-15.5.1",
  "title":  "Cannot parse GraphQL query",
  "status": 400,
  "detail": "Error in Ln: 1 Col: 183
             iption locations args{...InputValue}}}}fragment FullType on __Type{kind name de
                                                    ^
             Unknown Error(s)",
  "instance": "/GraphQL"
}
```

Column 183 is the exact offset of the `f` in `fragment` immediately after the `}}}}` that closes the `__schema` selection set.

## Expected behavior

The document is **valid** per the GraphQL specification. `}` and `fragment` are separate lexical tokens; the GraphQL grammar allows arbitrary (including zero) whitespace/ignored tokens between them. The server should parse the query and return the introspection result (verified — the same query with a single space inserted between `}` and `fragment` returns HTTP 200 with the full ~400 KB introspection JSON).

## Impact

- All live introspection tooling is broken against this server:
  - `graphql-inspector introspect <url>`
  - `graphql-inspector diff <old> <url>` / `validate <docs> <url>`
  - `graphql-codegen` with a URL schema source
  - Apollo Rover / Apollo CLI introspection
  - Any client using `getIntrospectionQuery()` from the `graphql-js` reference implementation (which produces a **minified** string by default in recent versions)
- Playground / Altair / GraphiQL / Nitro still work because their built-in introspection query happens to be pretty-printed, so `}` and `fragment` are separated by whitespace.
- Any user-authored query that omits whitespace between a selection set and a following fragment definition (perfectly legal) will also fail.

## Root-cause hypothesis

The parser (FParsec-based, in `FSharp.Data.GraphQL.Shared` / `Parser`) most likely requires at least one whitespace/ignored-token between a `Definition` and the next `Definition` in a `Document`. In the grammar:

```
Document        ::= Definition+
Definition      ::= OperationDefinition | FragmentDefinition | TypeSystemDefinition
FragmentDefinition ::= "fragment" FragmentName TypeCondition Directives? SelectionSet
```

The GraphQL spec defines the token stream so that any two adjacent tokens are separable without whitespace **unless both are name-like tokens** (identifiers, keywords, numbers). `}` is a punctuator, `fragment` is a keyword — they are distinct token classes and require no separator.

Likely offending patterns in the parser:

1. The top-level `document` combinator uses something like
   `many1 (definition .>> spaces1)` or `sepBy1 definition spaces1`
   instead of `many1 (definition .>> spaces)`.
2. Or `fragmentDefinition` is written as `pstring "fragment" >>. spaces1 >>. …` but preceded by a rule that demands whitespace *before* the keyword.
3. Or the top-level parser is `many1 (spaces >>. definition)` where the previous definition parser doesn't consume the closing `}` cleanly, leaving the parser expecting whitespace-then-definition and refusing to accept a punctuator boundary.

## Suggested fix

1. In the `document`/`definitions` combinator, ensure inter-definition separators are `spaces` (zero-or-more) rather than `spaces1` (one-or-more). GraphQL's `Ignored` production is `*`, not `+`.
2. Verify the same for all places where two adjacent grammar productions can meet at a punctuator/keyword boundary — notably:
   - `SelectionSet` followed by `FragmentDefinition` at the document level (the bug reported here).
   - `SelectionSet` followed by another `OperationDefinition` (`}query …`, `}mutation …`).
   - `Arguments`/`Directives` transitions at the end of a `Field`.
3. Add regression tests using the exact `graphql-js` output of `getIntrospectionQuery({ descriptions: true })` in its minified form, plus these two smoke tests:

   ```graphql
   {__typename}fragment X on Query{__typename}
   ```

   ```graphql
   query A{__typename}query B{__typename}
   ```

Both should parse successfully.

## References

- GraphQL spec, [§2.1 Source Text / Ignored Tokens](https://spec.graphql.org/October2021/#sec-Source-Text.Ignored-Tokens): *"Ignored tokens are allowed anywhere between other tokens."* (i.e., zero or more, not one or more.)
- graphql-js [`getIntrospectionQuery`](https://github.com/graphql/graphql-js/blob/main/src/utilities/getIntrospectionQuery.ts) — reference implementation used by essentially every JS/TS client.

## Workaround for consumers (until fixed)

- Do not perform live introspection against the server. Instead, run a formatted (whitespace-separated) introspection query manually and commit the resulting SDL/JSON as a static schema artifact; point tooling at the file rather than the URL.

Example PowerShell one-liner that works today:

```powershell
[System.Net.ServicePointManager]::ServerCertificateValidationCallback = { $true }
$q = 'query IntrospectionQuery { __schema { queryType { name } mutationType { name } subscriptionType { name } types { ...FullType } directives { name description locations args { ...InputValue } } } } fragment FullType on __Type { kind name description fields(includeDeprecated: true) { name description args { ...InputValue } type { ...TypeRef } isDeprecated deprecationReason } inputFields { ...InputValue } interfaces { ...TypeRef } enumValues(includeDeprecated: true) { name description isDeprecated deprecationReason } possibleTypes { ...TypeRef } } fragment InputValue on __InputValue { name description type { ...TypeRef } defaultValue } fragment TypeRef on __Type { kind name ofType { kind name ofType { kind name ofType { kind name ofType { kind name ofType { kind name ofType { kind name ofType { kind name } } } } } } } }'
$body = @{ query = $q } | ConvertTo-Json -Compress
Invoke-WebRequest -Uri https://localhost:5003/GraphQL -Method Post -ContentType 'application/json' -Body $body -UseBasicParsing |
    Select-Object -ExpandProperty Content |
    Set-Content -Path introspection.json -Encoding UTF8
```
