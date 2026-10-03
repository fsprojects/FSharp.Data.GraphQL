// Some of the documents below are ported from the tests of Hot Chocolate
// (MIT License, Copyright (c) 2018 - present ChilliCream Inc.)
// and of Apollo Server (MIT License, Copyright (c) 2016-2020 Apollo Graph, Inc.).
module FSharp.Data.GraphQL.Tests.ValidationDoSTests

open System
open System.Text
open Xunit

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Validation
open FSharp.Data.GraphQL.Validation.Ast

#nowarn "40"

let rec UserType : ObjectDef<obj> =
    DefineRec.Object<obj>(
        "User",
        fieldsFn =
            fun () -> [
                Define.Field ("id", StringType, (fun _ _ -> ""))
                Define.Field ("name", StringType, (fun _ _ -> ""))
                Define.Field ("email", StringType, (fun _ _ -> ""))
                Define.Field ("age", IntType, (fun _ _ -> 0))
                Define.Field ("address", StringType, (fun _ _ -> ""))
                Define.Field ("phone", StringType, (fun _ _ -> ""))
                Define.Field ("friend", UserType, (fun _ user -> user))
            ]
    )

and QueryType : ObjectDef<obj> =
    DefineRec.Object<obj>(
        "Query",
        fieldsFn =
            fun () -> [
                Define.Field ("hello", StringType, (fun _ _ -> ""))
                Define.Field ("field", QueryType, (fun _ root -> root))
                Define.Field ("me", UserType, (fun _ root -> root))
                Define.Field ("user", UserType, (fun _ root -> root))
                for name in [ "a"; "b"; "c"; "d"; "e" ] do
                    Define.Field (name, StringType, (fun _ _ -> ""))
            ]
    )

#warnon "40"

let private schema = Schema (QueryType)

let private introspectionSchema = (schema :> ISchema).Introspected

let private schemaInfo = SchemaInfo.FromIntrospectionSchema introspectionSchema

let private getContext = Parser.parse >> getValidationContext schemaInfo

let private validate = Parser.parse >> validateDocument introspectionSchema

/// Generous enough for slow CI machines, yet far below the minutes or hours that an exponential validation takes
let private timeout = TimeSpan.FromSeconds 10.0

let private runIsolated (f : unit -> 'T) : 'T = runOnSmallStack timeout f

let private errorMessages (result : ValidationResult<GQLProblemDetails>) =
    match result with
    | ValidationError errors -> errors |> List.map _.Message
    | Success ->
        fail "Expected validation errors, but the document is valid."
        []

let private tooManySelections limit = $"The document recursively requests too many selections (more than %i{limit})."

let private nestedTooDeeply limit =
    $"The document is nested too deeply once fragment spreads are inlined (more than %i{limit} levels)."

let private cyclicReference name = $"Fragment '%s{name}' is making a cyclic reference."

/// The operation, followed by the fragments F0 to F(levels - 1) that each spread the next fragment twice, and F(levels)
let private fragmentBomb (operation : string) (levels : int) =
    let document = StringBuilder ()
    document.AppendLine operation |> ignore
    for i in 0 .. levels - 1 do
        document.AppendLine $"fragment F%i{i} on Query {{ ...F%i{i + 1} ...F%i{i + 1} }}"
        |> ignore
    document.AppendLine $"fragment F%i{levels} on Query {{ a }}"
    |> ignore
    document.ToString ()

/// The operation, followed by the fragments F0 to F(length - 1) that each spread the next fragment once, and F(length)
let private fragmentChain (operation : string) (length : int) =
    let document = StringBuilder ()
    document.AppendLine operation |> ignore
    for i in 0 .. length - 1 do
        document.AppendLine $"fragment F%i{i} on Query {{ ...F%i{i + 1} }}"
        |> ignore
    document.AppendLine $"fragment F%i{length} on Query {{ a }}"
    |> ignore
    document.ToString ()

[<Fact>]
let ``Subscription root field rule terminates on a fragment that spreads itself`` () =
    let actual =
        runIsolated (fun () ->
            AstValidationTests.getContext "subscription S { ...F } fragment F on Subscription { ...F }"
            |> validateSubscriptionSingleRootField)
    actual |> equals Success

[<Fact>]
let ``Subscription root field rule terminates on fragments that spread each other through an inline fragment`` () =
    let actual =
        runIsolated (fun () ->
            AstValidationTests.getContext
                """subscription S { ... on Subscription { ...A } }
fragment A on Subscription { ping ...B }
fragment B on Subscription { ...A }"""
            |> validateSubscriptionSingleRootField)
    actual |> equals Success

[<Fact>]
let ``Subscription root field rule counts fields selected before a fragment spread`` () =
    let actual =
        AstValidationTests.getContext "subscription S { first: ping ...F } fragment F on Subscription { second: ping }"
        |> validateSubscriptionSingleRootField
    actual
    |> errorMessages
    |> equals [
        "Subscription operations should have only one root field. Operation 'S' has 2 fields (second, first)."
    ]

[<Fact>]
let ``Subscription root field rule collects a fragment spread twice only once`` () =
    let actual =
        AstValidationTests.getContext "subscription S { ...F ...F } fragment F on Subscription { ping }"
        |> validateSubscriptionSingleRootField
    actual |> equals Success

[<Fact>]
let ``Executor rejects a cyclic fragment in an unselected subscription instead of crashing`` () =
    let executor = Executor (schema)
    let result =
        runIsolated (fun () ->
            executor.CreateExecutionPlan ("query Q { hello } subscription S { ...F } fragment F on Query { ...F }", operationName = "Q"))
    match result with
    | Ok _ -> fail "Expected the execution plan to be rejected."
    | Error (struct (_, errors)) -> errors |> hasError (cyclicReference "F")

[<Fact>]
let ``Fragment cycle rule reports only the fragments of the cycle`` () =
    let actual =
        getContext "{ a } fragment X on Query { ...A } fragment A on Query { ...A }"
        |> validateFragmentsMustNotFormCycles
    actual |> errorMessages |> equals [ cyclicReference "A" ]

[<Fact>]
let ``Validation of a cyclic fragment bomb completes quickly`` () =
    let document = StringBuilder ()
    document.AppendLine "{ ...F0 }" |> ignore
    for i in 0..29 do
        let next = (i + 1) % 30
        document.AppendLine $"fragment F%i{i} on Query {{ ...F%i{next} ...F%i{next} }}"
        |> ignore
    let errors = runIsolated (fun () -> validate (document.ToString ()) |> errorMessages)
    errors
    |> List.filter (fun message -> message.EndsWith ("is making a cyclic reference.", StringComparison.Ordinal))
    |> List.length
    |> equals 30

[<Fact>]
let ``Validation of fragments that all spread each other completes quickly`` () =
    // Following the spreads per path visits every ordering of the fragments: 11! paths
    let names = [ for i in 0..11 -> $"F%i{i}" ]
    let document = StringBuilder ()
    document.AppendLine "query Q($unused: Int) { ...F0 }"
    |> ignore
    for name in names do
        let spreads =
            names
            |> List.filter (fun other -> other <> name)
            |> List.map (fun other -> $"...%s{other} @defer")
            |> String.concat " "
        document.AppendLine $"fragment %s{name} on Query {{ a %s{spreads} }}"
        |> ignore
    let errors = runIsolated (fun () -> validate (document.ToString ()) |> errorMessages)
    errors |> contains (cyclicReference "F11") |> ignore
    errors
    |> contains "A variable '$unused' is not used in operation 'Q'. Every variable must be used."
    |> ignore

let private apolloQuery =
    """query {
  user {
    id
    name
    ...UserDetails
  }
}

fragment UserDetails on User {
  email
  age
  ...MoreDetails
}

fragment MoreDetails on User {
  address
  phone
}"""

let private apolloBiggerQuery =
    """query {
  user {
    email
    age
    address
    phone
    ...UserDetails
  }
}

fragment UserDetails on User {
  id
  name
  email
  age
  ...MoreDetails
}

fragment MoreDetails on User {
  id
  name
  address
  phone
}"""

[<Fact>]
let ``Document size counts every definition with its fragment spreads inlined`` () =
    // Apollo Server counts 9 and 15 selections for the operations alone; the fragment definitions add 5 + 2 and 9 + 4
    let struct (querySelections, _) = measureDocument Int64.MaxValue (Parser.parse apolloQuery)
    let struct (biggerQuerySelections, _) = measureDocument Int64.MaxValue (Parser.parse apolloBiggerQuery)
    querySelections |> equals 16L
    biggerQuerySelections |> equals 28L

[<Fact>]
let ``Selection limit accepts a document at the limit and rejects a bigger one`` () =
    let validateWithLimit =
        Parser.parse
        >> validateDocumentWithLimits 16 128 100 introspectionSchema
    validateWithLimit apolloQuery |> equals Success
    validateWithLimit apolloBiggerQuery
    |> errorMessages
    |> equals [ tooManySelections 16 ]

[<Fact>]
let ``Fragment bomb is rejected quickly`` () =
    let errors =
        runIsolated (fun () ->
            validate (fragmentBomb "query Q { ...F0 }" 20)
            |> errorMessages)
    errors
    |> equals [ tooManySelections DocumentLimitsDefaults.MaxRecursiveSelections ]

[<Fact>]
let ``Unused fragment bomb is rejected quickly`` () =
    let errors = runIsolated (fun () -> validate (fragmentBomb "{ a }" 20) |> errorMessages)
    errors
    |> equals [ tooManySelections DocumentLimitsDefaults.MaxRecursiveSelections ]

[<Fact>]
let ``Many operations spreading a large fragment are rejected`` () =
    let document = StringBuilder ()
    for i in 0..199 do
        document.AppendLine $"query Q%i{i} {{ ...Big }}" |> ignore
    document.Append "fragment Big on Query {" |> ignore
    for i in 0..999 do
        document.Append $" a%i{i}: a" |> ignore
    document.AppendLine " }" |> ignore
    let errors = runIsolated (fun () -> validate (document.ToString ()) |> errorMessages)
    errors
    |> equals [ tooManySelections DocumentLimitsDefaults.MaxRecursiveSelections ]

[<Fact>]
let ``Recursive fragments fail`` () =
    // Hot Chocolate: Ensure_Recursive_Fragments_Fail
    let errors = runIsolated (fun () -> validate "fragment f on Query{...f} {...f}" |> errorMessages)
    errors |> contains (cyclicReference "f") |> ignore

[<Fact>]
let ``Recursive fragments nested in fields fail`` () =
    // Hot Chocolate: Ensure_Recursive_Fragments_Fail_2
    let document =
        """fragment f on Query {
  ...f
  field {
    ...f
    field {
      ...f
    }
  }
}

{...f}"""
    let errors = runIsolated (fun () -> validate document |> errorMessages)
    errors |> contains (cyclicReference "f") |> ignore

[<Fact>]
let ``Fragment traversal bomb is rejected quickly`` () =
    // Hot Chocolate: Fragment_Traversal_Bomb_Should_Complete_Quickly (CVE-2025-32032).
    // Hot Chocolate accepts this document; it inlines 50^9 selections here, so the selection limit rejects it.
    let names = "ABCDEFGHIJ"
    let document = StringBuilder ()
    document.AppendLine "{...A}" |> ignore
    for i in 0 .. names.Length - 2 do
        let spreads = String.replicate 50 $" ...%c{names[i + 1]}"
        document.AppendLine $"fragment %c{names[i]} on Query {{%s{spreads} }}"
        |> ignore
    document.AppendLine $"fragment %c{names[names.Length - 1]} on Query {{ __typename }}"
    |> ignore
    let errors = runIsolated (fun () -> validate (document.ToString ()) |> errorMessages)
    errors
    |> equals [ tooManySelections DocumentLimitsDefaults.MaxRecursiveSelections ]

[<Fact>]
let ``Fragment expansion bomb is rejected quickly`` () =
    // Hot Chocolate: Fragment_Expansion_Bomb_Should_Complete_Quickly (CVE-2025-32032).
    // Hot Chocolate accepts this document; it inlines 2^20 field selections here, so the selection limit rejects it.
    let depth = 20
    let document = StringBuilder ()
    document.AppendLine "{ field { ...F0 } }" |> ignore
    for i in 0 .. depth - 1 do
        document.AppendLine $"fragment F%i{i} on Query {{ fa: field {{ ...F%i{i + 1} }} fb: field {{ ...F%i{i + 1} }} }}"
        |> ignore
    document.AppendLine $"fragment F%i{depth} on Query {{ __typename }}"
    |> ignore
    let errors = runIsolated (fun () -> validate (document.ToString ()) |> errorMessages)
    errors
    |> equals [ tooManySelections DocumentLimitsDefaults.MaxRecursiveSelections ]

[<Fact>]
let ``Deep fragment expansion is valid`` () =
    // Hot Chocolate: Deep_Fragment_Expansion
    let document = StringBuilder ()
    document.AppendLine "query { me { ...F0 } }" |> ignore
    for i in 0..9 do
        document.AppendLine $"fragment F%i{i} on User {{ a: friend {{ ...F%i{i + 1} }} b: friend {{ ...F%i{i + 1} }} }}"
        |> ignore
    document.AppendLine "fragment F10 on User { a: id b: id }"
    |> ignore
    let actual = runIsolated (fun () -> validate (document.ToString ()))
    actual |> equals Success

[<Fact>]
let ``Inline fragments with many aliased typename fields validate quickly`` () =
    // Hot Chocolate: Inline_Fragment_TypeName_Amplification_Should_Be_Rejected_By_Parser (CVE-2023-26144)
    let document = StringBuilder ()
    document.AppendLine "{" |> ignore
    for _ in 0..9 do
        document.Append "  ... on Query {" |> ignore
        for j in 0..499 do
            document.Append $" f%i{j}: __typename" |> ignore
        document.AppendLine " }" |> ignore
    document.AppendLine "}" |> ignore
    let actual = runIsolated (fun () -> validate (document.ToString ()))
    actual |> equals Success

[<Fact>]
let ``Long fragment chain is rejected without overflowing the stack`` () =
    let errors = runIsolated (fun () -> validate (fragmentChain "{ ...F0 }" 10_000) |> errorMessages)
    errors
    |> equals [ tooManySelections DocumentLimitsDefaults.MaxRecursiveSelections ]

[<Fact>]
let ``Fragment chain deeper than the nesting limit is rejected`` () =
    let errors = runIsolated (fun () -> validate (fragmentChain "{ ...F0 }" 200) |> errorMessages)
    errors
    |> equals [ nestedTooDeeply DocumentLimitsDefaults.MaxNestingDepth ]

[<Fact>]
let ``Validation errors are capped`` () =
    let fields = [ for i in 0..499 -> $"zz%i{i}" ] |> String.concat " "
    let errors = validate $"{{ %s{fields} }}" |> errorMessages
    errors
    |> List.length
    |> equals (DocumentLimitsDefaults.MaxValidationErrors + 1)
    errors
    |> List.last
    |> equals "Too many validation errors, error limit reached. Validation aborted."

[<Fact>]
let ``Validation reporting fifty thousand errors completes quickly`` () =
    let fields = [ for i in 0..49_999 -> $"zz%i{i}" ] |> String.concat " "
    let document = Parser.parse $"{{ %s{fields} }}"
    let errors =
        runIsolated (fun () ->
            validateDocumentWithLimits Int32.MaxValue 128 Int32.MaxValue introspectionSchema document
            |> errorMessages)
    errors |> List.length |> equals 50_000

[<Fact>]
let ``Executor rejects a document with two hundred thousand fields`` () =
    let query = "{ " + String.Join (" ", Seq.replicate 200_000 "a") + " }"
    let executor = Executor (schema)
    let result = runIsolated (fun () -> executor.CreateExecutionPlan query)
    match result with
    | Ok _ -> fail "Expected the execution plan to be rejected."
    | Error (struct (_, errors)) ->
        errors
        |> List.map _.Message
        |> equals [ tooManySelections DocumentLimitsDefaults.MaxRecursiveSelections ]
