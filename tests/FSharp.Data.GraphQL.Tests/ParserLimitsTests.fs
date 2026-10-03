module FSharp.Data.GraphQL.Tests.ParserLimitsTests

open System
open Xunit

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast

/// Generous enough for slow CI machines, yet far below the time that exponential parsing of the documents below takes
let private timeout = TimeSpan.FromSeconds 10.0

let private parseIsolated (query : string) = runOnSmallStack timeout (fun () -> Parser.tryParse query)

let private wantDocument (result : Result<Document, string>) =
    match result with
    | Ok document -> document
    | Error message ->
        fail $"Expected the document to parse, but got: %s{message}"
        Unchecked.defaultof<_>

let private wantError (result : Result<Document, string>) =
    match result with
    | Ok _ ->
        fail "Expected a syntax error, but the document parsed."
        ""
    | Error message -> message

let private assertNestedTooDeeply (message : string) =
    Assert.Contains (
        $"The document is nested deeper than %i{DocumentLimitsDefaults.MaxNestingDepth} braces, brackets and parentheses.",
        message,
        StringComparison.Ordinal
    )

let private variableType (document : Document) =
    match document.Definitions with
    | [ OperationDefinition operation ] -> operation.VariableDefinitions.Head.Type
    | definitions ->
        fail $"Expected a single operation, but got %A{definitions}"
        Unchecked.defaultof<_>

let rec private listTypeDepth =
    function
    | ListType inner -> 1 + listTypeDepth inner
    | NonNullType inner -> listTypeDepth inner
    | NamedType _ -> 0

/// Selection sets nested to the given depth: "{ a { a … } }"
let private nestedSelections (depth : int) = String.replicate depth "{ a " + String.replicate depth "}"

[<Fact>]
let ``List type nested 100 deep parses in linear time`` () =
    let inputType = String.replicate 100 "[" + "Int" + String.replicate 100 "]"
    let document =
        parseIsolated $"query ($v: %s{inputType}) {{ a }}"
        |> wantDocument
    document |> variableType |> listTypeDepth |> equals 100

[<Fact>]
let ``Non-null list type nested 100 deep parses in linear time`` () =
    let inputType =
        String.replicate 100 "["
        + "Int!"
        + String.replicate 100 "]!"
    let document =
        parseIsolated $"query ($v: %s{inputType}) {{ a }}"
        |> wantDocument
    let actual = variableType document
    actual |> listTypeDepth |> equals 100
    match actual with
    | NonNullType (ListType _) -> ()
    | other -> fail $"Expected a non-null list type, but got %A{other}"

[<Fact>]
let ``Non-null and list types parse as before`` () =
    let document = Parser.parse "query ($a: Int!, $b: [Int!]!, $c: [Int]) { a }"
    let actual =
        match document.Definitions with
        | [ OperationDefinition operation ] -> operation.VariableDefinitions |> List.map _.Type
        | _ -> []
    actual
    |> equals [
        NonNullType (NamedType "Int")
        NonNullType (ListType (NonNullType (NamedType "Int")))
        ListType (NamedType "Int")
    ]

[<Fact>]
let ``Selection sets nested to the limit parse`` () =
    parseIsolated (nestedSelections DocumentLimitsDefaults.MaxNestingDepth)
    |> wantDocument
    |> ignore

[<Fact>]
let ``Values, types and inline fragments nested to the limit parse`` () =
    // The braces and parentheses around them count as two levels
    let depth = DocumentLimitsDefaults.MaxNestingDepth - 2
    let listValue =
        String.replicate depth "["
        + "1"
        + String.replicate depth "]"
    let objectValue =
        String.replicate depth "{ b: "
        + "1"
        + String.replicate depth " }"
    let inputType =
        String.replicate (depth + 1) "["
        + "Int!"
        + String.replicate (depth + 1) "]!"
    // "{ ... on Query { ... on Query { a } } }": each inline fragment opens one more selection set
    let fragmentDepth = DocumentLimitsDefaults.MaxNestingDepth - 1
    let inlineFragments =
        String.replicate fragmentDepth "{ ... on Query "
        + "{ a }"
        + String.replicate fragmentDepth " }"
    for document in
        [
            $"{{ f(a: %s{listValue}) }}"
            $"{{ f(a: %s{objectValue}) }}"
            $"query ($v: %s{inputType}) {{ a }}"
            inlineFragments
        ] do
        parseIsolated document |> wantDocument |> ignore

[<Theory>]
[<InlineData(129)>]
[<InlineData(1000)>]
[<InlineData(5000)>]
[<InlineData(20000)>]
let ``Selection sets nested past the limit are rejected`` (depth : int) =
    parseIsolated (nestedSelections depth)
    |> wantError
    |> assertNestedTooDeeply

[<Fact>]
let ``List value nested 1000 deep is rejected`` () =
    let value = String.replicate 1000 "[" + "1" + String.replicate 1000 "]"
    parseIsolated $"{{ f(a: %s{value}) }}"
    |> wantError
    |> assertNestedTooDeeply

[<Fact>]
let ``Object value nested 1000 deep is rejected`` () =
    let value =
        String.replicate 1000 "{ b: "
        + "1"
        + String.replicate 1000 " }"
    parseIsolated $"{{ f(a: %s{value}) }}"
    |> wantError
    |> assertNestedTooDeeply

[<Fact>]
let ``Twenty thousand unclosed selection sets are rejected`` () =
    parseIsolated (String.replicate 20000 "{")
    |> wantError
    |> assertNestedTooDeeply

[<Fact>]
let ``Nesting beyond the limit throws a FormatException from parse`` () =
    throws<FormatException>(fun () -> Parser.parse (nestedSelections 129) |> ignore)
    |> _.Message
    |> assertNestedTooDeeply

[<Fact>]
let ``Brackets in strings do not count as nesting`` () =
    let brackets = String.replicate 200 "["
    parseIsolated $"{{ f(a: \"%s{brackets}\") }}"
    |> wantDocument
    |> ignore
    parseIsolated $"{{ f(a: \"\\\"%s{brackets}\") }}"
    |> wantDocument
    |> ignore

[<Fact>]
let ``Brackets in comments do not count as nesting`` () =
    let braces = String.replicate 200 "{"
    parseIsolated $"# %s{braces}\n{{ a }}"
    |> wantDocument
    |> ignore

[<Fact>]
let ``Runs of quotes do not hide nesting`` () =
    // The grammar has no block strings: it reads """" as two empty strings, so the brackets after them are nested values
    let value = String.replicate 140 "[" + "1" + String.replicate 140 "]"
    parseIsolated $"{{ f(a: [\"\"\"\" %s{value} \"\"\"\"\n]) }}"
    |> wantError
    |> assertNestedTooDeeply

[<Theory>]
[<InlineData(0x2028)>]
[<InlineData(0x2029)>]
let ``Comments ended by a Unicode line or paragraph separator do not hide nesting`` (separator : int) =
    // The grammar ends a comment at these separators, so the braces after them are nested selection sets.
    // The separator is built from its code because Fantomas would write an escape of it out as the raw character.
    parseIsolated ("# c" + string (char separator) + nestedSelections 200)
    |> wantError
    |> assertNestedTooDeeply

[<Fact>]
let ``A quote in a comment does not hide brackets`` () =
    let braces = String.replicate 200 "{"
    parseIsolated $"# \"\n%s{braces}"
    |> wantError
    |> assertNestedTooDeeply

[<Fact>]
let ``Nesting violation reports its line and column`` () =
    Parser.tryFindNestingViolation 2 "{ a { b { c } } }"
    |> equals (ValueSome (struct (1, 9)))
    Parser.tryFindNestingViolation 2 "{\r\n {\r\n  {"
    |> equals (ValueSome (struct (3, 3)))
    Parser.tryFindNestingViolation 2 "{\n{\r{"
    |> equals (ValueSome (struct (3, 1)))
    Parser.tryFindNestingViolation 2 "{ a }\n{ b }\n{ c }"
    |> equals ValueNone

[<Fact>]
let ``Integers out of the 64-bit range are syntax errors`` () =
    let message = Parser.tryParse "{ f(a: 9223372036854775808) }" |> wantError
    Assert.Contains ("The integer 9223372036854775808 is out of the range of 64-bit integers.", message, StringComparison.Ordinal)
    throws<FormatException>(fun () -> Parser.parse "{ f(a: -9223372036854775809) }" |> ignore)
    |> ignore

[<Fact>]
let ``Integers at the ends of the 64-bit range parse`` () =
    let document = Parser.parse "{ f(a: 9223372036854775807, b: -9223372036854775808) }"
    let actual =
        match document.Definitions with
        | [ OperationDefinition operation ] ->
            match operation.SelectionSet with
            | [ Field field ] -> field.Arguments |> List.map _.Value
            | _ -> []
        | _ -> []
    actual
    |> equals [ IntValue Int64.MaxValue; IntValue Int64.MinValue ]
