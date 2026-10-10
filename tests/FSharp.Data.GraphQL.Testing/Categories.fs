/// <summary>
/// The test category names the attributes such as <see cref="EditionOct2021Attribute"/> add to a test, for
/// <c>[&lt;TestCategory (...)&gt;]</c> and for filters such as <c>dotnet test --filter TestCategory=Sep2025</c>.
/// <para>
/// The names hold no spaces, so a filter needs no quoting: an edition is the month and year of the GraphQL specification
/// release, an area is the part of the specification or the kind of test.
/// </para>
/// </summary>
[<RequireQualifiedAccess>]
module FSharp.Data.GraphQL.Testing.Categories

/// The October 2021 edition of the GraphQL specification.
[<Literal>]
let Oct2021 = "Oct2021"

/// The September 2025 edition of the GraphQL specification.
[<Literal>]
let Sep2025 = "Sep2025"

/// Lexical analysis: source characters, ignored tokens, punctuators, names and numbers.
[<Literal>]
let Lexical = "Lexical"

/// String values: quoted and block strings, escape sequences and block string indentation.
[<Literal>]
let Strings = "Strings"

/// Input values: scalar literals, lists, input objects, variables and constant values.
[<Literal>]
let Values = "Values"

/// Executable definitions: operations, selection sets, fields, arguments, fragments and directives.
[<Literal>]
let Executable = "Executable"

/// Type system definitions and extensions: schemas, types, fields, arguments and directive definitions.
[<Literal>]
let TypeSystem = "TypeSystem"

/// Inputs at the limits: very large, very long or deeply nested documents.
[<Literal>]
let Stress = "Stress"

/// Printing a document back to GraphQL source and the round trip through parsing and printing.
[<Literal>]
let Printer = "Printer"

/// Real-world documents and test corpora collected from other GraphQL implementations.
[<Literal>]
let Corpus = "Corpus"

/// Malformed, truncated or hostile input that must end in an error rather than a crash or a hang.
[<Literal>]
let Robustness = "Robustness"

/// <summary>Tests that take noticeably longer than the rest, so that a quick run can skip them with <c>TestCategory!=Slow</c>.</summary>
[<Literal>]
let Slow = "Slow"
