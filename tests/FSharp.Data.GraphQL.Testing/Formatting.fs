/// Renders the parts of the failure messages of Assert and TestDiscoveryGuard.
[<RequireQualifiedAccess>]
module internal FSharp.Data.GraphQL.Testing.Formatting

open System
open System.Text

/// Renders a value the way F# Interactive prints it (the %A format), which shows records, unions, options and collections structurally.
let render (value : 'T) : string = $"%A{value}"

/// Renders a type name the way it reads in source: generic arguments in angle brackets, no arity suffix, no namespace.
let rec typeName (type' : Type) : string =
    if type'.IsArray then
        $"{typeName (nonNull (type'.GetElementType ()))}[]"
    elif type'.IsGenericType then
        let name = type'.Name
        let name =
            match name.IndexOf ('`', StringComparison.Ordinal) with
            | -1 -> name
            | tick -> name.Substring (0, tick)
        let arguments =
            type'.GetGenericArguments ()
            |> Seq.map typeName
            |> String.concat ", "
        $"{name}<{arguments}>"
    else
        type'.Name

/// <summary>
/// Lays out labelled values one per line like the <c>expected:</c> and <c>actual:</c> lines of MSTest's own failures: the
/// labels padded to one width, the continuation lines of a multi-line value indented under its first line.
/// </summary>
let details (lines : struct (string * string) list) : string =
    match lines with
    | [] -> ""
    | lines ->
        let width =
            lines
            |> List.map (fun struct (label, _) -> label.Length)
            |> List.max
        lines
        |> Seq.map (fun struct (label, text) ->
            let prefix = $"{label}:".PadRight(width + 2)
            prefix
            + text.ReplaceLineEndings (Environment.NewLine + String (' ', prefix.Length)))
        |> String.concat Environment.NewLine

/// <summary>
/// The details of a failed equality check: the expected and the actual value. When both render to the same text, a note
/// says so, because the reader would otherwise see two identical values under a failure.
/// </summary>
let comparison (expected : string) (actual : string) : string =
    let lines = details [ struct ("expected", expected); struct ("actual", actual) ]
    if String.Equals (expected, actual, StringComparison.Ordinal) then
        lines
        + Environment.NewLine
        + "(Both values render the same, so they differ in something the rendering does not show, "
        + "such as float digits beyond the printed precision or a member that is compared by reference.)"
    else
        lines

/// <summary>
/// Joins the parts of a failure message the way MSTest 4 lays out its own failures, below the <c>Assertion failed.</c>
/// line <c>Assert.Fail</c> adds: the description of what was expected, the caller's message on the next line,
/// then the details after a blank line.
/// </summary>
let failureMessage (summary : string) (message : string | null) (details : string) : string =
    let text = StringBuilder summary
    match message with
    | null -> ()
    | message when String.IsNullOrWhiteSpace message -> ()
    | message -> text.AppendLine().Append(message) |> ignore
    if details.Length > 0 then
        text.AppendLine().AppendLine().Append(details) |> ignore
    text.ToString ()
