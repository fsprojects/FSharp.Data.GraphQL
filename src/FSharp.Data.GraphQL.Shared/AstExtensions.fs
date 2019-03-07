module FSharp.Data.GraphQL.Ast.Extensions

open System
open System.Text
open FSharp.Data.GraphQL.Ast
open System.Globalization

[<Flags>]
type internal AppendBehavior =
    | None = 0
    | AddSpaceBefore = 1
    | AddSpaceAfter = 2

type internal PaddedStringBuilder() =
    let sb = StringBuilder()
    let mutable padCount = 0
    member __.Pad() = padCount <- padCount + 2
    member __.Unpad() = padCount <- padCount - 2
    member __.AppendLine() = sb.AppendLine().Append("".PadLeft(padCount, ' ')) |> ignore
    member __.Append(str : string, ?behavior : AppendBehavior) = 
        let behavior = defaultArg behavior AppendBehavior.None
        if behavior.HasFlag(AppendBehavior.AddSpaceBefore) then sb.Append(" ") |> ignore
        sb.Append(str) |> ignore
        if behavior.HasFlag(AppendBehavior.AddSpaceAfter) then sb.Append(" ") |> ignore
    member x.AppendWithSpaceAfter(str) = x.Append(str, AppendBehavior.AddSpaceAfter)
    member x.AppendWithSpaceBefore(str) = x.Append(str, AppendBehavior.AddSpaceBefore)
    override __.ToString() = sb.ToString()

type Document with
    member this.ToQueryString() =
        let sb = PaddedStringBuilder()
        let withQuotes (s : string) = "\"" + s + "\""
        let rec printValue x =
            let printObjectValue (name, value) =
                sb.Append(withQuotes name)
                sb.Append(": ")
                printValue value
            match x with
            | IntValue x -> sb.Append(x.ToString(CultureInfo.InvariantCulture))
            | FloatValue x -> sb.Append(x.ToString(CultureInfo.InvariantCulture))
            | BooleanValue x -> sb.Append(if x then "true" else "false")
            | StringValue x -> sb.Append(withQuotes x)
            | EnumValue x -> sb.Append(x)
            | ListValue x ->
                if x.Length > 0 then sb.Append("[ ")
                match x with
                | [] -> sb.Append("[]")
                | [x] -> printValue x; sb.Append(" ]")
                | x :: xs -> printValue x; sb.Append(", "); List.iter printValue xs
            | ObjectValue x ->
                if x.Count > 0 then sb.Append("{ ")
                match Map.toList x with
                | [] -> sb.Append("null")
                | [x] -> printObjectValue x; sb.Append(" }")
                | x :: xs -> printObjectValue x; sb.Append(", "); List.iter printObjectValue xs
            | Variable x -> sb.Append("$" + x)
        let printVariables (vdefs : VariableDefinition list) =
            let printVariable (vdef : VariableDefinition) =
                sb.Append("$")
                sb.Append(vdef.VariableName)
                sb.Append(": ")
                sb.Append(vdef.Type.ToString())
            if vdefs.Length > 0 then sb.Append("(")
            let rec helper vdefs =
                match vdefs with
                | [] -> ()
                | [vdef] -> printVariable vdef; sb.Append(") ")
                | vdef :: tail -> printVariable vdef; sb.Append(", "); helper tail
            helper vdefs
        let printArguments (arguments : Argument list) =
            let printArgument (arg : Argument) =
                sb.Append(arg.Name + ": ")
                printValue arg.Value
            if arguments.Length > 0 then sb.Append("(")
            let rec helper args =
                match args with
                | [] -> ()
                | [arg] -> printArgument arg; sb.Append(")")
                | arg :: tail -> printArgument arg; sb.Append(", "); helper tail
            helper arguments
        let printDirectives (directives : Directive list) =
            let printDirective (directive : Directive) =
                sb.Append("@" + directive.Name)
                printArguments directive.Arguments
            let rec helper directives =
                match directives with
                | [] -> ()
                | [directive] -> printDirective directive
                | directive :: tail -> printDirective directive; helper tail
            helper directives
        let rec printSelectionSet (selectionSet : Selection list) =
            let printSelection = function
                | Field field ->
                    field.Alias |> Option.iter (fun alias -> sb.Append(alias + ": "))
                    sb.Append(field.Name)
                    printArguments field.Arguments
                    if field.Directives.Length > 0 then sb.Append(" ")
                    printDirectives field.Directives
                    if field.SelectionSet.Length > 0 then sb.Append(" ")
                    printSelectionSet field.SelectionSet
                | FragmentSpread frag ->
                    sb.Append("..." + frag.Name)
                    if frag.Directives.Length > 0 then sb.Append(" ")
                    printDirectives frag.Directives
                | InlineFragment frag ->
                    sb.Append("...")
                    frag.TypeCondition |> Option.iter (fun t -> sb.Append("on " + t))
                    printDirectives frag.Directives
                    sb.Append(" ")
                    printSelectionSet frag.SelectionSet
            if selectionSet.Length > 0 then sb.Append("{"); sb.Pad()
            let rec helper selectionSet =
                match selectionSet with
                | [] -> ()
                | [selection] -> sb.AppendLine(); printSelection selection; sb.Unpad(); sb.AppendLine(); sb.Append("}")
                | selection :: tail -> sb.AppendLine(); printSelection selection; helper tail
            helper selectionSet
        let printOperations = function
            | OperationDefinition odef ->
                match odef.OperationType with
                | Query -> sb.AppendWithSpaceAfter("query")
                | Mutation -> sb.AppendWithSpaceAfter("mutation")
                | Subscription -> sb.AppendWithSpaceAfter("subscription")
                odef.Name 
                |> Option.iter (fun name -> 
                    if odef.VariableDefinitions.Length = 0 
                    then sb.AppendWithSpaceAfter(name) 
                    else sb.Append(name))
                printVariables odef.VariableDefinitions
                printDirectives odef.Directives
                printSelectionSet odef.SelectionSet
            | FragmentDefinition fdef ->
                // TODO: Fragment Definitions must have a name! Fix it on Ast.Document.
                sb.AppendWithSpaceAfter(fdef.Name.Value)
                // TODO: Fragment Definitions must have a type condition! Fix it on Ast.Document.
                sb.AppendWithSpaceAfter("on " + fdef.TypeCondition.Value)
                printDirectives fdef.Directives
                if fdef.Directives.Length > 0 then sb.Append(" ")
                printSelectionSet fdef.SelectionSet
        for def in this.Definitions do printOperations def
        sb.ToString()