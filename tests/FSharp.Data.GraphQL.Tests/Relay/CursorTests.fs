// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.Relay.CursorTests

#nowarn "40"

open System
open System.Collections.Immutable
open System.Text.Json
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Server.Relay
open FSharp.Data.GraphQL.Shared

type Widget = { Id : string; Name : string }

type User = { Id : string; Name : string; Widgets : Widget list }

[<Fact>]
let ``Relay cursor works for types with nested fileds`` () =
    let viewer = {
        Id = "1"
        Name = "Anonymous"
        Widgets = [
            { Id = "1"; Name = "What's it" }
            { Id = "2"; Name = "Who's it" }
            { Id = "3"; Name = "How's it" }
        ]
    }

    let tryGetUser id = if viewer.Id = id then Some viewer else None
    let tryGetWidget id = viewer.Widgets |> List.tryFind (fun w -> w.Id = id)

    let rec Widget =
        Define.Object<Widget> (
            name = "Widget",
            description = "A shiny widget",
            interfaces = [ Node ],
            fields = [
                Define.GlobalIdField (fun _ w -> w.Id)
                Define.Field ("name", StringType, (fun _ (w : Widget) -> w.Name))
            ]
        )

    and WidgetsField name (getUser : ResolveFieldContext -> 'a -> User) =
        let resolve ctx xx =
            let user = getUser ctx xx
            let widgets = user.Widgets |> List.toArray
            Connection.ofArray widgets

        Define.Field (name, ConnectionOf Widget, "A person's collection of widgets", Connection.allArgs, resolve)

    and User =
        Define.Object<User> (
            name = "User",
            description = "A person who uses our app",
            interfaces = [ Node ],
            fields = [
                Define.GlobalIdField (fun _ w -> w.Id)
                Define.Field ("name", StringType, (fun _ w -> w.Name))
                WidgetsField "widgets" (fun _ user -> user)
            ]
        )

    and Node = Define.Node<obj> (fun () -> [ User; Widget ])

    let Query =
        Define.Object (
            "Query",
            [
                Define.NodeField (
                    Node,
                    fun _ () id ->
                        match id with
                        | GlobalId ("User", i) -> tryGetUser i |> Option.map box
                        | GlobalId ("Widget", i) -> tryGetWidget i |> Option.map box
                        | _ -> None
                )
                Define.Field ("viewer", User, (fun _ () -> viewer))
                WidgetsField "widgets" (fun _ () -> viewer)
            ]
        )

    let schema = Schema (query = Query, config = { SchemaConfig.Default with Types = [ User; Widget ] })
    let schemaProcessor = Executor (schema)

    let query =
        """{
            viewer {
                name
            }
            widgets {
                edges { cursor }
            }
        }"""

    let result = sync <| schemaProcessor.AsyncExecute (parse query, getMockInputContext)

    match result with
    | Direct (_, errors) -> empty errors
    | response -> fail $"Expected a 'Direct' GQLResponse but got\n{response}"

/// Encodes the text the way a client forging a cursor by hand would.
let private toBase64 (text : string) = Convert.ToBase64String (Text.Encoding.UTF8.GetBytes text)

// No cursor below decodes to this value, so getting it back means the cursor was rejected
[<Literal>]
let private DefaultOffset = 777

/// Asserts that both decoding functions reject the cursor: tryToOffset with no value and toOffset with the default value.
let private rejectsCursor (cursor : string) (description : string) =
    match Cursor.tryToOffset cursor with
    | ValueNone -> ()
    | ValueSome offset -> fail $"Expected tryToOffset to return no value for a cursor with {description}, but it read the offset {offset}"
    let offset = Cursor.toOffset DefaultOffset cursor
    Assert.True (
        (offset = DefaultOffset),
        $"Expected toOffset to return the default offset {DefaultOffset} for a cursor with {description}, but got {offset}"
    )

// A cursor comes from the client, so whatever it sends must be rejected without an exception
[<Theory>]
[<InlineData("", "an empty string")>]
[<InlineData(" ", "only a space")>]
[<InlineData("%%%%", "characters outside the base64 alphabet")>]
[<InlineData("YXJyYXljb25uZWN0aW9uOjE", "a length that is not a multiple of four")>]
[<InlineData("====", "nothing but padding")>]
[<InlineData("Привіт==", "non-ASCII characters")>]
let ``Cursor decoding rejects a cursor that is not base64`` (cursor : string, reason : string) =
    rejectsCursor cursor $"{reason} ('{cursor}')"

[<Theory>]
[<InlineData("arrayconnection:-1", "a negative number")>]
[<InlineData("arrayconnection:-0", "a negative zero")>]
[<InlineData("arrayconnection:+1", "an explicit sign")>]
[<InlineData("arrayconnection:2147483648", "a number that overflows Int32")>]
[<InlineData("arrayconnection:99999999999999999999", "a number that overflows Int64")>]
[<InlineData("arrayconnection:١٢", "Arabic-Indic digits")>]
[<InlineData("arrayconnection:１２", "full-width digits")>]
[<InlineData("arrayconnection: 1", "a leading space")>]
[<InlineData("arrayconnection:1 ", "a trailing space")>]
[<InlineData("arrayconnection:\t1", "a leading tab")>]
[<InlineData("arrayconnection:1\n", "a trailing line feed")>]
[<InlineData("arrayconnection:1.0", "a decimal point")>]
[<InlineData("arrayconnection:1,000", "a group separator")>]
[<InlineData("arrayconnection:1e3", "an exponent")>]
[<InlineData("arrayconnection:0x10", "a hexadecimal prefix")>]
[<InlineData("arrayconnection:", "no number")>]
[<InlineData("arrayconnection", "no separator")>]
[<InlineData("ARRAYCONNECTION:1", "the prefix in another case")>]
[<InlineData("person:1", "another prefix")>]
let ``Cursor decoding rejects a malformed offset`` (payload : string, reason : string) =
    rejectsCursor (toBase64 payload) $"{reason} ('{payload}')"

[<Theory>]
[<InlineData(0)>]
[<InlineData(1)>]
[<InlineData(42)>]
[<InlineData(2147483647)>]
let ``Cursor decoding reads back the offset ofOffset wrote`` (offset : int) =
    let cursor = Cursor.ofOffset offset
    Assert.Equal (ValueSome offset, Cursor.tryToOffset cursor)
    Assert.Equal (offset, Cursor.toOffset DefaultOffset cursor)

let letters = [| "a"; "b"; "c"; "d" |]

/// Pages through the letters forward, using array offsets as cursors.
let resolveLetters (ctx : ResolveFieldContext) () =
    let start =
        match ctx.TryArg "after" with
        | ValueNone -> 0
        | ValueSome cursor ->
            match Cursor.tryToOffset cursor with
            // A malformed cursor is the client's mistake, so it is reported as a GraphQL error
            | ValueNone -> raise (GQLMessageException "Invalid cursor")
            // The offset may be as large as Int32.MaxValue, so it is compared before incrementing to avoid an overflow
            | ValueSome offset when offset < letters.Length -> offset + 1
            | ValueSome _ -> letters.Length
    let first = ctx.TryArg "first" |> ValueOption.defaultValue letters.Length
    let edges =
        letters
        |> Array.skip start
        |> Array.truncate first
        |> Array.mapi (fun index letter -> { Cursor = Cursor.ofOffset (start + index); Node = letter })
    Some {
        TotalCount = async { return Some letters.Length }
        PageInfo = {
            HasNextPage = async { return start + edges.Length < letters.Length }
            HasPreviousPage = async { return start > 0 }
            StartCursor = async { return edges |> Array.tryHead |> Option.map _.Cursor }
            EndCursor = async { return edges |> Array.tryLast |> Option.map _.Cursor }
        }
        Edges = async { return Seq.ofArray edges }
    }

let lettersSchema =
    Schema (
        Define.Object (
            "Query",
            [
                Define.Field (
                    "letters",
                    Nullable (ConnectionOf StringType),
                    "Letters paged forward by array offset cursors",
                    Connection.forwardArgs,
                    resolveLetters
                )
            ]
        )
    )

let executeLetters (after : string) =
    // The cursor goes through a variable, so that any string reaches the resolver unchanged by GraphQL string escaping
    let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("after", JsonSerializer.SerializeToElement after)
    Executor(lettersSchema).AsyncExecute (
        "query ($after: String) { letters(after: $after) { edges { node } } }",
        getMockInputContext,
        variables = variables
    )
    |> sync

[<Fact>]
let ``Connection field pages after a cursor ofOffset wrote`` () =
    let result = executeLetters (Cursor.ofOffset 1)
    ensureDirect result <| fun data errors ->
        empty errors
        data
        |> equals (
            upcast NameValueLookup.ofList [
                "letters", upcast NameValueLookup.ofList [
                    "edges", upcast [
                        box <| NameValueLookup.ofList [ "node", upcast "c" ]
                        upcast NameValueLookup.ofList [ "node", upcast "d" ]
                    ]
                ]
            ]
        )

[<Theory>]
[<InlineData("", "an empty string")>]
[<InlineData("%%%%", "characters outside the base64 alphabet")>]
[<InlineData("Привіт==", "non-ASCII characters")>]
// arrayconnection
[<InlineData("YXJyYXljb25uZWN0aW9u", "no separator")>]
// arrayconnection:-2
[<InlineData("YXJyYXljb25uZWN0aW9uOi0y", "a negative offset")>]
// arrayconnection: 1
[<InlineData("YXJyYXljb25uZWN0aW9uOiAx", "a leading space in the offset")>]
// arrayconnection:2147483648
[<InlineData("YXJyYXljb25uZWN0aW9uOjIxNDc0ODM2NDg=", "an offset that overflows Int32")>]
let ``Connection field reports a GraphQL error for a malformed after cursor`` (cursor : string, reason : string) =
    let result = executeLetters cursor
    ensureDirect result <| fun data errors ->
        match errors with
        | [ error ] when error.Message = "Invalid cursor" -> ()
        | _ -> fail $"Expected the single error 'Invalid cursor' for a cursor with {reason} ('{cursor}'), but got %A{errors}"
        errors |> hasErrorAtPath [ box "letters" ] "Invalid cursor"
        data |> equals (upcast NameValueLookup.ofList [ "letters", null ])
