// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.Relay.CursorTests

#nowarn "40"

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Server.Relay

type Widget = { Id : string; Name : string }

type User = { Id : string; Name : string; Widgets : Widget list }

[<Fact>]
let ``Relay cursor works for types with nested fileds`` () =
    let viewer =
        { Id = "1"
          Name = "Anonymous"
          Widgets =
            [ { Id = "1"; Name = "What's it" }
              { Id = "2"; Name = "Who's it" }
              { Id = "3"; Name = "How's it" } ] }

    let getUser id = if viewer.Id = id then Some viewer else None
    let getWidget id = viewer.Widgets |> List.tryFind (fun w -> w.Id = id)

    let rec Widget =
        Define.Object<Widget> (
            name = "Widget",
            description = "A shiny widget",
            interfaces = [ Node ],
            fields =
                [ Define.GlobalIdField (fun _ w -> w.Id)
                  Define.Field ("name", StringType, (fun _ (w : Widget) -> w.Name)) ]
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
            fields =
                [ Define.GlobalIdField (fun _ w -> w.Id)
                  Define.Field ("name", StringType, (fun _ w -> w.Name))
                  WidgetsField "widgets" (fun _ user -> user) ]
        )

    and Node = Define.Node<obj> (fun () -> [ User; Widget ])

    let Query =
        Define.Object (
            "Query",
            [ Define.NodeField (
                  Node,
                  fun _ () id ->
                      match id with
                      | GlobalId ("User", i) -> getUser i |> Option.map box
                      | GlobalId ("Widget", i) -> getWidget i |> Option.map box
                      | _ -> None
              )
              Define.Field ("viewer", User, (fun _ () -> viewer))
              WidgetsField "widgets" (fun _ () -> viewer) ]
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

    let result = sync <| schemaProcessor.AsyncExecute (parse query)

    match result with
    | Direct (_, errors) -> empty errors
    | _ -> fail "Expected a direct GQLResponse"
