/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Relay

open FSharp.Data.GraphQL.Types

type Edge<'Node> = 
    { Cursor : string
      Node : 'Node }

type PageInfo = 
    { HasNextPage : bool
      HasPreviousPage : bool
      StartCursor : string option
      EndCursor : string option }

type Connection<'Node> = 
    { TotalCount : int
      PageInfo : PageInfo
      Edges : Edge<'Node> list }
    member x.Items : 'Node list = x.Edges |> List.map (fun e -> e.Node)

let Edge (nodeType: #OutputDef<'Node>) = Define.Object<Edge<'Node>>(
    name = nodeType.ToString() + "Edge",
    description = "An edge in a connection from an object to another object of type " + nodeType.ToString(),
    fields = [
        Define.Field("cursor", String, "A cursor for use in pagination", fun _ edge -> edge.Cursor)
        Define.Field("node", nodeType, "The item at the end of the edge", fun _ edge -> edge.Node)
    ]) 

let PageInfo = Define.Object<PageInfo>(
    name = "PageInfo",
    description = "Information about pagination in a connection.",
    fields = [
        Define.Field("hasNextPage", Boolean, "When paginating forwards, are there more items?", fun _ pageInfo -> pageInfo.HasNextPage)
        Define.Field("hasPreviousPage", Boolean, "When paginating backwards, are there more items?", fun _ pageInfo -> pageInfo.HasPreviousPage)
        Define.Field("startCursor", Nullable String, "When paginating backwards, the cursor to continue.", fun _ pageInfo -> pageInfo.StartCursor)
        Define.Field("endCursor", Nullable String, "When paginating forwards, the cursor to continue.", fun _ pageInfo -> pageInfo.EndCursor)
    ])

let Connection(nodeType: #OutputDef<'Node>) = Define.Object<Connection<'Node>>(
    name = nodeType.ToString(),
    description = "A connection from an object to a list of objects of type " + nodeType.ToString(),
    fields = [
        Define.Field("totalCount", Int, """A count of the total number of objects in this connection, ignoring pagination. This allows a client to fetch the first five objects by passing \"5\" as the argument to `first`, then fetch the total count so it could display \"5 of 83\", for example. In cases where we employ infinite scrolling or don't have an exact count of entries, this field will return `null`.""", fun _ conn -> conn.TotalCount)
        Define.Field("pageInfo", PageInfo, "Information to aid in pagination.", fun _ conn -> conn.PageInfo)
        Define.Field("edges", ListOf(Edge nodeType), "Information to aid in pagination.", fun _ conn -> conn.Edges |> List.toSeq) //TODO: make this toSeq conversion implicit somehow
        Define.Field("items", ListOf(nodeType),
            """A list of all of the objects returned in the connection. This is a convenience field provided for quickly exploring the API; rather than querying for \"{ edges { node } }\" when no edge data is needed, this field can be used instead. Note that when clients like Relay need to fetch the \"cursor\" field on the edge to enable efficient pagination, this shortcut cannot be used, and the full \"{ edges { node } } \" version should be used instead.""",
            fun _ conn -> conn.Items |> List.toSeq) //TODO: make this toSeq conversion implicit somehow
    ])