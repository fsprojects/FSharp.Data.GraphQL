/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Relay

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
    { TotalCount : int option
      PageInfo : PageInfo
      Edges : Edge<'Node> seq }

type SliceInfo =
    | Forward of first:int * after:string option
    | Backward of last:int * before:string option

type SliceMetaInfo = { Start: int; Length: int }

[<RequireQualifiedAccess>]
module Cursor =
    [<Literal>]
    let Prefix = "arrayconnection"
    let toOffset defaultValue cursor =
        match cursor with
        | GlobalId(Prefix, id) ->
            match System.Int32.TryParse id with
            | true, num -> num
            | false, _ -> defaultValue
        | _ -> defaultValue
    let ofOffset offset = toGlobalId Prefix (offset.ToString())

[<AutoOpen>]
module Definitions =

    let (|SliceInfo|_|) (ctx:ResolveFieldContext) = 
        match ctx.TryArg "first", ctx.TryArg "after" with
        | Some (Some first), None -> Some (Forward(first, None))
        | Some (Some first), (Some after) -> Some (Forward(first, after))
        | None, _ ->
            match ctx.Arg "last", ctx.TryArg "before" with
            | Some last, None -> Some (Backward(last, None))
            | Some last, Some before -> Some (Backward(last, before))
            | _, _ -> None
        | _ -> None
    
    /// Object defintion representing information about pagination in context of Relay connection
    let PageInfo =
      Define.Object<PageInfo>(
        name = "PageInfo",
        description = "Information about pagination in a connection.",
        fields = [
            Define.Field("hasNextPage", Boolean, "When paginating forwards, are there more items?", fun _ pageInfo -> pageInfo.HasNextPage)
            Define.Field("hasPreviousPage", Boolean, "When paginating backwards, are there more items?", fun _ pageInfo -> pageInfo.HasPreviousPage)
            Define.Field("startCursor", Nullable String, "When paginating backwards, the cursor to continue.", fun _ pageInfo -> pageInfo.StartCursor)
            Define.Field("endCursor", Nullable String, "When paginating forwards, the cursor to continue.", fun _ pageInfo -> pageInfo.EndCursor)
        ])
    
    /// Converts existing output type defintion into an edge in a Relay connection.
    /// <paramref name="nodeType"/> must not be a List.
    let EdgeOf (nodeType: #OutputDef<'Node>) = 
        match nodeType with
        | List _ -> failwith (nodeType.ToString() + " cannot be used as a relay Edge or Connection - only non-list type defintions are allowed")
        | Named n ->
            Define.Object<Edge<'Node>>(
                name = n.Name + "Edge",
                description = "An edge in a connection from an object to another object of type " + n.Name,
                fields = [
                    Define.Field("cursor", String, "A cursor for use in pagination", fun _ edge -> edge.Cursor)
                    Define.Field("node", nodeType, "The item at the end of the edge. Must NOT be an enumerable collection.", fun _ edge -> edge.Node) ])
        | _ -> failwithf "Unexpected value of nodeType: %O" nodeType
    
    /// Converts existing output type definition into Relay-compatible connection.
    /// <paramref name="nodeType"/> must not be a List.
    let ConnectionOf(nodeType: #OutputDef<'Node>) = 
        let n =
            match nodeType with
            | Named n -> n
            | _ -> failwithf "Unexpected value of nodeType: %O" nodeType
        Define.Object<Connection<'Node>>(
            name = n.Name + "Connection",
            description = "A connection from an object to a list of objects of type " + n.Name,
            fields = [
                Define.Field("totalCount", Nullable Int, """A count of the total number of objects in this connection, ignoring pagination. This allows a client to fetch the first five objects by passing \"5\" as the argument to `first`, then fetch the total count so it could display \"5 of 83\", for example. In cases where we employ infinite scrolling or don't have an exact count of entries, this field will return `null`.""", fun _ conn -> conn.TotalCount)
                Define.Field("pageInfo", PageInfo, "Information to aid in pagination.", fun _ conn -> conn.PageInfo)
                Define.Field("edges", ListOf(EdgeOf nodeType), "Information to aid in pagination.", fun _ conn -> conn.Edges)])

[<RequireQualifiedAccess>]
module Connection =

    let forwardArgs = [ 
        Define.Input("first", Nullable Int)
        Define.Input("after", Nullable String) ]
    
    let backwardArgs = [ 
        Define.Input("last", Nullable Int)
        Define.Input("before", Nullable String) ]
    
    let allArgs = forwardArgs @ backwardArgs

    let ofArray array =
        let edges = 
            array
            |> Array.mapi (fun idx elem -> { Cursor = Cursor.ofOffset idx; Node = elem })
        let first = if edges.Length = 0 then None else Some edges.[0].Cursor
        let last = if edges.Length = 0 then None else Some edges.[edges.Length-1].Cursor
        { TotalCount = Array.length array |> Some
          PageInfo = 
            { HasNextPage = false
              HasPreviousPage = false
              StartCursor = first
              EndCursor = last }
          Edges = edges }
    