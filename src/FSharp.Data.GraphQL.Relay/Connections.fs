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
    
    /// Object defintion representing information about pagination in context of Relay connection
    let PageInfo = Define.Object<PageInfo>(
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
    
    /// Converts existing output type definition into Relay-compatible connection.
    /// <paramref name="nodeType"/> must not be a List.
    let ConnectionOf(nodeType: #OutputDef<'Node>) = 
        let (Named n) = nodeType
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
    
    let ofArraySlice meta (SliceInfo args) (array: 't []) = 
        let startOffset, endOffset =
            match args with
            | Forward(first, after) -> 
                let start = match after with None -> 0 | Some a -> (Cursor.toOffset meta.Start a) - meta.Start
                (start, start + first)
            | Backward(last, before) -> 
                let finish = match before with None -> array.Length | Some b -> (Cursor.toOffset (array.Length + meta.Start) b) - meta.Start
                (finish - last, finish)
        let slice = Array.sub array startOffset (System.Math.Min(endOffset, array.Length) - startOffset)
        let edges = slice |> Array.mapi (fun idx value -> { Cursor = Cursor.ofOffset (idx + startOffset + meta.Start); Node = value })
        let edgeHead = edges |> Array.tryHead
        let edgeTail = edges |> Array.tryLast
        { TotalCount = None
          Edges = edges
          PageInfo = 
            { HasNextPage = endOffset <> meta.Length
              HasPreviousPage = startOffset <> meta.Start
              StartCursor = edgeHead |> Option.map (fun e -> e.Cursor)
              EndCursor = edgeTail |> Option.map (fun e -> e.Cursor) }}
    
    let ofArray args (array: 't []) = ofArraySlice { Start = 0; Length = array.Length } args array
    
    let ofList args list = ofArray args (Array.ofList list)
    
    let ofListSlice meta args list = ofArraySlice meta args (Array.ofList list)
    
    let ofSeq args seq = ofArray args (Array.ofSeq seq)
    
    let ofSeqSlice meta args seq = ofArraySlice meta args (Array.ofSeq seq)