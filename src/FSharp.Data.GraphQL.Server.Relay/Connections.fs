// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Server.Relay

open System.Runtime.CompilerServices
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns

/// <summary>
/// Represents a Relay edge – an object with a cursor and a node.
/// Edges are used to traverse connections in Relay pagination.
/// </summary>
/// <typeparam name="Node">The type of the node at the end of this edge.</typeparam>
[<Struct; IsReadOnly>]
type Edge<'Node> = {
    /// <summary>Opaque cursor string used to identify this node's position in the connection.</summary>
    Cursor : string
    /// <summary>The object at the end of this edge. Must satisfy the Relay Node interface.</summary>
    Node : 'Node
}

/// <summary>
/// Information about pagination in a Relay connection.
/// Follows the Relay Cursor Connections Specification.
/// </summary>
/// <remarks>
/// PageInfo provides pagination metadata that allows clients to determine
/// whether more pages are available and where to continue pagination.
/// </remarks>
type PageInfo = {
    /// <summary>
    /// Indicates whether more items exist following the current page when paginating forward.
    /// Returns <c>false</c> if this is the last page.
    /// </summary>
    HasNextPage : Async<bool>
    /// <summary>
    /// Indicates whether more items exist before the current page when paginating backward.
    /// Returns <c>false</c> if this is the first page.
    /// </summary>
    HasPreviousPage : Async<bool>
    /// <summary>
    /// The cursor corresponding to the first edge in the current page.
    /// Returns <c>None</c> if the page is empty.
    /// </summary>
    StartCursor : Async<string option>
    /// <summary>
    /// The cursor corresponding to the last edge in the current page.
    /// Returns <c>None</c> if the page is empty.
    /// </summary>
    EndCursor : Async<string option>
}

/// <summary>
/// Represents a Relay connection – a paginated set of edges with metadata.
/// Follows the Relay Cursor Connections Specification.
/// </summary>
/// <typeparam name="Node">The type of nodes contained in this connection.</typeparam>
/// <remarks>
/// Unlike traditional offset-based pagination, Relay connections use cursors
/// to navigate through results, allowing the result set to change between requests
/// while maintaining consistent pagination behavior.
/// </remarks>
type Connection<'Node> = {
    /// <summary>
    /// The total count of items in the entire result set, ignoring pagination.
    /// Returns <c>None</c> when the count is not available or would be too expensive to compute.
    /// </summary>
    TotalCount : Async<int option>
    /// <summary>
    /// Metadata about the current page, including cursors and availability of adjacent pages.
    /// </summary>
    /// <seealso cref="PageInfo"/>
    PageInfo : PageInfo
    /// <summary>
    /// The list of edges in the current page. Each edge contains a cursor and a node.
    /// </summary>
    /// <seealso cref="Edge{T}"/>
    Edges : Async<Edge<'Node> seq>
}
//    interface seq<'Node> with
//        member x.GetEnumerator () = (Seq.map (fun edge -> edge.Node) x.Edges).GetEnumerator()
//        member x.GetEnumerator () : System.Collections.IEnumerator = upcast (x :> seq<'Node>).GetEnumerator()

/// <summary>
/// Describes pagination direction and parameters for slicing a Relay connection.
/// </summary>
/// <typeparam name="Cursor">The type used to represent cursor values.</typeparam>
/// <remarks>
/// SliceInfo encapsulates the "first/after" (forward) or "last/before" (backward)
/// pagination arguments as defined in the Relay specification.
/// </remarks>
type SliceInfo<'Cursor> =
    /// <summary>
    /// Forward pagination: retrieve the first N items after a given cursor.
    /// If <c>After</c> is <c>ValueNone</c>, starts from the beginning of the result set.
    /// </summary>
    | Forward of First : int * After : 'Cursor voption
    /// <summary>
    /// Backward pagination: retrieve the last N items before a given cursor.
    /// If <c>Before</c> is <c>ValueNone</c>, retrieves the last N items from the end of the result set.
    /// </summary>
    | Backward of Last : int * Before : 'Cursor voption

    member this.PageSize =
        match this with
        | Forward (first, _) -> first
        | Backward (last, _) -> last

    member this.Cursor =
        match this with
        | Forward (_, after) -> after
        | Backward (_, before) -> before

[<AutoOpen>]
module Definitions =

    /// <summary>
    /// Active pattern that extracts Relay pagination arguments from a GraphQL field context.
    /// </summary>
    /// <param name="ctx">The GraphQL field resolution context.</param>
    /// <returns>
    /// <c>ValueSome(Forward)</c> if "first" argument is present (with optional "after"),
    /// <c>ValueSome(Backward)</c> if "last" argument is present (with optional "before"),
    /// or <c>ValueNone</c> if no pagination arguments are found.
    /// </returns>
    /// <seealso cref="SliceInfo{T}"/>
    [<return: Struct>]
    let (|SliceInfo|_|) (ctx : ResolveFieldContext) =
        match ctx.TryArg "first", ctx.TryArg "after" with
        | ValueSome (first), ValueNone -> ValueSome (Forward (first, ValueNone))
        | ValueSome (first), (after) -> ValueSome (Forward (first, after))
        | ValueNone, _ ->
            match ctx.TryArg "last", ctx.TryArg "before" with
            | ValueSome (last), ValueNone -> ValueSome (Backward (last, ValueNone))
            | ValueSome (last), (before) -> ValueSome (Backward (last, before))
            | _, _ -> ValueNone

    /// <summary>
    /// GraphQL object type definition for <see cref="PageInfo"/>.
    /// Defines the schema for pagination metadata in Relay connections.
    /// </summary>
    let PageInfo =
        Define.Object<PageInfo> (
            name = "PageInfo",
            description = "Information about pagination in a connection.",
            fields = [
                Define.AsyncField (
                    "hasNextPage",
                    BooleanType,
                    "When paginating forwards, are there more items?",
                    fun _ pageInfo -> pageInfo.HasNextPage
                )
                Define.AsyncField (
                    "hasPreviousPage",
                    BooleanType,
                    "When paginating backwards, are there more items?",
                    fun _ pageInfo -> pageInfo.HasPreviousPage
                )
                Define.AsyncField (
                    "startCursor",
                    Nullable StringType,
                    "When paginating backwards, the cursor to continue.",
                    fun _ pageInfo -> pageInfo.StartCursor
                )
                Define.AsyncField (
                    "endCursor",
                    Nullable StringType,
                    "When paginating forwards, the cursor to continue.",
                    fun _ pageInfo -> pageInfo.EndCursor
                )
            ]
        )

    /// <summary>
    /// Creates a GraphQL object type definition for a Relay edge wrapping the specified node type.
    /// </summary>
    /// <param name="nodeType">The GraphQL output type definition for nodes. Must not be a list type.</param>
    /// <typeparam name="Node">The .NET type of nodes in the edge.</typeparam>
    /// <returns>An <c>ObjectDef&lt;Edge&lt;'Node&gt;&gt;</c> with "cursor" and "node" fields.</returns>
    /// <exception cref="System.Exception">Thrown if <paramref name="nodeType"/> is a list type.</exception>
    /// <seealso cref="Edge{T}"/>
    /// <seealso cref="ConnectionOf"/>
    let EdgeOf (nodeType : #OutputDef<'Node>) =
        match nodeType with
        | List _ ->
            failwith $"{nodeType.ToString ()} cannot be used as a relay Edge or Connection – only non-list type definitions are allowed"
        | Named n ->
            Define.Object<Edge<'Node>> (
                name = n.Name + "Edge",
                description = $"An edge in a connection from an object to another object of type {n.Name}",
                fields = [
                    Define.Field ("cursor", StringType, "A cursor for use in pagination", (fun _ edge -> edge.Cursor))
                    Define.Field (
                        "node",
                        nodeType,
                        "The item at the end of the edge. Must NOT be an enumerable collection.",
                        fun _ edge -> edge.Node
                    )
                ]
            )
        | _ -> failwithf "Unexpected value of nodeType: %O" nodeType

    /// <summary>
    /// Creates a GraphQL object type definition for a Relay connection containing the specified node type.
    /// </summary>
    /// <param name="nodeType">The GraphQL output type definition for nodes. Must not be a list type.</param>
    /// <typeparam name="Node">The .NET type of nodes in the connection.</typeparam>
    /// <returns>
    /// An <c>ObjectDef&lt;Connection&lt;'Node&gt;&gt;</c> with "totalCount", "pageInfo", and "edges" fields.
    /// </returns>
    /// <exception cref="System.Exception">Thrown if <paramref name="nodeType"/> is a list type.</exception>
    /// <seealso cref="Connection{T}"/>
    /// <seealso cref="EdgeOf"/>
    let ConnectionOf (nodeType : #OutputDef<'Node>) =
        let n =
            match nodeType with
            | Named n -> n
            | _ -> failwithf "Unexpected value of nodeType: %O" nodeType
        Define.Object<Connection<'Node>> (
            name = n.Name + "Connection",
            description =
                "A connection from an object to a list of objects of type "
                + n.Name,
            fields = [
                Define.AsyncField (
                    "totalCount",
                    Nullable IntType,
                    """A count of the total number of objects in this connection, ignoring pagination. This allows a client to fetch the first five objects by passing \"5\" as the argument to `first`, then fetch the total count so it could display \"5 of 83\", for example. In cases where we employ infinite scrolling or don't have an exact count of entries, this field will return `null`.""",
                    fun _ conn -> conn.TotalCount
                )
                Define.Field ("pageInfo", PageInfo, "Information to aid in pagination.", (fun _ conn -> conn.PageInfo))
                Define.AsyncField ("edges", ListOf (EdgeOf nodeType), "Information to aid in pagination.", (fun _ conn -> conn.Edges))
            ]
        )

[<RequireQualifiedAccess>]
module Cursor =
    /// <summary>Prefix used for array-based connection cursors when encoding as Global IDs.</summary>
    [<Literal>]
    let Prefix = "arrayconnection"

    /// <summary>
    /// Decodes a cursor string to an integer offset.
    /// </summary>
    /// <param name="defaultValue">The value to return if decoding fails.</param>
    /// <param name="cursor">The cursor string to decode (expected to be a Global ID).</param>
    /// <returns>The decoded offset, or <paramref name="defaultValue"/> if parsing fails.</returns>
    let toOffset defaultValue cursor =
        match cursor with
        | GlobalId (Prefix, id) ->
            match System.Int32.TryParse id with
            | true, num -> num
            | false, _ -> defaultValue
        | _ -> defaultValue

    /// <summary>
    /// Encodes an integer offset as a cursor string using the Global ID format.
    /// </summary>
    /// <param name="offset">The zero-based array offset to encode.</param>
    /// <returns>An opaque cursor string suitable for use in Relay pagination.</returns>
    let ofOffset offset = toGlobalId Prefix (offset.ToString ())

module Edge =

    /// <summary>
    /// Transforms the node in an edge while preserving the cursor.
    /// </summary>
    /// <param name="mapping">The function to transform the node from type <typeparamref name="T"/> to type <typeparamref name="U"/>.</param>
    /// <param name="edge">The edge to transform.</param>
    /// <typeparam name="T">The type of the node in the source edge.</typeparam>
    /// <typeparam name="U">The type of the node in the resulting edge.</typeparam>
    /// <returns>A new edge with the transformed node and the same cursor.</returns>
    let map mapping (edge : Edge<'T>) : Edge<'U> =
        { Cursor = edge.Cursor; Node = mapping edge.Node }

[<RequireQualifiedAccess>]
module Connection =

    /// <summary>
    /// Transforms a <see cref="Connection{T}"/> into a <see cref="Connection{U}"/>
    /// by applying a mapping function to each node while preserving cursor information and pagination metadata.
    /// </summary>
    /// <param name="mapping">The function to transform nodes from type <typeparamref name="T"/> to type <typeparamref name="U"/>.</param>
    /// <param name="conn">The source connection to transform.</param>
    /// <typeparam name="T">The type of nodes in the source connection.</typeparam>
    /// <typeparam name="U">The type of nodes in the resulting connection.</typeparam>
    /// <returns>A new connection with transformed nodes. Cursors, page info, and total count are preserved unchanged.</returns>
    /// <seealso cref="Edge.map"/>
    let map mapping (conn : Connection<'T>) : Connection<'U> =
        {
            TotalCount = conn.TotalCount
            PageInfo = conn.PageInfo
            Edges = async {
                let! edges = conn.Edges
                return edges |> Seq.map (Edge.map mapping)
            }
        }

    /// <summary>
    /// Argument definitions for forward pagination ("first" and "after").
    /// Use these when defining GraphQL fields that support forward-only pagination.
    /// </summary>
    /// <seealso cref="backwardArgs"/>
    /// <seealso cref="allArgs"/>
    let forwardArgs =
        [ Define.Input ("first", Nullable IntType)
          Define.Input ("after", Nullable StringType) ]

    /// <summary>
    /// Argument definitions for backward pagination ("last" and "before").
    /// Use these when defining GraphQL fields that support backward-only pagination.
    /// </summary>
    /// <seealso cref="forwardArgs"/>
    /// <seealso cref="allArgs"/>
    let backwardArgs =
        [ Define.Input ("last", Nullable IntType)
          Define.Input ("before", Nullable StringType) ]

    /// <summary>
    /// Complete set of argument definitions for bidirectional pagination.
    /// Combines <see cref="forwardArgs"/> and <see cref="backwardArgs"/>.
    /// Use these when defining GraphQL fields that support both forward and backward pagination.
    /// </summary>
    let allArgs = forwardArgs @ backwardArgs

    /// <summary>
    /// Creates a Relay connection from an array of nodes using array indices as cursors.
    /// </summary>
    /// <param name="array">The array of nodes to convert into a connection.</param>
    /// <typeparam name="Node">The type of nodes in the array.</typeparam>
    /// <returns>
    /// A <see cref="Connection{T}"/> containing all items from the array.
    /// The <see cref="PageInfo"/> indicates this is a complete result set (no adjacent pages).
    /// </returns>
    /// <remarks>
    /// This is a convenience function for simple scenarios. For proper pagination,
    /// use slicing based on <see cref="SliceInfo{T}"/>.
    /// </remarks>
    let ofArray array =
        let edges =
            array
            |> Array.mapi (fun idx elem -> { Cursor = Cursor.ofOffset idx; Node = elem })
        let first = if edges.Length = 0 then None else Some edges.[0].Cursor
        let last =
            if edges.Length = 0 then
                None
            else
                Some edges.[edges.Length - 1].Cursor
        {
            TotalCount = async { return Array.length array |> Some }
            PageInfo = {
                HasNextPage = async { return false }
                HasPreviousPage = async { return false }
                StartCursor = async { return first }
                EndCursor = async { return last }
            }
            Edges = async { return edges }
        }
