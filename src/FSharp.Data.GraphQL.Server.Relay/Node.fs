// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Server.Relay

open System
open System.Reflection
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

/// <summary>
/// Functions for working with Relay Global Object Identification.
/// Implements the Global Object Identification specification from Relay.
/// </summary>
/// <remarks>
/// Global IDs are base64-encoded strings in the format "typeName:localId".
/// This allows Relay clients to uniquely identify any object in the graph.
/// </remarks>
[<AutoOpen>]
module GlobalId =

    /// <summary>
    /// Parses a Relay global node identifier into its component parts.
    /// </summary>
    /// <param name="id">The base64-encoded global ID string.</param>
    /// <returns>
    /// <c>ValueSome(typeName, localId)</c> if parsing succeeds, or <c>ValueNone</c> if the format is invalid.
    /// </returns>
    /// <remarks>
    /// Global IDs follow the format: base64("typeName:localId").
    /// </remarks>
    /// <seealso cref="toGlobalId"/>
    let fromGlobalId id =
        let decoded = Text.Encoding.UTF8.GetString (Convert.FromBase64String id)
        match decoded.IndexOf ':' with
        | -1 -> ValueNone
        | idx -> ValueSome (decoded.Substring (0, idx), decoded.Substring (idx + 1))

    /// <summary>
    /// Active pattern for matching and deconstructing Relay global IDs.
    /// </summary>
    /// <param name="id">The global ID string to match.</param>
    /// <returns>
    /// <c>ValueSome(typeName, localId)</c> if the ID is valid, or <c>ValueNone</c> otherwise.
    /// </returns>
    /// <example>
    /// <code>
    /// match someId with
    /// | GlobalId ("User", localId) -> sprintf "User with ID: %s" localId
    /// | _ -> "Invalid ID"
    /// </code>
    /// </example>
    /// <seealso cref="fromGlobalId"/>
    [<return: Struct>]
    let (|GlobalId|_|) = fromGlobalId

    /// <summary>
    /// Creates a Relay-compatible global node identifier from a type name and local ID.
    /// </summary>
    /// <param name="typeName">The GraphQL type name of the object.</param>
    /// <param name="id">The local identifier for the object within its type.</param>
    /// <returns>A base64-encoded global ID string in the format base64("typeName:id").</returns>
    /// <seealso cref="fromGlobalId"/>
    let toGlobalId typeName id = Convert.ToBase64String (Text.Encoding.UTF8.GetBytes (typeName + ":" + id))

    let private resolveTypeFun possibleTypes =
        let map =
            lazy
                (possibleTypes ()
                 |> List.map (fun odef ->
                     let defType = odef.GetType ()
                     let trueType = defType.GenericTypeArguments.[0]
                     (trueType, odef))
                 |> dict)
        fun o ->
            match map.Value.TryGetValue (o.GetType ()) with
            | true, defType -> defType
            | false, _ -> failwithf "Object of type '%s' was none of the defined types [%A]" (o.GetType().FullName) (map.Value.Keys)

    type FSharp.Data.GraphQL.Types.SchemaDefinitions.Define with

        /// <summary>
        /// Creates a GraphQL field definition for a Relay Global ID with an explicit type name.
        /// </summary>
        /// <param name="typeName">The GraphQL type name to use in the global ID encoding.</param>
        /// <param name="resolve">Function that extracts the local ID from the object.</param>
        /// <typeparam name="In">The .NET type of the parent object.</typeparam>
        /// <returns>A field definition named "id" of type <c>ID!</c> that returns a global ID.</returns>
        /// <seealso cref="toGlobalId"/>
        static member GlobalIdField (typeName : string, resolve : (ResolveFieldContext -> 'In -> string)) =
            Define.Field (
                name = "id",
                typedef = IDType,
                description = "The ID of an object",
                resolve = fun ctx value -> toGlobalId typeName (resolve ctx value)
            )

        /// <summary>
        /// Creates a GraphQL field definition for a Relay Global ID, inferring the type name from context.
        /// </summary>
        /// <param name="resolve">Function that extracts the local ID from the object.</param>
        /// <typeparam name="In">The .NET type of the parent object.</typeparam>
        /// <returns>
        /// A field definition named "id" of type <c>ID!</c> that returns a global ID.
        /// The type name is automatically taken from <c>ctx.ParentType.Name</c>.
        /// </returns>
        /// <seealso cref="toGlobalId"/>
        static member GlobalIdField (resolve : (ResolveFieldContext -> 'In -> string)) =
            Define.Field (
                name = "id",
                typedef = IDType,
                description = "The ID of an object",
                resolve = fun ctx value -> toGlobalId ctx.ParentType.Name (resolve ctx value)
            )

        /// <summary>
        /// Creates a GraphQL field definition for the Relay "node" query field (synchronous version).
        /// </summary>
        /// <param name="nodeDef">The Node interface definition that all returned objects must implement.</param>
        /// <param name="resolve">
        /// Function that fetches an object by its global ID.
        /// Parameters: context, parent value, and global ID string.
        /// Returns <c>Some(object)</c> if found, or <c>None</c> if not found.
        /// </param>
        /// <typeparam name="Res">The type of the resolved node (must implement the Node interface).</typeparam>
        /// <typeparam name="Val">The type of the parent/root value.</typeparam>
        /// <returns>A nullable field definition named "node" that accepts an "id" argument.</returns>
        /// <remarks>
        /// The Node interface is part of the Relay Global Object Identification specification.
        /// All objects that can be refetched by ID should implement this interface.
        /// </remarks>
        /// <seealso cref="NodeAsyncField"/>
        /// <seealso cref="Node"/>
        static member NodeField (nodeDef : InterfaceDef<'Res>, resolve : (ResolveFieldContext -> 'Val -> string -> 'Res option)) =
            Define.Field (
                name = "node",
                typedef = Nullable nodeDef,
                description = "Fetches an object given its ID",
                args = [ Define.Input ("id", IDType, description = "Identifier of an object") ],
                resolve =
                    fun ctx value ->
                        let id = ctx.Arg ("id")
                        resolve ctx value id
            )

        /// <summary>
        /// Creates a GraphQL field definition for the Relay "node" query field (asynchronous version).
        /// </summary>
        /// <param name="nodeDef">The Node interface definition that all returned objects must implement.</param>
        /// <param name="resolve">
        /// Asynchronous function that fetches an object by its global ID.
        /// Parameters: context, parent value, and global ID string.
        /// Returns <c>Async&lt;Some(object)&gt;</c> if found, or <c>Async&lt;None&gt;</c> if not found.
        /// </param>
        /// <typeparam name="Res">The type of the resolved node (must implement the Node interface).</typeparam>
        /// <typeparam name="Val">The type of the parent/root value.</typeparam>
        /// <returns>An async nullable field definition named "node" that accepts an "id" argument.</returns>
        /// <remarks>
        /// Use this overload when node resolution requires I/O operations (database queries, HTTP calls, etc.).
        /// </remarks>
        /// <seealso cref="NodeField"/>
        /// <seealso cref="Node"/>
        static member NodeAsyncField (nodeDef : InterfaceDef<'Res>, resolve : (ResolveFieldContext -> 'Val -> string -> Async<'Res option>)) =
            Define.AsyncField (
                name = "node",
                typedef = Nullable nodeDef,
                description = "Fetches an object given its ID",
                args = [ Define.Input ("id", IDType, description = "Identifier of an object") ],
                resolve =
                    fun ctx value -> async {
                        let id = ctx.Arg ("id")
                        return! resolve ctx value id
                    }
            )

        /// <summary>
        /// Creates the Relay Node interface definition.
        /// </summary>
        /// <param name="possibleTypes">
        /// A function that returns the list of all object types that implement the Node interface.
        /// This is used for runtime type resolution.
        /// </param>
        /// <returns>
        /// An interface definition with a single "id" field of type <c>ID!</c>.
        /// </returns>
        /// <remarks>
        /// The Node interface is the foundation of Relay's Global Object Identification pattern.
        /// Any object that can be uniquely identified and refetched should implement this interface.
        /// The interface requires a single field: "id" which must return a globally unique identifier.
        /// </remarks>
        /// <exception cref="System.Exception">
        /// Thrown during resolution if an object's type is not in the list of possible types.
        /// </exception>
        /// <seealso cref="NodeField"/>
        /// <seealso cref="NodeAsyncField"/>
        /// <seealso cref="GlobalIdField"/>
        static member Node (possibleTypes : unit -> ObjectDef list) =
            Define.Interface (
                name = "Node",
                description = "An object that can be uniquely identified by its id",
                fields = [ Define.Field ("id", IDType) ],
                resolveType = resolveTypeFun possibleTypes
            )
