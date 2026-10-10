// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Server.Relay

open System
open System.Reflection
open System.Text
open System.Text.Unicode
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
    /// <c>ValueSome(typeName, localId)</c> if parsing succeeds, or <c>ValueNone</c> if the format is invalid:
    /// the ID is <see langword="null"/> or empty, is not valid base64, does not decode to UTF-8 text,
    /// or the text has no <c>:</c> separator.
    /// </returns>
    /// <remarks>
    /// <para>Global IDs follow the format: base64("typeName:localId").</para>
    /// <para>
    /// The ID usually comes from the client, so this function never throws on malformed input.
    /// The text is split at the first <c>:</c>, so the local ID may contain further separators.
    /// </para>
    /// </remarks>
    /// <seealso cref="toGlobalId"/>
    let fromGlobalId (id : string) : (string * string) voption =
        // A null ID can only come from a caller that bypasses the non-nullable signature, such as C# code
        if String.IsNullOrEmpty id then
            ValueNone
        else
            // Base64 writes every 3 bytes as 4 characters, so the decoded bytes never outnumber three quarters
            // of the characters; whitespace, which the decoder skips, only makes them fewer
            let bytes = Array.zeroCreate<byte> (id.Length / 4 * 3)
            // TryFromBase64String reports malformed input through its result, while FromBase64String throws
            match Convert.TryFromBase64String (id, bytes.AsSpan ()) with
            | false, _ -> ValueNone
            | true, written ->
                let decoded = ReadOnlySpan<byte> (bytes, 0, written)
                // toGlobalId always writes valid UTF-8, so other bytes are rejected instead of being decoded
                // into U+FFFD replacement characters, which would map different IDs to the same text
                if not (Utf8.IsValid decoded) then
                    ValueNone
                else
                    // ':' is ASCII and UTF-8 never uses ASCII bytes inside a multi-byte sequence,
                    // so the first ':' byte is the first ':' character
                    match decoded.IndexOf (byte ':') with
                    | -1 -> ValueNone
                    | separator ->
                        ValueSome (Encoding.UTF8.GetString (decoded.Slice (0, separator)), Encoding.UTF8.GetString (decoded.Slice (separator + 1)))

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
        /// <para>
        /// The Node interface is part of the Relay Global Object Identification specification.
        /// All objects that can be refetched by ID should implement this interface.
        /// </para>
        /// <para>
        /// The resolver receives the ID exactly as the client sent it. Decode it with <see cref="fromGlobalId"/>,
        /// directly or through the active pattern built on it, which never throws on a malformed ID,
        /// and return <c>None</c> when it yields no value, so that a malformed ID resolves to
        /// a <see langword="null"/> node as the Relay specification expects.
        /// </para>
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
        /// <para>Use this overload when node resolution requires I/O operations (database queries, HTTP calls, etc.).</para>
        /// <para>
        /// The resolver receives the ID exactly as the client sent it. Decode it with <see cref="fromGlobalId"/>,
        /// directly or through the active pattern built on it, which never throws on a malformed ID,
        /// and return <c>None</c> when it yields no value, so that a malformed ID resolves to
        /// a <see langword="null"/> node as the Relay specification expects.
        /// </para>
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
