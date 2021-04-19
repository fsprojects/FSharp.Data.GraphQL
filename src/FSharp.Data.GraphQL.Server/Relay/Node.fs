/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Relay

open System
open System.Reflection
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

[<AutoOpen>]
module GlobalId =

    /// Tries to parse Relay global node identifier.
    /// Relay identifiers are strings, which follows base64(typeName:nodeId) format.
    let fromGlobalId id =
        let decoded = Text.Encoding.UTF8.GetString(Convert.FromBase64String id)
        match decoded.IndexOf ':' with
        | -1 -> None
        | idx -> Some(decoded.Substring(0, idx), decoded.Substring(idx + 1))

    /// Active pattern over <see cref="fromGlobalId"/> function.
    let (|GlobalId|_|) = fromGlobalId

    /// Tries to parse typeName - id pair into single Relay-compatible global node identifier
    let toGlobalId typeName id = Convert.ToBase64String(Text.Encoding.UTF8.GetBytes(typeName + ":" + id))

    let private resolveTypeFun possibleTypes =
        let map = lazy (
            possibleTypes()
            |> List.map (fun odef ->
                let defType = odef.GetType()
                let trueType = defType.GenericTypeArguments.[0]
                (trueType, odef))
            |> dict)
        fun o ->
            match map.Value.TryGetValue(o.GetType()) with
            | true, defType -> defType
            | false, _ ->
                failwithf "Object of type '%s' was none of the defined types [%A]" (o.GetType().FullName) (map.Value.Keys)

    type FSharp.Data.GraphQL.Types.SchemaDefinitions.Define with

        /// Field definition for the Relay GlobalID field for the chosen type.
        static member GlobalIdField (typeName: string, resolve: (ResolveFieldContext -> 'In -> string)) =
            Define.Field(
                name = "id",
                typedef = ID,
                description = "The ID of an object",
                resolve = fun ctx value -> toGlobalId typeName (resolve ctx value))

        /// Field definition for the Relay GlobalID field for the chosen type.
        static member GlobalIdField (resolve: (ResolveFieldContext -> 'In -> string)) =
            Define.Field(
                name = "id",
                typedef = ID,
                description = "The ID of an object",
                resolve = fun ctx value ->
                    toGlobalId ctx.ParentType.Name (resolve ctx value))

        /// Field definition for the Relay Node interface. Node interfaces are used
        /// by Relay to identify uniqe components - they need to implement and `id`
        /// field which is a globally unique object identifier.
        static member NodeField (nodeDef: InterfaceDef<'Res>, resolve: (ResolveFieldContext -> 'Val -> string -> 'Res option)) =
            Define.Field(
                name = "node",
                typedef = Nullable nodeDef,
                description = "Fetches an object given its ID",
                args = [ Define.Input("id", ID, description = "Identifier of an object") ],
                resolve = fun ctx value ->
                    let id = ctx.Arg("id")
                    resolve ctx value id)

        /// Field definition for the Relay Node interface. Node interfaces are used
        /// by Relay to identify uniqe components - they need to implement and `id`
        /// field which is a globally unique object identifier.
        static member Node (possibleTypes: unit -> ObjectDef list) =
          Define.Interface(
            name = "Node",
            description = "An object that can be uniquely identified by its id",
            fields = [ Define.Field("id", ID<string>) ],
            resolveType = resolveTypeFun possibleTypes)

