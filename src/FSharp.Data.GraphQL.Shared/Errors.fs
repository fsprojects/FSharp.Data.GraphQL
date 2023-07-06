namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Text.Json.Serialization
open Microsoft.FSharp.Core.LanguagePrimitives

type internal FieldPath = obj list

type IGQLError =
    abstract member Message : string with get

type IGQLErrorExtensions =
    abstract member Extensions : IReadOnlyDictionary<string, obj> voption with get

[<Struct>]
type GQLProblemLocation = { Line : int; Column : int }

/// <summary>
/// A machine-readable format for specifying errors in GraphQL API responses based on <see href="http://spec.graphql.org/October2021/#sec-Errors"/>.
/// </summary>
//[<JsonConverter(typeof(ProblemDetailsJsonConverter))>]
type GQLProblemDetails =
    {
        /// <summary>
        /// A short, human-readable summary of the problem type. It SHOULD NOT change from occurrence to occurrence
        /// of the problem, except for purposes of localization.
        /// </summary>
        [<JsonPropertyName("message")>]
        Message : string

        /// <summary>
        /// An array of fields path segments that that identify the specific field path in a GraphQL query where the resolution problem occurs.
        /// </summary>
        [<JsonPropertyName("path")>]
        Path : FieldPath Skippable

        /// <summary>
        /// An array of line and columnt pairs that identify the specific positions in a GraphQL query where the problem occurs.
        /// </summary>
        [<JsonPropertyName("locations")>]
        Locations : GQLProblemLocation list Skippable

        /// <summary>
        /// Gets the <see cref="IDictionary{TKey, TValue}"/> for extension members.
        /// <para>
        /// Problem type definitions MAY extend the problem details object with additional members. Extension members appear in the same namespace as
        /// other members of a problem type.
        /// </para>
        /// </summary>
        /// <remarks>
        /// The round-tripping behavior for <see cref="Extensions"/> is determined by the implementation of the Input \ Output formatters.
        /// In particular, complex types or collection types may not round-trip to the original type when using the built-in JSON or XML formatters.
        /// </remarks>
        [<JsonExtensionData>]
        Extensions : IReadOnlyDictionary<string, obj> Skippable
    }

    static member Create (message, ?extensions : IReadOnlyDictionary<string, obj>) =
        { Message = message
          Path = Skip
          Locations = Skip
          Extensions = extensions |> Skippable.ofOption }

    static member Create (message, extensions) = { Message = message; Path = Skip; Locations = Skip; Extensions = extensions }

    static member Create (message, path, ?extensions : IReadOnlyDictionary<string, obj>) =
        { Message = message
          Path = Include path
          Locations = Skip
          Extensions = extensions |> Skippable.ofOption }

    static member Create (message, path, extensions) =
        { Message = message
          Path = Include path
          Locations = Skip
          Extensions = extensions }

    static member Create (message, locations, ?extensions : IReadOnlyDictionary<string, obj>) =
        { Message = message
          Path = Skip
          Locations = Include locations
          Extensions = extensions |> Skippable.ofOption }

    static member Create (message, locations, extensions) =
        { Message = message
          Path = Skip
          Locations = Include locations
          Extensions = extensions }

    static member OfError (error : IGQLError) =
        let extensions =
            match error with
            | :? IGQLErrorExtensions as ext -> ext.Extensions |> Skippable.ofValueOption
            | _ -> Skip

        GQLProblemDetails.Create (error.Message, extensions)

    static member OfFieldError (path : FieldPath) (error : IGQLError) =
        let extensions =
            match error with
            | :? IGQLErrorExtensions as ext -> ext.Extensions |> Skippable.ofValueOption
            | _ -> Skip

        GQLProblemDetails.Create (error.Message, path, extensions)
