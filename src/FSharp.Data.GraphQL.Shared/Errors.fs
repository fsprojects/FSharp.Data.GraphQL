namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Linq
open System.Runtime
open System.Runtime.InteropServices
open System.Text.Json.Serialization

type internal FieldPath = obj list

type IGQLError =
    abstract member Message : string with get

type IGQLExceptionError =
    inherit IGQLError
    abstract member Exception : Exception with get

type internal ICoerceGQLError =
    inherit IGQLError
    abstract member VariableMessage : string with get

type IGQLErrorExtensions =
    abstract member Extensions : IReadOnlyDictionary<string, obj> voption with get

[<Struct>]
type GQLProblemLocation = { Line : int; Column : int }

[<RequireQualifiedAccess>]
module CustomErrorFields =

    [<Literal>]
    let Kind = "kind"
    [<Literal>]
    let Path = "path"
    [<Literal>]
    let VariableName = "variableName"
    [<Literal>]
    let VariableType = "variableType"
    [<Literal>]
    let ArgumentName = "argumentName"
    [<Literal>]
    let ArgumentType = "argumentType"
    [<Literal>]
    let ObjectType = "objectType"
    [<Literal>]
    let FieldType = "fieldType"

type ErrorKind =
    /// GraphQL request validation and planning
    | Validation
    /// Input variables or inline values coercion
    | InputCoercion
    /// Input variable object or inline object fields validation as a whole
    | InputObjectValidation
    /// GraphQL field execution
    | Execution

#nowarn "0386"
/// <summary>
/// A machine-readable format for specifying errors in GraphQL API responses based on <see href="http://spec.graphql.org/October2021/#sec-Errors"/>.
/// </summary>
//[<JsonConverter(typeof(ProblemDetailsJsonConverter))>]
[<CustomEquality; NoComparison>]
type GQLProblemDetails = {
    /// <summary>
    /// A short, human-readable summary of the problem type. It SHOULD NOT change from occurrence to occurrence
    /// of the problem, except for purposes of localization.
    /// </summary>
    [<JsonPropertyName("message")>]
    Message : string

    /// <summary>
    /// The possible exception associated with this error. It won't be serialized.
    /// </summary>
    [<JsonIgnore>]
    Exception : Exception voption

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
} with

    //member internal error.WithErrorKind (errorKind : ErrorKind) =
    //    let extensions =
    //        error.Extensions
    //        |> Skippable.map (fun extensions -> GQLProblemDetails.SetErrorKind errorKind extensions)
    //    if error.Extensions = extensions then error
    //    else { error with Extensions = extensions }

    //static member internal withErrorKind (errorKind : ErrorKind) (error: GQLProblemDetails) =
    //    let extensions =
    //        error.Extensions
    //        |> Skippable.map (fun extensions -> GQLProblemDetails.SetErrorKind errorKind extensions)
    //        |> Skippable.defaultWith (fun () -> Dictionary<string, obj> 1 |> GQLProblemDetails.SetErrorKind errorKind)
    //        |> Include
    //    if error.Extensions = extensions then error
    //    else { error with Extensions = extensions }

    static member SetErrorKind (errorKind : ErrorKind) (extensions : IReadOnlyDictionary<string, obj>) =
        let mutableExtensions =
            match extensions with
            | :? IDictionary<string, obj> as extensions when not extensions.IsReadOnly -> extensions
            | _ ->
#if NETSTANDARD2_0
                let dictionary = Dictionary<string, obj> (extensions.Count)
                for pair in extensions do
                    dictionary.Add (pair.Key, pair.Value)
                dictionary
#else
                Dictionary<string, obj> (collection = (extensions :> IEnumerable<KeyValuePair<string, obj>>))
#endif
        mutableExtensions[CustomErrorFields.Kind] <- errorKind
        match mutableExtensions with
        | :? IReadOnlyDictionary<string, obj> as extensions -> extensions
        | _ -> ReadOnlyDictionary<string, obj> mutableExtensions

    static member internal CreateWithKind (message, kind : ErrorKind, ?path) = {
        Message = message
        Exception = ValueNone
        Path = path |> Skippable.ofOption
        Locations = Skip
        Extensions = Dictionary<string, obj> 1 |> GQLProblemDetails.SetErrorKind kind |> Include
    }

    static member Create (message, extensions : IReadOnlyDictionary<string, obj>) = {
        Message = message
        Exception = ValueNone
        Path = Skip
        Locations = Skip
        Extensions = Include extensions
    }

    static member Create (message, [<Optional>] extensions) = {
        Message = message
        Exception = ValueNone
        Path = Skip
        Locations = Skip
        Extensions = extensions
    }

    static member CreateOfException (message : string, ex : Exception, [<Optional>] path : FieldPath Skippable, [<Optional>] extensions : IReadOnlyDictionary<string, obj> Skippable) = {
        Message = message
        Exception = ValueSome ex
        Path = path
        Locations = Skip
        Extensions = extensions
    }

    static member CreateOfException (message : string, ex : Exception, path : FieldPath, [<Optional>] extensions : IReadOnlyDictionary<string, obj> Skippable) = {
        Message = message
        Exception = ValueSome ex
        Path = Include path
        Locations = Skip
        Extensions = extensions
    }

    static member Create (message, path, extensions : IReadOnlyDictionary<string, obj>) = {
        Message = message
        Exception = ValueNone
        Path = Include path
        Locations = Skip
        Extensions = Include extensions
    }

    static member Create (message, path, [<Optional>] extensions) = {
        Message = message
        Exception = ValueNone
        Path = Include path
        Locations = Skip
        Extensions = extensions
    }

    static member Create (message, locations, extensions : IReadOnlyDictionary<string, obj>) = {
        Message = message
        Exception = ValueNone
        Path = Skip
        Locations = Include locations
        Extensions = Include extensions
    }

    static member Create (message, locations, extensions) = {
        Message = message
        Exception = ValueNone
        Path = Skip
        Locations = Include locations
        Extensions = extensions
    }

    static member OfError (error : IGQLError) =
        let extensions =
            match error with
            | :? IGQLErrorExtensions as ext -> ext.Extensions |> Skippable.ofValueOption
            | _ -> Skip

        let message =
            match error with
            | :? ICoerceGQLError as error -> error.VariableMessage + error.Message
            | _ -> error.Message

        match error with
        | :? IGQLExceptionError as exceptionError ->
            GQLProblemDetails.CreateOfException(exceptionError.Message, exceptionError.Exception, extensions = extensions)
        | _ ->
            GQLProblemDetails.Create (message, extensions)

    static member OfFieldError (path : FieldPath) (error : IGQLError) =
        let extensions =
            match error with
            | :? IGQLErrorExtensions as ext -> ext.Extensions |> Skippable.ofValueOption
            | _ -> Skip

        let message =
            match error with
            | :? ICoerceGQLError as error -> error.VariableMessage + error.Message
            | _ -> error.Message

        match error with
        | :? IGQLExceptionError as exceptionError ->
            GQLProblemDetails.CreateOfException(exceptionError.Message, exceptionError.Exception, path, extensions)
        | _ ->
            GQLProblemDetails.Create (message, path, extensions)

    static member internal OfFieldExecutionError (path : FieldPath) (error : IGQLError) =
        let extensions =
            match error with
            | :? IGQLErrorExtensions as ext -> ext.Extensions
            | _ -> ValueNone
            |> ValueOption.defaultWith (fun () -> Dictionary<string, obj> 1)
            |> GQLProblemDetails.SetErrorKind Execution
            |> Include

        let message =
            match error with
            | :? ICoerceGQLError as error -> error.VariableMessage + error.Message
            | _ -> error.Message

        match error with
        | :? IGQLExceptionError as exceptionError ->
            GQLProblemDetails.CreateOfException(exceptionError.Message, exceptionError.Exception, path, extensions)
        | _ ->
            GQLProblemDetails.Create (message, path, extensions)

    override this.GetHashCode () =
        let extensionsHashCode =
            match this.Extensions with
            | Skip -> 0
            | Include extensions ->
                extensions
                |> Seq.fold (fun hash kvp ->
                    hash ^^^ kvp.Key.GetHashCode () ^^^ kvp.Value.GetHashCode ()
                ) 0
#if NETSTANDARD2_0
        this.Message.GetHashCode () ^^^ this.Path.GetHashCode () ^^^ this.Locations.GetHashCode () ^^^ extensionsHashCode
#else
        HashCode.Combine (this.Message, this.Path, this.Locations, extensionsHashCode)
#endif

    override this.Equals (obj : obj) =
        match obj with
        | :? GQLProblemDetails as other -> (this :> IEquatable<GQLProblemDetails>).Equals other
        | _ -> false

    interface IEquatable<GQLProblemDetails> with

        member this.Equals (other : GQLProblemDetails) =

            let extensionsEqual =
                match this.Extensions, other.Extensions with
                | Skip, Skip -> true
                | Include thisExtensions, Include otherExtensions ->
                    Enumerable.SequenceEqual(thisExtensions, otherExtensions)
                | _ -> false

            this.Message = other.Message
            && this.Path = other.Path
            && this.Locations = other.Locations
            && extensionsEqual


