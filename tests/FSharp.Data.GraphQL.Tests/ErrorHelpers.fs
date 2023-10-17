// The MIT License (MIT)

[<AutoOpen>]
module internal ErrorHelpers

open System
open System.Text.Json.Serialization
open FSharp.Data.GraphQL

type ErrorSource =
    | Variable of Name : string
    | Argument of Name : string

let ensureDeferred (result : GQLExecutionResult) (onDeferred : Output -> GQLProblemDetails list -> IObservable<GQLDeferredResponseContent> -> unit) : unit =
    match result.Content with
    | Deferred(data, errors, deferred) -> onDeferred data errors deferred
    | response -> fail $"Expected a Deferred GQLResponse but got {Environment.NewLine}{response}"

let ensureDirect (result : GQLExecutionResult) (onDirect : Output -> GQLProblemDetails list -> unit) : unit =
    match result.Content with
    | Direct(data, errors) -> onDirect data errors
    | response -> fail $"Expected a Direct GQLResponse but got {Environment.NewLine}{response}"

let ensureRequestError (result : GQLExecutionResult) (onRequestError : GQLProblemDetails list -> unit) : unit =
    match result.Content with
    | RequestError errors -> onRequestError errors
    | response -> fail $"Expected RequestError GQLResponse but got {Environment.NewLine}{response}"

let ensureValidationError (message : string) (path : FieldPath) (error : GQLProblemDetails) =
    equals message error.Message
    equals (Include path) error.Path
    match error.Extensions with
    | Skip -> fail "Expected extensions to be present"
    | Include extensions ->
        equals Validation (unbox extensions[CustomErrorFields.Kind])

let ensureExecutionError (message : string) (path : FieldPath) (error : GQLProblemDetails) =
    equals message error.Message
    equals (Include path) error.Path
    match error.Extensions with
    | Skip -> fail "Expected extensions to be present"
    | Include extensions ->
        equals Execution (unbox extensions[CustomErrorFields.Kind])

let ensureInputCoercionError (errorSource : ErrorSource) (message : string) (``type`` : string) (error : GQLProblemDetails) =
    equals message error.Message
    match error.Extensions with
    | Skip -> fail "Expected extensions to be present"
    | Include extensions ->
        equals InputCoercion (unbox extensions[CustomErrorFields.Kind])
        match errorSource with
        | Variable name ->
            equals name (unbox extensions[CustomErrorFields.VariableName])
            equals ``type`` (unbox extensions[CustomErrorFields.VariableType])
        | Argument name  ->
            equals name (unbox extensions[CustomErrorFields.ArgumentName])
            equals ``type`` (unbox extensions[CustomErrorFields.ArgumentType])

let ensureInputObjectFieldCoercionError (errorSource : ErrorSource) (message : string) (inputObjectPath : FieldPath) (objectType : string) (fieldType : string) (error : GQLProblemDetails) =
    equals message error.Message
    match error.Extensions with
    | Skip -> fail "Expected extensions to be present"
    | Include extensions ->
        equals InputCoercion (unbox extensions[CustomErrorFields.Kind])
        match errorSource with
        | Variable name -> equals name (unbox extensions[CustomErrorFields.VariableName])
        | Argument name  -> equals name (unbox extensions[CustomErrorFields.ArgumentName])
        if not inputObjectPath.IsEmpty then
            equals inputObjectPath (unbox extensions[CustomErrorFields.Path])
        equals objectType (unbox extensions[CustomErrorFields.ObjectType])
        equals fieldType (unbox extensions[CustomErrorFields.FieldType])

let ensureInputObjectValidationError (errorSource : ErrorSource) (message : string) (inputObjectPath : FieldPath) (objectType : string) (error : GQLProblemDetails) =
    equals message error.Message
    match error.Extensions with
    | Skip -> fail "Expected extensions to be present"
    | Include extensions ->
        equals InputObjectValidation (unbox extensions[CustomErrorFields.Kind])
        match errorSource with
        | Variable name -> equals name (unbox extensions[CustomErrorFields.VariableName])
        | Argument name  -> equals name (unbox extensions[CustomErrorFields.ArgumentName])
        if not inputObjectPath.IsEmpty then
            equals inputObjectPath (unbox extensions[CustomErrorFields.Path])
        equals objectType (unbox extensions[CustomErrorFields.ObjectType])
