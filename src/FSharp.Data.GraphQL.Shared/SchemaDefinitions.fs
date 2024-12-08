// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc
namespace FSharp.Data.GraphQL.Types

open System
open System.Reflection
open System.Collections.Generic
open System.Text.Json
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Extensions
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Validation
open FSharp.Quotations

[<AutoOpen>]
module SchemaDefinitions =

    module Errors =

        type InputValue with

            member inputValue.GetCoerceError(destinationType) =
                let getMessage inputType value = $"Inline value '{value}' of type %s{inputType} cannot be converted into %s{destinationType}"
                let message =
                    match inputValue with
                    | IntValue value -> getMessage "integer" value
                    | FloatValue value -> getMessage "float" value
                    | BooleanValue value -> getMessage "boolean" value
                    | StringValue value -> getMessage "string" value
                    | NullValue ->  $"Inline value 'null' cannot be converted into {destinationType}"
                    | EnumValue value ->  getMessage "enum" value
                    | value -> raise <| NotSupportedException $"{value} cannot be passed as scalar input"
                Error [{ new IGQLError with member _.Message = message }]

            member inputValue.GetCoerceRangeError(destinationType, minValue, maxValue) =
                let getMessage inputType value = $"Inline value '{value}' of type %s{inputType} cannot be converted into %s{destinationType} of range from {minValue} to {maxValue}"
                let message =
                    match inputValue with
                    | IntValue value -> getMessage "integer" value
                    | FloatValue value -> getMessage "float" value
                    | BooleanValue value -> getMessage "boolean" value
                    | StringValue value -> getMessage "string" value
                    | NullValue ->  $"Inline value 'null' cannot be converted into {destinationType}"
                    | EnumValue value ->  getMessage "enum" value
                    | value -> raise <| NotSupportedException $"{value} cannot be passed as scalar input"
                Error [{ new IGQLError with member _.Message = message }]

        type JsonElement with

            member e.GetDeserializeError(destinationType, minValue, maxValue ) =
                let jsonValue = match e.ValueKind with JsonValueKind.String -> e.GetString() | _ -> e.GetRawText()
                Error [{ new IGQLError with member _.Message = $"JSON value '{jsonValue}' of kind '{e.ValueKind}' cannot be deserialized into %s{destinationType} of range from {minValue} to {maxValue}" }]

            member e.GetDeserializeError(destinationType) =
                let jsonValue = match e.ValueKind with JsonValueKind.String -> e.GetString() | _ -> e.GetRawText()
                Error [{ new IGQLError with member _.Message = $"JSON value '{jsonValue}' of kind '{e.ValueKind}' cannot be deserialized into %s{destinationType}" }]

        let getParseRangeError (destinationType, minValue, maxValue) value =
            Error [{ new IGQLError with member _.Message = $"Inline value '%s{value}' cannot be parsed into %s{destinationType} of range from {minValue} to {maxValue}" }]

        let getParseError destinationType value =
            Error [{ new IGQLError with member _.Message = $"Inline value '%s{value}' cannot be parsed into %s{destinationType}" }]


    open System.Globalization
    open Errors

    /// Tries to convert any value to int.
    let coerceIntValue (x : obj) : int option =
        match x with
        | null -> None
        | other ->
            try
                Some(System.Convert.ToInt32 other)
            with _ -> None

    /// Tries to convert any value to int64.
    let coerceLongValue (x : obj) : int64 option =
        match x with
        | null -> None
        | :? int as i -> Some (int64 i)
        | :? int64 as l -> Some(l)
        | :? double as d -> Some(int64 d)
        | :? string as s ->
            match Int64.TryParse(s) with
            | true, i -> Some i
            | false, _ -> None
        | :? bool as b ->
            Some(if b then 1L else 0L)
        | other ->
            try
                Some(System.Convert.ToInt64 other)
            with _ -> None

    /// Tries to convert any value to double.
    let coerceFloatValue (x : obj) : double option =
        match x with
        | null -> None
        | :? int as i -> Some(double i)
        | :? int64 as l -> Some(double l)
        | :? double as d -> Some d
        | :? string as s ->
            match Double.TryParse(s) with
            | true, i -> Some i
            | false, _ -> None
        | :? bool as b ->
            Some(if b then 1.
                 else 0.)
        | other ->
            try
                Some(System.Convert.ToDouble other)
            with _ -> None

    /// Tries to convert any value to bool.
    let coerceBoolValue (x : obj) : bool option =
        match x with
        | null -> None
        | other ->
            try
                Some(System.Convert.ToBoolean other)
            with _ -> None

    /// Tries to convert any value to URI.
    let coerceUriValue (x : obj) : Uri option =
        match x with
        | null -> None
        | :? Uri as u -> Some u
        | :? string as s ->
            match Uri.TryCreate(s, UriKind.RelativeOrAbsolute) with
            | true, uri -> Some uri
            | false, _ -> None
        | other -> None

    /// Tries to convert any value to DateTimeOffset.
    let coerceDateTimeOffsetValue (x : obj) : DateTimeOffset option =
        match x with
        | null -> None
        | :? DateTimeOffset as d -> Some d
        | :? DateTime as d -> Some (DateTimeOffset d)
        | :? string as s ->
            match DateTimeOffset.TryParse(s) with
            | true, date -> Some date
            | false, _ -> None
        | other -> None

    /// Tries to convert any value to DateOnly.
    let coerceDateOnlyValue (x : obj) : DateOnly option =
        match x with
        | null -> None
        | :? DateOnly as d -> Some d
        | :? DateTime as d -> Some (DateOnly.FromDateTime d)
        | :? string as s ->
            match DateOnly.TryParse(s) with
            | true, date -> Some date
            | false, _ -> None
        | other -> None

    /// Tries to convert any value to Guid.
    let coerceGuidValue (x : obj) : Guid option =
        match x with
        | null -> None
        | :? Guid as g -> Some g
        | :? string as s ->
            match Guid.TryParse(s) with
            | true, guid -> Some guid
            | false, _ -> None
        | other -> None

    /// Check if provided obj value is an Option and extract its wrapped value as object if possible
    let private (|Option|_|) (x : obj) =
        if isNull x then None
        else
            let t = x.GetType().GetTypeInfo()
            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
                t.GetDeclaredProperty("Value").GetValue(x) |> Some
            else None

    /// Tries to convert any value to string.
    let coerceStringValue (x : obj) : string option =
        match x with
        | null -> None
        | :? string as s -> Some s
        | :? bool as b ->
            Some(if b then "true"
                 else "false")
        | Option o -> Some(o.ToString())
        | _ -> Some(x.ToString())

    /// Tries to convert any value to generic type parameter.
    let coerceIdValue (x : obj) : string option =
        match x with
        | null -> None
        | :? string as s -> Some s
        | Option o -> Some(string o)
        | _ -> Some(string x)


    /// Tries to resolve AST query input to int.
    let coerceIntInput =
        let destinationType = "integer"
        function
        | Variable e when e.ValueKind = JsonValueKind.Number ->
            match e.TryGetInt32() with
            | true, value -> Ok value
            | false, _ -> e.GetDeserializeError(destinationType, Int32.MinValue, Int32.MaxValue)
        | Variable e when e.ValueKind = JsonValueKind.True -> Ok 1
        | Variable e when e.ValueKind = JsonValueKind.False -> Ok 0
        | Variable e -> e.GetDeserializeError (destinationType, Int32.MinValue, Int32.MaxValue)
        | InlineConstant (IntValue i) -> Ok (int i)
        | InlineConstant (BooleanValue b) -> Ok (if b then 1 else 0)
        | InlineConstant value -> value.GetCoerceRangeError(destinationType, Int32.MinValue, Int32.MaxValue)

    /// Tries to resolve AST query input to int64.
    let coerceLongInput =
        let destinationType = "integer"
        function
        | Variable e when e.ValueKind = JsonValueKind.Number ->
            match e.TryGetInt64() with
            | true, value -> Ok value
            | false, _ -> e.GetDeserializeError(destinationType, Int64.MinValue, Int64.MaxValue)
        | Variable e when e.ValueKind = JsonValueKind.True -> Ok 1L
        | Variable e when e.ValueKind = JsonValueKind.False -> Ok 0L
        | Variable e -> e.GetDeserializeError (destinationType, Int64.MinValue, Int64.MaxValue)
        | InlineConstant (IntValue i) -> Ok (int64 i)
        | InlineConstant (BooleanValue b) -> Ok(if b then 1L else 0L)
        | InlineConstant value -> value.GetCoerceRangeError(destinationType, Int64.MinValue, Int64.MaxValue)

    /// Tries to resolve AST query input to double.
    let coerceFloatInput =
        let destinationType = "float"
        function
        | Variable e when e.ValueKind = JsonValueKind.Number -> Ok (e.GetDouble())
        | Variable e when e.ValueKind = JsonValueKind.True -> Ok 1.
        | Variable e when e.ValueKind = JsonValueKind.False -> Ok 0.
        | Variable e -> e.GetDeserializeError (destinationType, Double.MinValue, Double.MaxValue)
        | InlineConstant (IntValue i) -> Ok(double i)
        | InlineConstant (FloatValue f) -> Ok f
        | InlineConstant (BooleanValue b) -> Ok(if b then 1. else 0.)
        | InlineConstant value -> value.GetCoerceRangeError(destinationType, Double.MinValue, Double.MaxValue)

    /// Tries to resolve AST query input to string.
    let coerceStringInput =
        let destinationType = "string"
        function
        | Variable e ->
            match e.ValueKind with
            | JsonValueKind.String -> Ok (e.GetString())
            | JsonValueKind.True
            | JsonValueKind.False
            | JsonValueKind.Number -> Ok (e.GetRawText())
            | _ -> e.GetDeserializeError destinationType
        | InlineConstant (IntValue i) -> Ok(i.ToString(CultureInfo.InvariantCulture))
        | InlineConstant (FloatValue f) -> Ok(f.ToString(CultureInfo.InvariantCulture))
        | InlineConstant (StringValue s) -> Ok s
        | InlineConstant (BooleanValue b) -> Ok (if b then "true" else "false")
        | InlineConstant (EnumValue e) -> Ok e
        | InlineConstant value -> value.GetCoerceError destinationType

    let coerceEnumInput =
        function
        | EnumValue e -> Ok e
        | value -> value.GetCoerceError "enum"

    /// Tries to resolve AST query input to bool.
    let coerceBoolInput =
        let destinationType = "boolean"
        function
        | Variable e when e.ValueKind = JsonValueKind.True -> Ok true
        | Variable e when e.ValueKind = JsonValueKind.False -> Ok false
        | Variable e when e.ValueKind = JsonValueKind.Number -> Ok (if e.GetDouble() = 0. then false else true)
        | Variable e -> e.GetDeserializeError destinationType
        | InlineConstant (IntValue i) -> Ok(if i = 0L then false else true)
        | InlineConstant (FloatValue f) -> Ok(if f = 0. then false else true)
        | InlineConstant (BooleanValue b) -> Ok b
        | InlineConstant value -> value.GetCoerceError destinationType

    /// Tries to resolve AST query input to provided generic type.
    let coerceIdInput input : Result<string, IGQLError list> =
        let destinationType = "identifier"
        match input with
        | Variable e when e.ValueKind = JsonValueKind.String -> Ok (e.GetString())
        | Variable e when e.ValueKind = JsonValueKind.Number ->
            try
                e.GetInt64() |> ignore
                Ok (e.GetRawText())
            with :? FormatException ->
                e.GetDeserializeError(destinationType, Int64.MinValue, Int64.MaxValue)
        | Variable e -> e.GetDeserializeError destinationType
        | InlineConstant (IntValue i) -> Ok(string i)
        | InlineConstant (FloatValue i) -> (FloatValue i).GetCoerceRangeError(destinationType, Int64.MinValue, Int64.MaxValue)
        | InlineConstant (StringValue s) -> Ok s
        | InlineConstant value -> value.GetCoerceError destinationType

    /// Tries to resolve AST query input to URI.
    let coerceUriInput =
        let destinationType = "URI"
        function
        | Variable e when e.ValueKind = JsonValueKind.String ->
            match Uri.TryCreate(e.GetString(), UriKind.RelativeOrAbsolute) with
            | true, uri -> Ok uri
            | false, _ -> e.GetDeserializeError destinationType
        | Variable e -> e.GetDeserializeError destinationType
        | InlineConstant (StringValue s) ->
            match Uri.TryCreate(s, UriKind.RelativeOrAbsolute) with
            | true, uri -> Ok uri
            | false, _ -> getParseError destinationType s
        | InlineConstant value -> value.GetCoerceError destinationType

    /// Tries to resolve AST query input to DateTimeOffset.
    let coerceDateTimeOffsetInput =
        let destinationType = "date and time with offset"
        function
        | Variable e when e.ValueKind = JsonValueKind.String ->
            let s = e.GetString()
            match DateTimeOffset.TryParse(s) with
            | true, date -> Ok date
            | false, _ -> e.GetDeserializeError destinationType
        | Variable e -> e.GetDeserializeError destinationType
        | InlineConstant (StringValue s) ->
            match DateTimeOffset.TryParse(s) with
            | true, date -> Ok date
            | false, _ -> getParseRangeError(destinationType, DateTimeOffset.MinValue, DateTimeOffset.MaxValue) s
        | InlineConstant value -> value.GetCoerceRangeError(destinationType, DateTimeOffset.MinValue, DateTimeOffset.MaxValue)

    /// Tries to resolve AST query input to DateOnly.
    let coerceDateOnlyInput =
        let destinationType = "date"
        function
        | Variable e when e.ValueKind = JsonValueKind.String ->
            let s = e.GetString()
            match DateOnly.TryParse(s) with
            | true, date -> Ok date
            | false, _ -> e.GetDeserializeError destinationType
        | Variable e -> e.GetDeserializeError destinationType
        | InlineConstant (StringValue s) ->
            match DateOnly.TryParse(s) with
            | true, date -> Ok date
            | false, _ -> getParseRangeError(destinationType, DateOnly.MinValue, DateOnly.MaxValue) s
        | InlineConstant value -> value.GetCoerceRangeError(destinationType, DateOnly.MinValue, DateOnly.MaxValue)

    /// Tries to resolve AST query input to Guid.
    let coerceGuidInput =
        let destinationType = "GUID"
        function
        | Variable e when e.ValueKind = JsonValueKind.String ->
            let s = e.GetString()
            match Guid.TryParse(s) with
            | true, guid -> Ok guid
            | false, _ -> e.GetDeserializeError destinationType
        | Variable e -> e.GetDeserializeError destinationType
        | InlineConstant (StringValue s) ->
            match Guid.TryParse(s) with
            | true, guid -> Ok guid
            | false, _ -> getParseError destinationType s
        | InlineConstant value -> value.GetCoerceError destinationType

    /// Wraps a GraphQL type definition, allowing defining field/argument
    /// to take option of provided value.
    let Nullable(innerDef : #TypeDef<'Val>) : NullableDef<'Val> = upcast { NullableDefinition.OfType = innerDef }

    /// Wraps a GraphQL type definition, allowing defining field/argument
    /// to take collection of provided value.
    let ListOf(innerDef : #TypeDef<'Val>) : ListOfDef<'Val, 'Seq> = upcast { ListOfDefinition.OfType = innerDef }

    let internal variableOrElse other value (variables : IReadOnlyDictionary<string, obj>) =
        match value with
        // TODO: Use FSharp.Collection.Immutable
        | VariableName variableName ->
            match variables.TryGetValue variableName with
            | true, value -> Ok value
            | false, _ -> Error [{ new IGQLError with member _.Message = $"A variable '$%s{variableName}' not found" }]
        | v -> other v

    /// GraphQL type of int
    let IntType : ScalarDefinition<int> =
        { Name = "Int"
          Description =
              Some
                  "The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1."
          CoerceInput = coerceIntInput
          CoerceOutput = coerceIntValue }

    /// GraphQL type of long
    let LongType : ScalarDefinition<int64> =
        { Name = "Long"
          Description =
              Some
                  "The `Long` scalar type represents non-fractional signed whole numeric values. Long can represent values between -(2^63) and 2^63 - 1."
          CoerceInput = coerceLongInput
          CoerceOutput = coerceLongValue }

    /// GraphQL type of boolean
    let BooleanType : ScalarDefinition<bool> =
        { Name = "Boolean"
          Description = Some "The `Boolean` scalar type represents `true` or `false`."
          CoerceInput = coerceBoolInput
          CoerceOutput = coerceBoolValue }

    /// GraphQL type of float
    let FloatType : ScalarDefinition<double> =
        { Name = "Float"
          Description =
              Some
                  "The `Float` scalar type represents signed double-precision fractional values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
          CoerceInput = coerceFloatInput
          CoerceOutput = coerceFloatValue }

    /// GraphQL type of string
    let StringType : ScalarDefinition<string> =
        { Name = "String"
          Description =
              Some
                  "The `String` scalar type represents textual data, represented as UTF-8 character sequences. The `String` type is most often used by GraphQL to represent free-form human-readable text."
          CoerceInput = coerceStringInput
          CoerceOutput = coerceStringValue }

    /// GraphQL type for custom identifier
    let IDType : ScalarDefinition<string> =
        { Name = "ID"
          Description =
              Some
                  "The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The `ID` type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID."
          CoerceInput = coerceIdInput
          CoerceOutput = coerceIdValue }

    let ObjType : ScalarDefinition<obj> = {
            Name = "Object"
            Description =
               Some
                  "The `Object` scalar type represents textual data, represented as UTF-8 character sequences. The `String` type is most often used by GraphQL to represent free-form human-readable text."
            CoerceInput = (fun o -> Ok (o))
            CoerceOutput = (fun o -> Some (o))
        }

    /// GraphQL type for System.Uri
    let UriType : ScalarDefinition<Uri> =
        { Name = "URI"
          Description =
              Some
                  "The `URI` scalar type represents a string resource identifier compatible with URI standard. The `URI` type appears in a JSON response as a String."
          CoerceInput = coerceUriInput
          CoerceOutput = coerceUriValue }

    /// GraphQL type for System.DateTimeOffset
    let DateTimeOffsetType : ScalarDefinition<DateTimeOffset> =
        { Name = "DateTimeOffset"
          Description =
              Some
                  "The `DateTimeOffset` scalar type represents a Date value with Time component. The `DateTimeOffset` type appears in a JSON response as a String representation compatible with ISO-8601 format."
          CoerceInput = coerceDateTimeOffsetInput
          CoerceOutput = coerceDateTimeOffsetValue }

    /// GraphQL type for System.DateOnly
    let DateOnlyType : ScalarDefinition<DateOnly> =
        { Name = "DateOnly"
          Description =
              Some
                  "The `DateOnly` scalar type represents a Date value without Time component. The `DateOnly` type appears in a JSON response as a `String` representation of full-date value as specified by [IETF 3339](https://www.ietf.org/rfc/rfc3339.txt)."
          CoerceInput = coerceDateOnlyInput
          CoerceOutput = coerceDateOnlyValue }

    /// GraphQL type for System.Guid
    let GuidType : ScalarDefinition<Guid> =
        { Name = "Guid"
          Description =
              Some
                  "The `Guid` scalar type represents a Globaly Unique Identifier value. It's a 128-bit long byte key, that can be serialized to string."
          CoerceInput = coerceGuidInput
          CoerceOutput = coerceGuidValue }

    /// GraphQL @include directive.
    let IncludeDirective : DirectiveDef =
        { Name = "include"
          Description =
              Some "Directs the executor to include this field or fragment only when the `if` argument is true."
          Locations =
              DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT
          Args =
              [| { InputFieldDefinition.Name = "if"
                   Description = Some "Included when true."
                   IsSkippable = false
                   TypeDef = BooleanType
                   DefaultValue = None
                   ExecuteInput = variableOrElse (InlineConstant >> coerceBoolInput >> Result.map box) } |] }

    /// GraphQL @skip directive.
    let SkipDirective : DirectiveDef =
        { Name = "skip"
          Description = Some "Directs the executor to skip this field or fragment when the `if` argument is true."
          Locations =
              DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT
          Args =
              [| { InputFieldDefinition.Name = "if"
                   Description = Some "Skipped when true."
                   IsSkippable = false
                   TypeDef = BooleanType
                   DefaultValue = None
                   ExecuteInput = variableOrElse (InlineConstant >> coerceBoolInput >> Result.map box) } |] }

    /// GraphQL @defer directive.
    let DeferDirective : DirectiveDef =
        { Name = "defer"
          Description = Some "Defers the resolution of this field or fragment"
          Locations =
            DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT ||| DirectiveLocation.FRAGMENT_DEFINITION
          Args = [||] }

    /// GraphQL @stream directive.
    let StreamDirective : DirectiveDef =
        { Name = "stream"
          Description = Some "Streams the resolution of this field or fragment"
          Locations =
            DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT ||| DirectiveLocation.FRAGMENT_DEFINITION
          Args = [||] }

    /// GraphQL @live directive.
    let LiveDirective : DirectiveDef =
        { Name = "live"
          Description = Some "Subscribes for live updates of this field or fragment"
          Locations =
            DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT ||| DirectiveLocation.FRAGMENT_DEFINITION
          Args = [||] }

    let inline internal strip (fn : 'In -> 'Out) : obj -> obj = fun i -> upcast fn (i :?> 'In)

    /// Common space for all definition helper methods.
    [<AbstractClass; Sealed>]
    type Define =

        /// <summary>
        /// Creates GraphQL type definition for user defined scalar.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="coerceInput">Function used to resolve .NET object from GraphQL query AST or variable.</param>
        /// <param name="coerceOutput">Function used to cross cast to .NET types.</param>
        /// <param name="description">Optional scalar description. Usefull for generating documentation.</param>
        static member Scalar(name : string, coerceInput : InputParameterValue -> Result<'T, string>,
                             coerceOutput : obj -> 'T option, ?description : string) : ScalarDefinition<'T> =
            { Name = name
              Description = description
              CoerceInput = coerceInput >> Result.mapError (fun msg -> { new IGQLError with member _.Message = msg } |> List.singleton)
              CoerceOutput = coerceOutput }

        /// <summary>
        /// Creates GraphQL type definition for user defined scalar.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="coerceInput">Function used to resolve .NET object from GraphQL query AST or variable.</param>
        /// <param name="coerceOutput">Function used to cross cast to .NET types.</param>
        /// <param name="description">Optional scalar description. Usefull for generating documentation.</param>
        static member Scalar(name : string, coerceInput : InputParameterValue -> Result<'T, string list>,
                             coerceOutput : obj -> 'T option, ?description : string) : ScalarDefinition<'T> =
            { Name = name
              Description = description
              CoerceInput = coerceInput >> Result.mapError (List.map (fun msg -> { new IGQLError with member _.Message = msg }))
              CoerceOutput = coerceOutput }

        /// <summary>
        /// Creates GraphQL type definition for user defined scalar.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="coerceInput">Function used to resolve .NET object from GraphQL query AST or variable.</param>
        /// <param name="coerceOutput">Function used to cross cast to .NET types.</param>
        /// <param name="description">Optional scalar description. Usefull for generating documentation.</param>
        static member Scalar(name : string, coerceInput : InputParameterValue -> Result<'T, IGQLError>,
                             coerceOutput : obj -> 'T option, ?description : string) : ScalarDefinition<'T> =
            { Name = name
              Description = description
              CoerceInput = coerceInput >> Result.mapError List.singleton
              CoerceOutput = coerceOutput }

        /// <summary>
        /// Creates GraphQL type definition for user defined scalar.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="coerceInput">Function used to resolve .NET object from GraphQL query AST or variable.</param>
        /// <param name="coerceOutput">Function used to cross cast to .NET types.</param>
        /// <param name="description">Optional scalar description. Usefull for generating documentation.</param>
        static member Scalar(name : string, coerceInput : InputParameterValue -> Result<'T, IGQLError list>,
                             coerceOutput : obj -> 'T option, ?description : string) : ScalarDefinition<'T> =
            { Name = name
              Description = description
              CoerceInput = coerceInput
              CoerceOutput = coerceOutput }

        /// <summary>
        /// Creates GraphQL type definition for user defined wrapped scalar.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="coerceInput">Function used to resolve .NET object from GraphQL query AST or variable.</param>
        /// <param name="coerceOutput">Function used to cross cast to .NET types.</param>
        /// <param name="description">Optional scalar description. Usefull for generating documentation.</param>
        static member WrappedScalar(name : string, coerceInput : InputParameterValue -> Result<'Wrapper, string>,
                                    coerceOutput : obj -> 'Primitive option, ?description : string) : ScalarDefinition<'Primitive, 'Wrapper> =
            { Name = name
              Description = description
              CoerceInput = coerceInput >> Result.mapError (fun msg -> { new IGQLError with member _.Message = msg } |> List.singleton)
              CoerceOutput = coerceOutput }

        /// <summary>
        /// Creates GraphQL type definition for user defined wrapped scalar.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="coerceInput">Function used to resolve .NET object from GraphQL query AST or variable.</param>
        /// <param name="coerceOutput">Function used to cross cast to .NET types.</param>
        /// <param name="description">Optional scalar description. Usefull for generating documentation.</param>
        static member WrappedScalar(name : string, coerceInput : InputParameterValue -> Result<'Wrapper, string list>,
                                    coerceOutput : obj -> 'Primitive option, ?description : string) : ScalarDefinition<'Primitive, 'Wrapper> =
            { Name = name
              Description = description
              CoerceInput = coerceInput >> Result.mapError (List.map (fun msg -> { new IGQLError with member _.Message = msg }))
              CoerceOutput = coerceOutput }

        /// <summary>
        /// Creates GraphQL type definition for user defined wrapped scalar.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="coerceInput">Function used to resolve .NET object from GraphQL query AST or variable.</param>
        /// <param name="coerceOutput">Function used to cross cast to .NET types.</param>
        /// <param name="description">Optional scalar description. Usefull for generating documentation.</param>
        static member WrappedScalar(name : string, coerceInput : InputParameterValue -> Result<'Wrapper, IGQLError>,
                                    coerceOutput : obj -> 'Primitive option, ?description : string) : ScalarDefinition<'Primitive, 'Wrapper> =
            { Name = name
              Description = description
              CoerceInput = coerceInput >> Result.mapError List.singleton
              CoerceOutput = coerceOutput }

        /// <summary>
        /// Creates GraphQL type definition for user defined wrapped scalar.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="coerceInput">Function used to resolve .NET object from GraphQL query AST or variable.</param>
        /// <param name="coerceOutput">Function used to cross cast to .NET types.</param>
        /// <param name="description">Optional scalar description. Usefull for generating documentation.</param>
        static member WrappedScalar(name : string, coerceInput : InputParameterValue -> Result<'Wrapper, IGQLError list>,
                                    coerceOutput : obj -> 'Primitive option, ?description : string) : ScalarDefinition<'Primitive, 'Wrapper> =
            { Name = name
              Description = description
              CoerceInput = coerceInput
              CoerceOutput = coerceOutput }

        /// <summary>
        /// Creates GraphQL type definition for user defined enums.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="options">List of enum value cases.</param>
        /// <param name="description">Optional enum description. Usefull for generating documentation.</param>
        static member Enum(name : string, options : EnumValue<'Val> list, ?description : string) : EnumDef<'Val> =
            upcast { EnumDefinition.Name = name
                     Description = description
                     Options = options |> List.toArray }

        /// <summary>
        /// Creates a single enum option to be used as argument in <see cref="Schema.Enum"/>.
        /// </summary>
        /// <param name="name">Enum value name. Must be unique in scope of the defining enum.</param>
        /// <param name="value">
        /// .NET value associated with target enum value. All enum values are represented as strings in GraphQL type system.
        /// Enum value will be stringified using 'ToString()' method when passing to a client.
        /// </param>
        /// <param name="description">Optional enum value description. Usefull for generating documentation.</param>
        /// <param name="deprecationReason">If set, marks an enum value as deprecated.</param>
        static member EnumValue(name : string, value : 'Val, ?description : string, ?deprecationReason : string) : EnumValue<'Val> =
            { Name = name
              Description = description
              Value = value
              DeprecationReason = deprecationReason }

        /// <summary>
        /// Creates GraphQL custom output object type. It can be used as a valid output but not an input object
        /// (see <see cref="InputObject"/> for more details).
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="fields">List of fields defined by the current object. </param>
        /// <param name="description">Optional object description. Usefull for generating documentation.</param>
        /// <param name="interfaces">
        /// List of implemented interfaces. Object must explicitly define all fields from all interfaces it implements.
        /// </param>
        /// <param name="isTypeOf">
        /// Optional function used to determine if provided .NET object instance matches current object definition.
        /// </param>
        static member Object(name : string, fields : FieldDef<'Val> list, ?description : string,
                             ?interfaces : InterfaceDef list, ?isTypeOf : obj -> bool) : ObjectDef<'Val> =
            upcast { ObjectDefinition.Name = name
                     Description = description
                     FieldsFn =
                         lazy (fields
                               |> List.map (fun f -> f.Name, f)
                               |> Map.ofList)
                     Implements = defaultArg (Option.map List.toArray interfaces) [||]
                     IsTypeOf = isTypeOf }

        /// <summary>
        /// Creates a custom GraphQL input object type. Unlike GraphQL objects, input objects are valid input types,
        /// that can be included in GraphQL query strings. Input object maps to a .NET type, which can be standard
        /// .NET class or struct, or a F# record.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="fields">List of input fields defined by the current input object. </param>
        /// <param name="description">Optional input object description. Useful for generating documentation.</param>
        static member InputObject(name : string, fields : InputFieldDef list, ?description : string) : InputObjectDefinition<'Out> =
            { Name = name
              Description = description
              Fields = lazy (fields |> List.toArray)
              Validator = GQLValidator.empty
              ExecuteInput = Unchecked.defaultof<_> }

        /// <summary>
        /// Creates a custom GraphQL input object type. Unlike GraphQL objects, input objects are valid input types,
        /// that can be included in GraphQL query strings. Input object maps to a .NET type, which can be standard
        /// .NET class or struct, or a F# record.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="fields">List of input fields defined by the current input object. </param>
        /// <param name="description">Optional input object description. Useful for generating documentation.</param>
        static member InputObject(name : string, fields : InputFieldDef list, validator: GQLValidator<'Out>, ?description : string) : InputObjectDefinition<'Out> =
            { Name = name
              Description = description
              Fields = lazy (fields |> List.toArray)
              Validator = validator
              ExecuteInput = Unchecked.defaultof<_> }

        /// <summary>
        /// Creates the top level subscription object that holds all of the possible subscriptions as fields.
        /// </summary>
        /// <param name="name">Top level name. Must be unique in scope of the current schema.</param>
        /// <param name="fields">List of subscription fields to be defined for the schema. </param>
        /// <param name="description">Optional description. Usefull for generating documentation.</param>
        static member SubscriptionObject<'Val>(name: string, fields: SubscriptionFieldDef<'Val> list, ?description: string):SubscriptionObjectDefinition<'Val> =
            { Name = name
              Fields = (fields |> List.map (fun f -> f.Name, f) |> Map.ofList)
              Description = description }

        /// <summary>
        /// Creates field defined inside object types with automatically generated field resolve function.
        /// Field name must match object's property or field.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">Optional list of arguments used to parametrize field resolution.</param>
        /// <param name="deprecationReason">If set, marks current field as deprecated.</param>
        static member AutoField(name : string, typedef : #OutputDef<'Res>, ?description: string, ?args: InputFieldDef list, ?deprecationReason: string) : FieldDef<'Val, 'Res> =
            upcast { FieldDefinition.Name = name
                     Description = description
                     TypeDef = typedef
                     Resolve = Resolve.defaultResolve<'Val, 'Res> name
                     Args = defaultArg args [] |> Array.ofList
                     DeprecationReason = deprecationReason
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside interfaces. When used for objects may cause runtime exceptions due to
        /// lack of resolve function supplied. To use auto generated resolvers use <see cref="AutoField"/>.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member Field(name : string, typedef : #OutputDef<'Res>, ?deprecationReason : string) : FieldDef<'Val, 'Res> =
            upcast { FieldDefinition.Name = name
                     Description = None
                     TypeDef = typedef
                     Resolve = Undefined
                     Args = [||]
                     DeprecationReason = deprecationReason
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member Field(name : string, typedef : #OutputDef<'Res>,
                            [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res>,
                            ?deprecationReason : string) : FieldDef<'Val, 'Res> =
            upcast { FieldDefinition.Name = name
                     Description = None
                     TypeDef = typedef
                     Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                     Args = [||]
                     DeprecationReason = deprecationReason
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member Field(name : string, typedef : #OutputDef<'Res>, description : string,
                            [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res>,
                            ?deprecationReason : string) : FieldDef<'Val, 'Res> =

            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                     Args = [||]
                     DeprecationReason = deprecationReason
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member Field(name : string, typedef : #OutputDef<'Res>, args : InputFieldDef list,
                            [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res>,
                            ?deprecationReason : string) : FieldDef<'Val, 'Res> =
            upcast { FieldDefinition.Name = name
                     Description = None
                     TypeDef = typedef
                     Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                     Args = args |> List.toArray
                     DeprecationReason = deprecationReason
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        static member Field(name : string, typedef : #OutputDef<'Res>, description : string, args : InputFieldDef list,
                            [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res>,
                            ?deprecationReason : string) : FieldDef<'Val, 'Res> =
            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                     Args = args |> List.toArray
                     DeprecationReason = deprecationReason
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside object type with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member AsyncField(name : string, typedef : #OutputDef<'Res>,
                                 [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res>>,
                                 ?deprecationReason : string) : FieldDef<'Val, 'Res> =
            upcast { FieldDefinition.Name = name
                     Description = None
                     TypeDef = typedef
                     Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                     Args = [||]
                     DeprecationReason = deprecationReason
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside object type with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, description : string,
                                 [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res>>,
                                 ?deprecationReason : string) : FieldDef<'Val, 'Res> =
            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                     Args = [||]
                     DeprecationReason = deprecationReason
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside object type with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, args : InputFieldDef list,
                                 [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res>>,
                                 ?deprecationReason : string) : FieldDef<'Val, 'Res> =
            upcast { FieldDefinition.Name = name
                     Description = None
                     TypeDef = typedef
                     Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                     Args = args |> List.toArray
                     DeprecationReason = deprecationReason
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside object type with asynchronously resolved value. Fields is marked as deprecated.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, description : string,
                                 args : InputFieldDef list,
                                 [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res>>,
                                 ?deprecationReason : string) : FieldDef<'Val, 'Res> =
            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                     Args = args |> List.toArray
                     DeprecationReason = deprecationReason
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates a custom defined field using a custom field execution function.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="execField">Expression used to execute the field.</param>
        static member CustomField(name : string, [<ReflectedDefinition(true)>] execField : Expr<ExecuteField>) : FieldDef<'Val, obj> =
            upcast { FieldDefinition.Name = name
                     Description = None
                     TypeDef = ObjType
                     Resolve = ResolveExpr(execField)
                     Args = [||]
                     DeprecationReason = None
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates a subscription field inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = None
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty }

        /// <summary>
        /// Creates a subscription field inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>,
                                        tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = None
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver }

        /// <summary>
        /// Creates a subscription field inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        description: string,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty }

        /// <summary>
        /// Creates a subscription field inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        description: string,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>,
                                        tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver }

        /// <summary>
        /// Creates a subscription field inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        description: string,
                                        args: InputFieldDef list,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = args |> List.toArray
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty }

        /// <summary>
        /// Creates a subscription field inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        description: string,
                                        args: InputFieldDef list,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>,
                                        tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = args |> List.toArray
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver }

        /// <summary>
        /// Creates a subscription field inside object type. Field is marked as deprecated.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        description: string,
                                        args: InputFieldDef list,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>,
                                        deprecationReason : string): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = Some deprecationReason
                     Args = args |> List.toArray
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty }

        /// <summary>
        /// Creates a subscription field inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        description: string,
                                        args: InputFieldDef list,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>,
                                        tagsResolver : TagsResolver,
                                        deprecationReason : string): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = Some deprecationReason
                     Args = args |> List.toArray
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver }

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = None
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty }

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>,
                                             tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = None
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver }

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             description: string,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty }

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             description: string,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>,
                                             tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver }

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             description: string,
                                             args: InputFieldDef list,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = args |> List.toArray
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty }

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             description: string,
                                             args: InputFieldDef list,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>,
                                             tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = args |> List.toArray
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver }

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value. Field is marked as deprecated.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             description: string,
                                             args: InputFieldDef list,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>,
                                             deprecationReason : string): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = Some deprecationReason
                     Args = args |> List.toArray
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty }

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             description: string,
                                             args: InputFieldDef list,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>,
                                             tagsResolver : TagsResolver,
                                             deprecationReason : string): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = Some deprecationReason
                     Args = args |> List.toArray
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver }


        /// <summary>
        /// Creates an input field. Input fields are used like ordinary fileds in case of <see cref="InputObject"/>s,
        /// and can be used to define arguments to objects and interfaces fields.
        /// </summary>
        /// <param name="name">
        /// Field name. Must be unique in scope of the defining input object or withing field's argument list.
        /// </param>
        /// <param name="typedef">GraphQL type definition of the current input type</param>
        /// <param name="defaultValue">If defined, this value will be used when no matching input has been provided by the requester.</param>
        /// <param name="description">Optional input description. Usefull for generating documentation.</param>
        static member Input(name : string, typedef : #InputDef<'In>, ?defaultValue : 'In, ?description : string) : InputFieldDef =
            upcast { InputFieldDefinition.Name = name
                     Description = description
                     IsSkippable = false
                     TypeDef = typedef
                     DefaultValue = defaultValue
                     ExecuteInput = Unchecked.defaultof<ExecuteInput> }

        /// <summary>
        /// Creates an input field. Input fields are used like ordinary fileds in case of <see cref="InputObject"/>s,
        /// and can be used to define arguments to objects and interfaces fields.
        /// </summary>
        /// <param name="name">
        /// Field name. Must be unique in scope of the defining input object or withing field's argument list.
        /// </param>
        /// <param name="typedef">GraphQL type definition of the current input type</param>
        /// <param name="defaultValue">If defined, this value will be used when no matching input has been provided by the requester.</param>
        /// <param name="description">Optional input description. Usefull for generating documentation.</param>
        static member SkippableInput(name : string, typedef : #InputDef<'In>, ?description : string) : InputFieldDef =
            upcast { InputFieldDefinition.Name = name
                     Description = description |> Option.map (fun s -> s + " Skip this field if you want to avoid saving it")
                     IsSkippable = true
                     TypeDef =
                        match (box typedef) with
                        | :? NullableDef<'In> as n -> n
                        | _ -> Nullable typedef
                     DefaultValue = None
                     ExecuteInput = Unchecked.defaultof<ExecuteInput> }

        /// <summary>
        /// Creates a custom GraphQL interface type. It's needs to be implemented by object types and should not be used alone.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="fields">List of fields defined by the current interface.</param>
        /// <param name="description">Optional input description. Usefull for generating documentation.</param>
        /// <param name="resolveType">Optional function used to resolve actual Object definition of the .NET object provided as an input.</param>
        static member Interface(name : string, fields : FieldDef<'Val> list, ?description : string,
                                ?resolveType : obj -> ObjectDef) : InterfaceDef<'Val> =
            upcast { InterfaceDefinition.Name = name
                     Description = description
                     FieldsFn = fun () -> fields |> List.toArray
                     ResolveType = resolveType }

        /// <summary>
        /// Creates a custom GraphQL union type, materialized as one of the types defined. It can be used as interface/object type field.
        /// In order to work with F# discriminated unions, <paramref name="resolveValue"/> function may be used to unwrap objects
        /// nested as discriminated union cases.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="options">Interfaces or objects used as union cases.</param>
        /// <param name="resolveValue">Given F# discriminated union as input, returns .NET object valid with one of the defined GraphQL union cases.</param>
        /// <param name="resolveType">Resolves an Object definition of one of possible types, give input object.</param>
        /// <param name="description">Optional union description. Usefull for generating documentation.</param>
        static member Union(name : string, options : ObjectDef list, resolveValue : 'In -> 'Out,
                            ?resolveType : 'In -> ObjectDef, ?description : string) : UnionDef<'In> =
            upcast { UnionDefinition.Name = name
                     Description = description
                     Options = options |> List.toArray
                     ResolveType = resolveType
                     ResolveValue = resolveValue }

    /// Common space for all definition helper that use the other definitions and must access them lazily.
    [<AbstractClass; Sealed>]
    type DefineRec =

        /// <summary>
        /// Creates a GraphQL custom output object type that has a field of a type that references this object type recursively.
        /// It can be used as a valid output but not as an input object
        /// (see <see cref="InputObject"/> for more details).
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="fieldsFn">
        /// Function which generates a list of fields defined by the current object. Usefull, when object defines recursive dependencies.
        /// </param>
        /// <param name="description">Optional object description. Usefull for generating documentation.</param>
        /// <param name="interfaces">
        /// List of implemented interfaces. Object must explicitly define all fields from all interfaces it implements.
        /// </param>
        /// <param name="isTypeOf">
        /// Optional function used to determine if provided .NET object instance matches current object definition.
        /// </param>
        static member Object(name : string, fieldsFn : unit -> FieldDef<'Val> list, ?description : string,
                             ?interfaces : InterfaceDef list, ?isTypeOf : obj -> bool) : ObjectDef<'Val> =
            upcast { ObjectDefinition.Name = name
                     Description = description
                     FieldsFn =
                         lazy (fieldsFn()
                               |> List.map (fun f -> f.Name, f)
                               |> Map.ofList)
                     Implements = defaultArg (Option.map List.toArray interfaces) [||]
                     IsTypeOf = isTypeOf }

        /// <summary>
        /// Creates a custom GraphQL input object type. Unlike GraphQL objects, input objects are valid input types,
        /// that can be included in GraphQL query strings. Input object maps to a .NET type, which can be standard
        /// .NET class or struct, or a F# record.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="fieldsFn">
        /// Function which generates a list of input fields defined by the current input object. Useful, when object defines recursive dependencies.
        /// </param>
        /// <param name="description">Optional input object description. Useful for generating documentation.</param>
        static member InputObject(name : string, fieldsFn : unit -> InputFieldDef list, ?description : string) : InputObjectDefinition<'Out> =
            { Name = name
              Fields = lazy (fieldsFn () |> List.toArray)
              Description = description
              Validator = GQLValidator.empty
              ExecuteInput = Unchecked.defaultof<_> }

        /// <summary>
        /// Creates a custom GraphQL input object type. Unlike GraphQL objects, input objects are valid input types,
        /// that can be included in GraphQL query strings. Input object maps to a .NET type, which can be standard
        /// .NET class or struct, or a F# record.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="fieldsFn">
        /// Function which generates a list of input fields defined by the current input object. Useful, when object defines recursive dependencies.
        /// </param>
        /// <param name="validator">Object validator.</param>
        /// <param name="description">Optional input object description. Useful for generating documentation.</param>
        static member InputObject(name : string, fieldsFn : unit -> InputFieldDef list, validator: GQLValidator<'Out>, ?description : string) : InputObjectDefinition<'Out> =
            { Name = name
              Fields = lazy (fieldsFn () |> List.toArray)
              Description = description
              Validator = validator
              ExecuteInput = Unchecked.defaultof<_> }

        /// <summary>
        /// Creates a custom GraphQL interface type that has a field of type that refernces this interface type recurively.
        /// It's needs to be implemented by object types and should not be used alone.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="fieldsFn">
        /// Function which generates a list of fields defined by the current interface. Usefull, when object defines recursive dependencies.
        /// </param>
        /// <param name="description">Optional input description. Usefull for generating documentation.</param>
        /// <param name="resolveType">Optional function used to resolve actual Object definition of the .NET object provided as an input.</param>
        static member Interface(name : string, fieldsFn : unit -> FieldDef<'Val> list, ?description : string,
                                ?resolveType : obj -> ObjectDef) : InterfaceDef<'Val> =
            upcast { InterfaceDefinition.Name = name
                     Description = description
                     FieldsFn = fun () -> fieldsFn() |> List.toArray
                     ResolveType = resolveType }

