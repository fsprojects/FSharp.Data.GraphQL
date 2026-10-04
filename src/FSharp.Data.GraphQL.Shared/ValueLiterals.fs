/// <summary>
/// Prints .NET values of GraphQL input types as GraphQL value literals
/// (<see href="https://spec.graphql.org/October2021/#sec-Input-Values">§2.9 Input Values</see>), the format in which
/// <see href="https://spec.graphql.org/October2021/#sec-The-__InputValue-Type">introspection</see> reports
/// the default value of an argument or an input object field.
/// </summary>
module internal FSharp.Data.GraphQL.ValueLiterals

open System
open System.Collections
open System.Globalization
open System.Numerics
open System.Reflection
open System.Text
open System.Text.Json
open FSharp.Reflection
open FSharp.Data.GraphQL.Ast.Extensions
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns

/// Whether the text is a GraphQL name, /[_A-Za-z][_0-9A-Za-z]*/, which can name an input object field.
let private isName (text : string) =
    let isNameStart character =
        character = '_'
        || (character >= 'A' && character <= 'Z')
        || (character >= 'a' && character <= 'z')
    text.Length > 0
    && isNameStart text[0]
    && text
       |> String.forall (fun character ->
           isNameStart character
           || (character >= '0' && character <= '9'))

/// Appends the items separated by commas, the way lists and input objects separate them,
/// and stops at the first item that has no literal.
let private tryAppendSeparated (builder : StringBuilder) (tryAppendItem : 'Item -> bool) (items : 'Item seq) =
    use enumerator = items.GetEnumerator ()
    let mutable printed = true
    let mutable isFirst = true
    while printed && enumerator.MoveNext () do
        if not isFirst then
            builder.Append (", ") |> ignore
        isFirst <- false
        printed <- tryAppendItem enumerator.Current
    printed

/// Appends a JSON value as the equal GraphQL value. A JSON number already is a valid GraphQL number,
/// so only an object with a key that is not a GraphQL name has no literal.
let rec private tryAppendJson (builder : StringBuilder) (element : JsonElement) =
    match element.ValueKind with
    | JsonValueKind.Null ->
        builder.Append ("null") |> ignore
        true
    | JsonValueKind.True ->
        builder.Append ("true") |> ignore
        true
    | JsonValueKind.False ->
        builder.Append ("false") |> ignore
        true
    | JsonValueKind.Number ->
        builder.Append (element.GetRawText ()) |> ignore
        true
    | JsonValueKind.String ->
        match element.GetString () with
        | null -> builder.Append ("null") |> ignore
        | text -> appendStringValue builder text
        true
    | JsonValueKind.Array ->
        builder.Append ('[') |> ignore
        let printed =
            element.EnumerateArray ()
            |> tryAppendSeparated builder (tryAppendJson builder)
        builder.Append (']') |> ignore
        printed
    | JsonValueKind.Object ->
        builder.Append ('{') |> ignore
        let printed =
            element.EnumerateObject ()
            |> tryAppendSeparated builder (fun property ->
                if isName property.Name then
                    builder.Append(property.Name).Append(": ") |> ignore
                    tryAppendJson builder property.Value
                else
                    false)
        builder.Append ('}') |> ignore
        printed
    | _ -> false

/// Appends the serialized value of a scalar, the one a response holds, as a GraphQL value. A value of a type
/// that has no literal of its own, like a date, a GUID or a URI, is printed the way a response serializes it.
let private tryAppendSerialized (jsonOptions : JsonSerializerOptions) (builder : StringBuilder) (serialized : objnull) =
    match serialized with
    | null ->
        builder.Append ("null") |> ignore
        true
    | :? bool as value ->
        builder.Append (if value then "true" else "false") |> ignore
        true
    | :? string as value ->
        appendStringValue builder value
        true
    | :? char as value ->
        appendStringValue builder (string value)
        true
    | :? sbyte
    | :? byte
    | :? int16
    | :? uint16
    | :? int
    | :? uint32
    | :? int64
    | :? uint64
    | :? BigInteger
    | :? decimal ->
        builder.Append (Convert.ToString (serialized, CultureInfo.InvariantCulture))
        |> ignore
        true
    // The round-trip format keeps the precision, and its exponent notation is valid in a GraphQL float
    | :? double as value when not (Double.IsNaN value || Double.IsInfinity value) ->
        builder.Append (value.ToString ("R", CultureInfo.InvariantCulture))
        |> ignore
        true
    | :? single as value when not (Single.IsNaN value || Single.IsInfinity value) ->
        builder.Append (value.ToString ("R", CultureInfo.InvariantCulture))
        |> ignore
        true
    // GraphQL has no literal for NaN and the infinities
    | :? double
    | :? single -> false
    | value -> tryAppendJson builder (JsonSerializer.SerializeToElement (value, value.GetType (), jsonOptions))

/// The property that holds the value of an input object field. The runtime passes the fields of an input object
/// to the constructor parameters of the same names, ignoring case, and records and classes expose
/// those parameters through properties of the same names.
let private tryFindFieldProperty (objectType : Type) (fieldName : string) : PropertyInfo voption =
    match objectType.GetProperty (fieldName, BindingFlags.Public ||| BindingFlags.Instance) with
    | null ->
        match
            objectType.GetProperty (
                fieldName,
                BindingFlags.Public
                ||| BindingFlags.Instance
                ||| BindingFlags.IgnoreCase
            )
        with
        | null -> ValueNone
        | property -> ValueSome property
    | property -> ValueSome property

/// The value of an input object field, or ValueNone when the field is a skippable one that is skipped.
let private includedValue (fieldValue : objnull) : objnull voption =
    match fieldValue with
    | null -> ValueSome null
    | value ->
        let valueType = value.GetType ()
        if ReflectionHelper.isSkippableType valueType then
            match FSharpValue.GetUnionFields (value, valueType) with
            | _, [| item |] -> ValueSome item
            | _ -> ValueNone
        else
            ValueSome value

/// Appends a value of the input type as a GraphQL value, or returns false when the value has no literal.
let rec private tryAppendValue (jsonOptions : JsonSerializerOptions) (builder : StringBuilder) (typeDef : TypeDef) (value : objnull) =
    // A nullable input holds an option, and so may an input object field that is not nullable,
    // when its constructor parameter is optional
    match Helpers.unwrap value with
    | null ->
        builder.Append ("null") |> ignore
        true
    | value ->
        match typeDef with
        | Nullable innerDef -> tryAppendValue jsonOptions builder innerDef value
        | List itemDef ->
            match value with
            // A string is a sequence of characters, but it is a single list item
            | :? string -> tryAppendValue jsonOptions builder itemDef value
            | :? IEnumerable as items ->
                builder.Append ('[') |> ignore
                let printed =
                    items
                    |> Seq.cast<obj>
                    |> tryAppendSeparated builder (tryAppendValue jsonOptions builder itemDef)
                builder.Append (']') |> ignore
                printed
            // Input coercion accepts a single item in place of a list
            | _ -> tryAppendValue jsonOptions builder itemDef value
        | Enum enumDef ->
            match
                enumDef.Options
                |> Array.vtryFind (fun enumValue -> enumValue.Value = value)
            with
            | ValueSome enumValue ->
                builder.Append (enumValue.Name) |> ignore
                true
            // A value that is not one of the enum values has no literal
            | ValueNone -> false
        | Scalar scalarDef ->
            match scalarDef.CoerceOutput value with
            | Some serialized -> tryAppendSerialized jsonOptions builder serialized
            // A value that the scalar cannot serialize has no literal
            | None -> false
        | InputObject objectDef ->
            let objectType = value.GetType ()
            let fieldProperties =
                objectDef.Fields
                |> Array.vchoose (fun field ->
                    tryFindFieldProperty objectType field.Name
                    |> ValueOption.map (fun property -> struct (field, property)))
            // The value of a field without a property is unknown
            if fieldProperties.Length < objectDef.Fields.Length then
                false
            else
                builder.Append ('{') |> ignore
                let printed =
                    fieldProperties
                    |> Seq.vchoose (fun struct (field, property) ->
                        property.GetValue value
                        |> includedValue
                        |> ValueOption.map (fun fieldValue -> struct (field, fieldValue)))
                    |> tryAppendSeparated builder (fun struct (field, fieldValue) ->
                        builder.Append(field.Name).Append(": ") |> ignore
                        tryAppendValue jsonOptions builder field.TypeDef fieldValue)
                builder.Append ('}') |> ignore
                printed
        // A custom input type, like the file upload, is printed the way it serializes to JSON
        | _ -> tryAppendJson builder (JsonSerializer.SerializeToElement (value, value.GetType (), jsonOptions))

/// <summary>
/// Prints a value of an input type as a GraphQL value literal, the format in which introspection reports
/// the default value of an argument or an input object field.
/// </summary>
/// <remarks>
/// <para>
/// An enum value is printed as the name of its <see cref="T:FSharp.Data.GraphQL.Types.EnumValue`1"/>,
/// a scalar value as its output coercion serializes it, and an input object with the GraphQL names of its fields.
/// </para>
/// <para>
/// The function never throws: the introspected schema backs the validation of every request,
/// so a default value that fails to print must not fail them all.
/// </para>
/// </remarks>
/// <param name="jsonOptions">
/// The JSON options of the schema. They serialize a scalar value that has no literal of its own, like a date,
/// the same way they serialize it in a response.
/// </param>
/// <param name="typeDef">The input type of the value.</param>
/// <param name="value">The value to print.</param>
/// <returns>
/// The literal, or <see cref="ValueNone"/> when the value has none: an enum value that is not one of the enum values,
/// a float that is not a number or is infinite, a value that its scalar cannot serialize, or an input object
/// that has no property for one of its fields.
/// </returns>
let tryPrint (jsonOptions : JsonSerializerOptions) (typeDef : InputDef) (value : objnull) : string voption =
    let builder = StringBuilder ()
    let printed =
        try
            tryAppendValue jsonOptions builder typeDef value
        with _ ->
            // An output coercion, a property getter or a JSON converter of a custom type has thrown
            false
    if printed then
        ValueSome (builder.ToString ())
    else
        ValueNone
