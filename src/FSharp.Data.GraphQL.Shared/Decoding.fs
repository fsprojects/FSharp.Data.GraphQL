namespace FSharp.Data.GraphQL.Decoding

open System
open System.Globalization
open FSharp.Data.GraphQL.Ast
open FsToolkit.ErrorHandling

type DecodePathSegment =
  | AtIndex of int
  | InProperty of string
  | InInputObject of typeName : string

type DecodePath = DecodePathSegment list

[<RequireQualifiedAccess>]
type DecodeErrorReason =
  | TypeMismatch of expected : string * actual : string
  | Failure of string

type DecodeError =
  {
    Reason : DecodeErrorReason
    Path : DecodePath
  }

type DecodeResult<'t> = Result<'t, DecodeError>

type Decoder<'t> = Value -> DecodeResult<'t>

type CasingConvention =
  | DropCamelCase // fooBar
  | SnakeCase // foo_bar

[<NoComparison>]
type AutoDecoderSettings =
  internal {
    // If an object property is missing, we can replace it with null instead of an error
    ReplaceMissingWithNull : bool
    // When matching record fields we can automatically transform the field names to different casing conventions
    // e.g. In JavaScript we might have "fooBar" and in .NET "FooBar"
    Casing : CasingConvention option
    // Mapping from full-type name to a boxed decoder
    Customizations : Map<string, Decoder<obj>>
  }

module DecodeError =

  let typeMismatch (expected : string) (actual : string) =
    {
      Reason = DecodeErrorReason.TypeMismatch (expected, actual)
      Path = []
    }

  let reason (reason : string) =
    {
      Reason = DecodeErrorReason.Failure reason
      Path = []
    }

  let nest (pathSegment : DecodePathSegment) (error : DecodeError) =
    { error with Path = pathSegment :: error.Path }

module Decoder =

  let always (x) : Decoder<'t> =
    (fun _ -> x)

  let succeed (x) : Decoder<'t> =
    always (Ok x)

  let fail (error) : Decoder<'t> =
    always (Error error)

  let map (f : 't -> 'u) (m : Decoder<'t>) : Decoder<'u> =
    (fun value ->
      match m value with
      | Ok t -> Ok (f t)
      | Error error -> Error error)

  let mapError (f : DecodeError -> DecodeError) (m : Decoder<'t>) : Decoder<'t> =
    (fun value ->
      match m value with
      | Ok t -> Ok t
      | Error error -> Error (f error))

  let bind (f : 't -> Decoder<'u>) (m : Decoder<'t>) : Decoder<'u> =
    (fun value ->
      match m value with
      | Ok t -> (f t) value
      | Error error -> Error error)

  let inline box (d : Decoder<'t>) : Decoder<obj> =
    map box d

  let inline unbox (d : Decoder<obj>) : Decoder<'t> =
    map unbox d

module AutoDecoderSettings =

  let empty =
    {
      ReplaceMissingWithNull = false
      Casing = None
      Customizations = Map.empty
    }

  let replaceMissingWithNull (flag : bool) (settings : AutoDecoderSettings) =
    {
      settings with
        ReplaceMissingWithNull = flag
    }

  let casing (conv : CasingConvention) (settings : AutoDecoderSettings) =
    {
      settings with
        Casing = Some conv
    }

  let customize (decoder : Decoder<'t>) (settings : AutoDecoderSettings) =
    let t = typeof<'t>

    {
      settings with
        Customizations =
          settings.Customizations
          |> Map.add t.FullName (Decoder.box decoder)
    }

  let customizeBoxed (t : Type) (decoder : Decoder<obj>) (settings : AutoDecoderSettings) =
    {
      settings with
        Customizations =
          settings.Customizations
          |> Map.add t.FullName (Decoder.box decoder)
    }

module CasingConvention =

  open System.Text.RegularExpressions

  let private charToString (c : char) : string =
    String([| c |])

  let private lowerFirst (name : string) =
    if name.Length > 0
    then
      if Char.IsUpper name.[0] then
        charToString(Char.ToLowerInvariant(name.[0])) + (name.Substring(1))
      else
        name
    else
      name

  let apply (name : string) (conv : CasingConvention) =
    match conv with
    | DropCamelCase -> lowerFirst name
    | SnakeCase -> Regex.Replace(lowerFirst name, "[A-Z]", "_$0").ToLowerInvariant()

module Decode =

  let nullValue : Decoder<unit> =
    fun value ->
      match value with
      | NullValue -> Ok ()
      | x ->
        Error (DecodeError.reason (sprintf "expected None but got %A" x))

  let field (name : string) (fieldDecoder : Decoder<'t>) : Decoder<'t> =
    fun value ->
      match value with
      | ObjectValue m ->
        match m |> Map.tryFind name with
        | Some value ->
          let fieldDecoder =
            fieldDecoder
            |> Decoder.mapError (DecodeError.nest (InProperty name))

          fieldDecoder value
        | None ->
          let propertiesFound =
            m
            |> Map.toSeq
            |> Seq.map (fst >> (fun x -> "\"" + x + "\""))
            |> String.concat "; "

          let message = sprintf "Missing object property \"%s\", but found the following properties: %s" name propertiesFound

          Error (DecodeError.reason message)
      | x ->
        Error (DecodeError.reason (sprintf "expected an Object but got %A" x))

  let stringish : Decoder<string> =
    fun value ->
      match value with
      | StringValue s -> Ok s
      | IntValue i -> Ok (string i)
      | FloatValue f -> Ok (string f)
      | BooleanValue b -> Ok (if b then "true" else "false")
      | NullValue ->
        Error (DecodeError.reason (sprintf "expected value of type String but got None"))
      | x ->
        Error (DecodeError.reason (sprintf "expected value of type String but got %A" x))

  let string : Decoder<string> =
    fun value ->
      match value with
      | StringValue s -> Ok s
      | x ->
        Error (DecodeError.reason (sprintf "expected value of type String but got %A" x))

  let intish : Decoder<int> =
    fun value ->
      match value with
      | IntValue i -> Ok (int i)
      | BooleanValue true -> Ok 1
      | BooleanValue false -> Ok 0
      | FloatValue f -> Ok (int f)
      | StringValue s ->
        match Int32.TryParse s with
        | (true, i) -> Ok i
        | _ ->
          Error (DecodeError.reason (sprintf "Expected an int-like value but found the string \"%s\"" s))
      | x ->
        Error (DecodeError.reason (sprintf "Expected an int-like value but found %A" x))

  let int : Decoder<int> =
    fun value ->
      match value with
      | IntValue i -> Ok (int i)
      | x ->
        Error (DecodeError.reason (sprintf "Expected an int but found %A" x))

  let floatish : Decoder<double> =
    fun value ->
      match value with
      | IntValue i -> Ok (double i)
      | FloatValue f -> Ok f
      | StringValue s ->
          match Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
          | true, f -> Ok f
          | false, _ ->
            Error (DecodeError.reason (sprintf "Expected an float-like value but found the string \"%s\"" s))
      | BooleanValue b -> Ok (if b then 1. else 0.)
      | x ->
        Error (DecodeError.reason (sprintf "Expected a float but found %A" x))

  let float : Decoder<double> =
    fun value ->
      match value with
      | FloatValue f -> Ok f
      | x ->
        Error (DecodeError.reason (sprintf "Expected a float but found %A" x))

  let longish : Decoder<int64> =
    fun value ->
      match value with
      | IntValue i -> Ok (int64 i)
      | FloatValue f -> Ok (int64 f)
      | StringValue s ->
          match Int64.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
          | true, i -> Ok i
          | false, _ ->
            Error (DecodeError.reason (sprintf "Expected an long-like value but found the string \"%s\"" s))
      | BooleanValue b -> Ok (if b then 1L else 0L)
      | x ->
        Error (DecodeError.reason (sprintf "Expected a long-like but found %A" x))

  let boolish : Decoder<bool> =
    fun value ->
      match value with
      | NullValue -> Ok false
      | IntValue i -> Ok (i <> 0L)
      | FloatValue f -> Ok (f <> 0.)
      | BooleanValue b -> Ok b
      | StringValue s ->
        match Boolean.TryParse s with
        | (true, b) -> Ok b
        | _ ->
          Error (DecodeError.reason (sprintf "Expected an bool-like value but found the string \"%s\"" s))
      | x ->
        Error (DecodeError.reason (sprintf "Expected an bool-like value but found %A" x))

  let bool : Decoder<bool> =
    fun value ->
      match value with
      | BooleanValue b -> Ok b
      | x ->
        Error (DecodeError.reason (sprintf "Expected a bool but found %A" x))

  let option (decoder : Decoder<'t>) : Decoder<'t option> =
    fun value ->
      match value with
      | NullValue -> Ok None
      | value ->
        (Decoder.map Some decoder) value

  let list (decoder : Decoder<'t>) : Decoder<'t list> =
    fun value ->
      match value with
      | ListValue xs ->
        xs
        |> List.mapi (fun i value ->
          (decoder |> Decoder.mapError (DecodeError.nest (AtIndex i))) value
        )
        |> List.sequenceResultM
      | x ->
        Error (DecodeError.reason (sprintf "Expected a list but found %A" x))

  let enum cases : Decoder<'t> =
    fun value ->
      match value with
      | EnumValue e ->
        match cases |> Map.tryFind e with
        | Some t -> Ok t
        | None ->
          Error (DecodeError.reason (sprintf "Unexpected value fo enum: %A" e))
      | x ->
        Error (DecodeError.reason (sprintf "Expected an enum but found %A" x))

  let uri : Decoder<Uri> =
    fun value ->
      match value with
      | StringValue s ->
          match Uri.TryCreate(s, UriKind.RelativeOrAbsolute) with
          | true, uri -> Ok uri
          | false, _ ->
            Error (DecodeError.reason (sprintf "Expected a valid URI but found the string \"%s\"" s))
      | x ->
        Error (DecodeError.reason (sprintf "Expected a string but found %A" x))

  let dateTime : Decoder<DateTime> =
    fun value ->
      match value with
      | StringValue s ->
          match DateTime.TryParse(s) with
          | true, dt -> Ok dt
          | false, _ ->
            Error (DecodeError.reason (sprintf "Expected a valid date-time but found the string \"%s\"" s))
      | x ->
        Error (DecodeError.reason (sprintf "Expected a string but found %A" x))

  let guid : Decoder<Guid> =
    fun value ->
      match value with
      | StringValue s ->
          match Guid.TryParse(s) with
          | true, g -> Ok g
          | false, _ ->
            Error (DecodeError.reason (sprintf "Expected a valid GUID but found the string \"%s\"" s))
      | x ->
        Error (DecodeError.reason (sprintf "Expected a string but found %A" x))

  open FSharp.Reflection

  let private isReferenceEnum t =
    if FSharpType.IsUnion t then
      FSharpType.GetUnionCases(t)
      |> Seq.forall (fun i -> i.GetFields().Length = 0)
    else
      false

  let rec autoObj (settings : AutoDecoderSettings) (t : Type) : Decoder<obj> =
    match Map.tryFind t.FullName settings.Customizations with
    | Some decoder ->
      decoder
    | None ->
      if t = typeof<bool> then
        (fun v -> bool v |> Result.map (fun t -> t |> box))
      elif t = typeof<string> then
        (fun v -> string v |> Result.map (fun t -> t |> box))
      elif t = typeof<int> then
        (fun v -> int v |> Result.map (fun t -> t |> box))
      elif FSharpType.IsRecord(t, allowAccessToPrivateRepresentation=true) then
        autoRecord settings t
      elif FSharpType.IsTuple t then
        autoTuple settings t
      elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<obj option> then
        autoOption settings t
      elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<obj list> then
        autoList settings t
      elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<System.Collections.Generic.IEnumerable<obj>> then
        autoEnumerable settings t
      elif isReferenceEnum t then
        autoUnionEnum settings t
      else
        (fun (value : Value) ->
          Error (DecodeError.reason (sprintf "We cannot automatically decode %A as type %A" value t)))

  and private autoRecord (settings : AutoDecoderSettings) (t : Type) =
    let fields = FSharpType.GetRecordFields(t, allowAccessToPrivateRepresentation=true)

    function
    | ObjectValue m ->
      result {
        let! values =
          fields
          |> Seq.map (fun field ->
            result {
              let transformedName =
                match settings.Casing with
                | Some strat -> CasingConvention.apply field.Name strat
                | None -> field.Name

              let! value =
                m
                |> Map.tryFind transformedName
                |> (fun o ->
                  if settings.ReplaceMissingWithNull then
                    o
                    |> Option.defaultValue (NullValue)
                    |> Some
                  else
                    o)
                |> Result.requireSome (
                  let propertiesFound =
                    m
                    |> Map.toSeq
                    |> Seq.map (fst >> (fun x -> "\"" + x + "\""))
                    |> String.concat "; "

                  let message = sprintf "Missing object property \"%s\", but found the following properties: %s" transformedName propertiesFound

                  DecodeError.reason message)

              let decoder =
                autoObj settings field.PropertyType

              let! decodedValue =
                decoder value
                |> Result.mapError (DecodeError.nest (InProperty field.Name))

              return box decodedValue
            }
          )
          |> List.ofSeq
          |> List.sequenceResultM

        return FSharpValue.MakeRecord(t, Seq.toArray values, allowAccessToPrivateRepresentation=true) |> box
      }
    | x -> Error (DecodeError.reason (sprintf "Expected an object but found %A" x))

  and private autoTuple (settings : AutoDecoderSettings) (t : Type) : Decoder<obj> =
    let els = FSharpType.GetTupleElements(t)

    if Seq.length els = 1 then
      autoObj settings t
    else
      function
      | ListValue xs ->
        if List.length xs <> Seq.length els then
          let message = sprintf "Incorrect number of tuple elements: expected %i but found %i" (Seq.length els) (List.length xs)

          Error (DecodeError.reason message)
        else
          result {
            let! elements =
              xs
              |> Seq.zip els
              |> Seq.mapi (fun i (t, v) ->
                (autoObj settings t) v
                |> Result.mapError (DecodeError.nest (AtIndex i))
              )
              |> Seq.toList
              |> List.sequenceResultM

            return FSharpValue.MakeTuple(Seq.toArray elements, t)
          }
      | x ->
        Error (DecodeError.reason (sprintf "Expected a list but found %A" x))

  and private autoUnionEnum (settings : AutoDecoderSettings) (t : Type) : Decoder<obj> =
    let cases =
      FSharpType.GetUnionCases(t)
      |> Seq.map (fun uci ->
        let transformedName =
          match settings.Casing with
          | Some casing -> CasingConvention.apply uci.Name casing
          | None -> uci.Name

        let value = FSharpValue.MakeUnion(uci, [||], allowAccessToPrivateRepresentation=true)

        transformedName, value
      )
      |> Map.ofSeq

    let tryString s =
      match Map.tryFind s cases with
      | Some case -> Ok case
      | None ->
        Error (DecodeError.reason (sprintf "Unexpected enum value \"%s\"" s))

    function
    | EnumValue s -> tryString s
    | StringValue s -> tryString s
    | x ->
      Error (DecodeError.reason (sprintf "Expected an enum but found %A" x))

  and private autoOption (settings : AutoDecoderSettings) (t : Type) : Decoder<obj> =
    let elementT = t.GenericTypeArguments.[0]
    let elementDecoder = autoObj settings elementT

    let ucis = FSharpType.GetUnionCases(t)

    function
    | NullValue -> Ok (box None)
    | value ->
      elementDecoder value
      |> Result.map (fun value -> FSharpValue.MakeUnion(ucis.[1], [| value |]))

  and private autoList (settings : AutoDecoderSettings) (t : Type) : Decoder<obj> =
    let elementT = t.GenericTypeArguments.[0]
    let elementDecoder = autoObj settings elementT

    function
    | ListValue values ->
      let ucis = FSharpType.GetUnionCases(t, allowAccessToPrivateRepresentation=true)
      let empty = FSharpValue.MakeUnion(ucis.[0], [||], allowAccessToPrivateRepresentation=true)

      (List.indexed values, Ok empty)
      ||> Seq.foldBack (fun (index, value) acc ->
        match acc with
        | Error _ -> acc
        | Ok acc ->
          match elementDecoder value with
          | Error error ->
            Error (DecodeError.nest (AtIndex index) error)
          | Ok result ->
            FSharpValue.MakeUnion(ucis.[1], [| result; acc |], allowAccessToPrivateRepresentation=true) |> Ok)
    | x ->
      Error (DecodeError.reason (sprintf "Expected a list but found %A" x))

  and private autoEnumerable (settings : AutoDecoderSettings) (t : Type) : Decoder<obj> =
    let elementT = t.GenericTypeArguments.[0]
    let objListType = typedefof<obj list>
    let listType = objListType.MakeGenericType(elementT)

    autoList settings listType

  let autoRecordFromFields (settings : AutoDecoderSettings) (fieldDecoders : Map<string, Decoder<obj>>) : Decoder<'t> =
    let t = typeof<'t>
    let fields = FSharpType.GetRecordFields(t, allowAccessToPrivateRepresentation=true)

    (fun (value : Value) ->
      match value with
      | ObjectValue values ->
        result {
          let! decodedValues =
            fields
            |> Seq.map (fun field ->
              result {
                let! fieldDecoder =
                  fieldDecoders
                  |> Map.tryFind field.Name
                  |> Result.requireSome (DecodeError.reason (sprintf "Could not find a decoder for %s" field.Name))

                let transformedName =
                  match settings.Casing with
                  | Some strat -> CasingConvention.apply field.Name strat
                  | None -> field.Name

                let! value =
                  values
                  |> Map.tryFind transformedName
                  |> (fun o ->
                    if settings.ReplaceMissingWithNull then
                      o
                      |> Option.defaultValue (NullValue)
                      |> Some
                    else
                      o)
                  |> Result.requireSome (DecodeError.reason (sprintf "Could not find a value for %s" transformedName))

                let! decodedValue = fieldDecoder value

                return decodedValue
              }
              |> Result.mapError (DecodeError.nest (InProperty field.Name)))
            |> Seq.toList
            |> List.sequenceResultM

          let record =
            FSharpValue.MakeRecord(t, Seq.toArray decodedValues, allowAccessToPrivateRepresentation=true)
            :?> 't

          return record
        }
      | _ ->
        Error (DecodeError.reason (sprintf "Expected an object but found %A" value)))

  let auto<'t> (casing : CasingConvention option) : Decoder<'t> =
    let settings =
      {
        ReplaceMissingWithNull = true
        Casing = casing
        Customizations = Map.empty
      }

    autoObj settings typeof<'t>
    |> Decoder.unbox

  let autoWithSettings (settings : AutoDecoderSettings) : Decoder<'t> =
    autoObj settings typeof<'t>
    |> Decoder.unbox

[<AutoOpen>]
module ComputationExpression =

  type DecoderBuilder() =
    member this.Bind(m, f) =
      Decoder.bind f m

    member this.Return(x) =
      Decoder.succeed x

  let decoder = DecoderBuilder()
