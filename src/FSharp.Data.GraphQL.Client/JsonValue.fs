// --------------------------------------------------------------------------------------
// Copyright (c) Microsoft Corporation 2005-2012.
// This sample code is provided "as is" without warranty of any kind.
// We disclaim all warranties, either express or implied, including the
// warranties of merchantability and fitness for a particular purpose.
//
// A simple F# portable parser for JSON data
// --------------------------------------------------------------------------------------

/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System
open System.IO
open System.ComponentModel
open System.Globalization
open System.Text

/// Specifies the formatting behaviour of JSON values.
[<RequireQualifiedAccess>]
type JsonSaveOptions =
  /// Format (indent) the JsonValue.
  | None = 0
  /// Print the JsonValue in one line in a compact way.
  | DisableFormatting = 1

/// Represents a JSON value. Large numbers that do not fit in the
/// Decimal type are represented using the Float case, while
/// smaller numbers are represented as decimals to avoid precision loss.
[<RequireQualifiedAccess>]
[<StructuredFormatDisplay("{_Print}")>]
type JsonValue =
  | Integer of int
  | String of string
  | Float of float
  | Record of properties:(string * JsonValue)[]
  | Array of elements:JsonValue[]
  | Boolean of bool
  | Null  

  ///  <exclude />
  [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
  [<CompilerMessageAttribute("This property is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
  member x._Print =
    let str = x.ToString()
    if str.Length > 512 then str.Substring(0, 509) + "..."
    else str

  /// Serializes the JsonValue to the specified System.IO.TextWriter.
  member x.WriteTo (w:TextWriter, saveOptions) =
    let newLine =
      if saveOptions = JsonSaveOptions.None then
        fun indentation plus ->
          w.WriteLine()
          System.String(' ', indentation + plus) |> w.Write
      else
        fun _ _ -> ()
    let propSep =
      if saveOptions = JsonSaveOptions.None then "\": "
      else "\":"
    let rec serialize indentation = function
      | Null -> w.Write "null"
      | Boolean b -> w.Write(if b then "true" else "false")
      | Float number -> w.Write number
      | Integer number -> w.Write number
      | String s ->
          w.Write "\""
          JsonValue.JsonStringEncodeTo w s
          w.Write "\""
      | Record properties ->
          w.Write "{"                      
          for i = 0 to properties.Length - 1 do
            let k,v = properties.[i]
            if i > 0 then w.Write ","
            newLine indentation 2            
            w.Write "\""
            JsonValue.JsonStringEncodeTo w k
            w.Write propSep
            serialize (indentation + 2) v
          newLine indentation 0
          w.Write "}"
      | Array elements ->
          w.Write "["
          for i = 0 to elements.Length - 1 do
            if i > 0 then w.Write ","
            newLine indentation 2
            serialize (indentation + 2) elements.[i]
          if elements.Length > 0 then
            newLine indentation 0
          w.Write "]"
    serialize 0 x 

  static member internal JsonStringEncodeTo (w:TextWriter) (value:string) =
    if String.IsNullOrEmpty value then ()
    else 
      for i = 0 to value.Length - 1 do
        let c = value.[i]
        let ci = int c
        if ci >= 0 && ci <= 7 || ci = 11 || ci >= 14 && ci <= 31 then
          w.Write("\\u{0:x4}", ci) |> ignore
        else 
          match c with
          | '\b' -> w.Write "\\b"
          | '\t' -> w.Write "\\t"
          | '\n' -> w.Write "\\n"
          | '\f' -> w.Write "\\f"
          | '\r' -> w.Write "\\r"
          | '"'  -> w.Write "\\\""
          | '\\' -> w.Write "\\\\"
          | _    -> w.Write c

  member x.ToString saveOptions =
    let w = new StringWriter(CultureInfo.InvariantCulture)
    x.WriteTo(w, saveOptions)
    w.GetStringBuilder().ToString()

  override x.ToString() = x.ToString(JsonSaveOptions.None)

type private JsonParser(jsonText:string) =
    let mutable i = 0
    let s = jsonText
    let buf = StringBuilder()

    let skipWhitespace() =
      while i < s.Length && Char.IsWhiteSpace s.[i] do
        i <- i + 1
    let isNumChar c =
      Char.IsDigit c || c = '.' || c='e' || c='E' || c='+' || c='-'
    let isIntChar c =
      Char.IsDigit c || c = '+' || c = '-'
    let throw() =
      let msg =
        sprintf
          "Invalid JSON starting at character %d, snippet = \n----\n%s\n-----\njson = \n------\n%s\n-------" 
          i (jsonText.[(max 0 (i-10))..(min (jsonText.Length-1) (i+10))]) (if jsonText.Length > 1000 then jsonText.Substring(0, 1000) else jsonText)
      failwith msg
    let ensure cond =
      if not cond then throw()

    let rec parseValue() =
        skipWhitespace()
        ensure(i < s.Length)
        match s.[i] with
        | '"' -> JsonValue.String(parseString())
        | '-' -> parseNum()
        | c when Char.IsDigit(c) -> parseNum()
        | '{' -> parseObject()
        | '[' -> parseArray()
        | 't' -> parseLiteral("true", JsonValue.Boolean true)
        | 'f' -> parseLiteral("false", JsonValue.Boolean false)
        | 'n' -> parseLiteral("null", JsonValue.Null)
        | _ -> throw()

    and parseRootValue() =
        skipWhitespace()
        ensure(i < s.Length)
        match s.[i] with
        | '{' -> parseObject()
        | '[' -> parseArray()
        | _ -> throw()

    and parseString() =
        ensure(i < s.Length && s.[i] = '"')
        i <- i + 1
        while i < s.Length && s.[i] <> '"' do
            if s.[i] = '\\' then
                ensure(i+1 < s.Length)
                match s.[i+1] with
                | 'b' -> buf.Append('\b') |> ignore
                | 'f' -> buf.Append('\f') |> ignore
                | 'n' -> buf.Append('\n') |> ignore
                | 't' -> buf.Append('\t') |> ignore
                | 'r' -> buf.Append('\r') |> ignore
                | '\\' -> buf.Append('\\') |> ignore
                | '/' -> buf.Append('/') |> ignore
                | '"' -> buf.Append('"') |> ignore
                | 'u' ->
                    ensure(i+5 < s.Length)
                    let hexdigit d =
                        if d >= '0' && d <= '9' then int32 d - int32 '0'
                        elif d >= 'a' && d <= 'f' then int32 d - int32 'a' + 10
                        elif d >= 'A' && d <= 'F' then int32 d - int32 'A' + 10
                        else failwith "hexdigit"
                    let unicodeChar (s:string) =
                        if s.Length <> 4 then failwith "unicodeChar";
                        char (hexdigit s.[0] * 4096 + hexdigit s.[1] * 256 + hexdigit s.[2] * 16 + hexdigit s.[3])
                    let ch = unicodeChar (s.Substring(i+2, 4))
                    buf.Append(ch) |> ignore
                    i <- i + 4
                | 'U' ->
                    ensure(i+9 < s.Length)
                    let unicodeChar (s:string) =
                        if s.Length <> 8 then failwith "unicodeChar";
                        if s.[0..1] <> "00" then failwith "unicodeChar";
                        UnicodeHelper.getUnicodeSurrogatePair <| System.UInt32.Parse(s, NumberStyles.HexNumber) 
                    let lead, trail = unicodeChar (s.Substring(i+2, 8))
                    buf.Append(lead) |> ignore
                    buf.Append(trail) |> ignore
                    i <- i + 8
                | _ -> throw()
                i <- i + 2
            else
                buf.Append(s.[i]) |> ignore
                i <- i + 1
        ensure(i < s.Length && s.[i] = '"')
        i <- i + 1
        let str = buf.ToString()
        buf.Clear() |> ignore
        str

    and parseNum() =
        let start = i
        while i < s.Length && (isNumChar s.[i]) do
            i <- i + 1
        let len = i - start
        let sub = s.Substring(start,len)
        let asFloat (sub : string) =
            match TextConversions.AsFloat [| |] false CultureInfo.InvariantCulture sub with
            | Some x -> JsonValue.Float x
            | _ -> throw()
        if Seq.forall isIntChar sub
        then
            match TextConversions.AsInteger CultureInfo.InvariantCulture sub with
            | Some x -> JsonValue.Integer x
            | _ -> asFloat sub
        else asFloat sub

    and parsePair() =
        let key = parseString()
        skipWhitespace()
        ensure(i < s.Length && s.[i] = ':')
        i <- i + 1
        skipWhitespace()
        key, parseValue()

    and parseObject() =
        ensure(i < s.Length && s.[i] = '{')
        i <- i + 1
        skipWhitespace()
        let pairs = ResizeArray<_>()
        if i < s.Length && s.[i] = '"' then
            pairs.Add(parsePair())
            skipWhitespace()
            while i < s.Length && s.[i] = ',' do
                i <- i + 1
                skipWhitespace()
                pairs.Add(parsePair())
                skipWhitespace()
        ensure(i < s.Length && s.[i] = '}')
        i <- i + 1
        JsonValue.Record(pairs.ToArray())

    and parseArray() =
        ensure(i < s.Length && s.[i] = '[')
        i <- i + 1
        skipWhitespace()
        let vals = ResizeArray<_>()
        if i < s.Length && s.[i] <> ']' then
            vals.Add(parseValue())
            skipWhitespace()
            while i < s.Length && s.[i] = ',' do
                i <- i + 1
                skipWhitespace()
                vals.Add(parseValue())
                skipWhitespace()
        ensure(i < s.Length && s.[i] = ']')
        i <- i + 1
        JsonValue.Array(vals.ToArray())

    and parseLiteral(expected, r) =
        ensure(i+expected.Length < s.Length)
        for j in 0 .. expected.Length - 1 do
            ensure(s.[i+j] = expected.[j])
        i <- i + expected.Length
        r

    member __.Parse() =
        let value = parseRootValue()
        skipWhitespace()
        if i <> s.Length then
            throw()
        value

type JsonValue with
  /// Parses the specified JSON string.
  static member Parse(text) = JsonParser(text).Parse()

  /// Attempts to parse the specified JSON string.
  static member TryParse(text) =
    try Some <| JsonParser(text).Parse()
    with _ -> None