namespace FSharp.Data.GraphQL.Client
open System.ComponentModel
    /// Specifies the formatting behaviour of JSON values.
    [<RequireQualifiedAccess; Struct>]
    type JsonSaveOptions =
        | None = 0
        | DisableFormatting = 1

    /// Represents a JSON value. Large numbers that do not fit in the
    /// Decimal type are represented using the Float case, while
    /// smaller numbers are represented as decimals to avoid precision loss.
    [<RequireQualifiedAccess; StructuredFormatDisplay ("{_Print}")>]
    type JsonValue =
        | Integer of int
        | String of string
        | Float of float
        | Record of properties: (string * JsonValue)[]
        | Array of elements: JsonValue[]
        | Boolean of bool
        | Null

        static member
          internal JsonStringEncodeTo: w: System.IO.TextWriter ->
                                         value: string -> unit

        /// Parses the specified JSON string.
        static member Parse: text: string -> JsonValue

        /// Attempts to parse the specified JSON string.
        static member TryParse: text: string -> JsonValue option

        override ToString: unit -> string

        member ToString: saveOptions: JsonSaveOptions -> string

        /// Serializes the JsonValue to the specified System.IO.TextWriter.
        member
          WriteTo: w: System.IO.TextWriter * saveOptions: JsonSaveOptions ->
                     unit

        ///  <exclude />
        [<EditorBrowsableAttribute(EditorBrowsableState.Never);
          CompilerMessageAttribute("This property is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
        member _Print: string

