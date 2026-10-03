module FSharp.Data.GraphQL.Tests.ValueLiteralsTests

open System
open System.Text.Json.Serialization
open Xunit

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

[<RequireQualifiedAccess>]
type AttendanceMode =
    | Local
    | Remote
    /// Not a value of the GraphQL enum
    | Hybrid

type Attendee = { UserId : string; Nickname : string option; Modes : AttendanceMode list }

type Meeting = { Title : string; Host : Attendee }

type Draft = { Title : string; Note : string Skippable }

/// Receives the user ID through its constructor, but exposes it under another name
type Anonymous (userId : string) =
    member _.Id = userId

let private attendanceModeType =
    Define.Enum<AttendanceMode>(
        "AttendanceMode",
        [
            Define.EnumValue ("LOCAL", AttendanceMode.Local)
            Define.EnumValue ("REMOTE", AttendanceMode.Remote)
        ]
    )

let private dayType = Define.Enum<DayOfWeek>("Day", [ Define.EnumValue ("MONDAY", DayOfWeek.Monday) ])

let private attendeeType =
    Define.InputObject<Attendee>(
        "Attendee",
        [
            Define.Input ("userId", StringType)
            Define.Input ("nickname", Nullable StringType)
            Define.Input ("modes", ListOf attendanceModeType)
        ]
    )

let private meetingType =
    Define.InputObject<Meeting>("Meeting", [ Define.Input ("title", StringType); Define.Input ("host", attendeeType) ])

let private draftType =
    Define.InputObject<Draft>("Draft", [ Define.Input ("title", StringType); Define.SkippableInput ("note", StringType) ])

let private anonymousType =
    Define.InputObject<Anonymous>("Anonymous", [ Define.Input ("userId", StringType) ])

/// The JSON options of the default schema config
let private jsonOptions = JsonFSharpOptions.Default().ToJsonSerializerOptions()

let private print (typeDef : #InputDef) (value : obj) = ValueLiterals.tryPrint jsonOptions typeDef value

[<Fact>]
let ``Built-in scalars must be printed as GraphQL literals`` () =
    print IntType 42 |> equals (ValueSome "42")
    print IntType (-7) |> equals (ValueSome "-7")
    print LongType 9007199254740993L
    |> equals (ValueSome "9007199254740993")
    print FloatType 1.5 |> equals (ValueSome "1.5")
    print FloatType 2.0 |> equals (ValueSome "2")
    print FloatType 1e-7 |> equals (ValueSome "1E-07")
    print BooleanType false |> equals (ValueSome "false")
    print IDType "42" |> equals (ValueSome "\"42\"")
    print StringType "text" |> equals (ValueSome "\"text\"")

[<Fact>]
let ``Scalars without literals of their own must be printed as the strings they serialize to`` () =
    print DateTimeOffsetType (DateTimeOffset (2024, 1, 2, 3, 4, 5, TimeSpan.Zero))
    |> equals (ValueSome "\"2024-01-02T03:04:05+00:00\"")
    print DateOnlyType (DateOnly (2024, 1, 2))
    |> equals (ValueSome "\"2024-01-02\"")
    print GuidType (Guid "6e8bc430-9c3a-11d9-9669-0800200c9a66")
    |> equals (ValueSome "\"6e8bc430-9c3a-11d9-9669-0800200c9a66\"")
    print UriType (Uri "https://example.com/path?query=1")
    |> equals (ValueSome "\"https://example.com/path?query=1\"")

[<Fact>]
let ``Strings must be escaped and parse back to the same text`` () =
    // The line separator is built from its code, as Fantomas writes its escape sequence out unescaped
    let text =
        "\"quoted\" \\ / \b \f \n \r \t \u0001 \u007F "
        + string (char 0x2028)
        + " é 😀"
    let literal = print StringType text |> wantValueSome
    literal
    |> equals "\"\\\"quoted\\\" \\\\ / \\b \\f \\n \\r \\t \\u0001 \\u007F \\u2028 é 😀\""
    $"{{ field(argument: %s{literal}) }}"
    |> ParserTests.test (
        ParserTests.doc1 (ParserTests.queryWithSelection (ParserTests.fieldWithNameAndArgs "field" [ ParserTests.argString "argument" text ]))
    )

[<Fact>]
let ``Floats that are not numbers must have no literal`` () =
    print FloatType nan |> equals ValueNone
    print FloatType infinity |> equals ValueNone

[<Fact>]
let ``Enum values must be printed as their names`` () =
    print attendanceModeType AttendanceMode.Remote
    |> equals (ValueSome "REMOTE")
    print dayType DayOfWeek.Monday
    |> equals (ValueSome "MONDAY")

[<Fact>]
let ``Values outside of the enum must have no literal`` () =
    print attendanceModeType AttendanceMode.Hybrid
    |> equals ValueNone
    print dayType DayOfWeek.Sunday |> equals ValueNone

[<Fact>]
let ``Nullable values must be unwrapped, and missing ones printed as null`` () =
    print (Nullable StringType) (Some "text")
    |> equals (ValueSome "\"text\"")
    print (Nullable StringType) None
    |> equals (ValueSome "null")
    print (StructNullable IntType) (ValueSome 1)
    |> equals (ValueSome "1")
    print (StructNullable IntType) ValueNone
    |> equals (ValueSome "null")
    print (Nullable attendanceModeType) (Some AttendanceMode.Local)
    |> equals (ValueSome "LOCAL")

[<Fact>]
let ``Lists must be printed with their items`` () =
    print (ListOf attendanceModeType) [ AttendanceMode.Local; AttendanceMode.Remote ]
    |> equals (ValueSome "[LOCAL, REMOTE]")
    print (ListOf (Nullable IntType)) [| Some 1; None |]
    |> equals (ValueSome "[1, null]")
    print (ListOf (ListOf IntType)) [ [ 1; 2 ]; [] ]
    |> equals (ValueSome "[[1, 2], []]")
    print (ListOf StringType) "single"
    |> equals (ValueSome "\"single\"")

[<Fact>]
let ``List with an item without a literal must have no literal`` () =
    print (ListOf attendanceModeType) [ AttendanceMode.Local; AttendanceMode.Hybrid ]
    |> equals ValueNone

[<Fact>]
let ``Input objects must be printed with the GraphQL names of their fields`` () =
    let host = { UserId = "1"; Nickname = None; Modes = [ AttendanceMode.Remote ] }
    print meetingType { Title = "Daily"; Host = host }
    |> equals (ValueSome "{title: \"Daily\", host: {userId: \"1\", nickname: null, modes: [REMOTE]}}")

[<Fact>]
let ``Skipped fields must be absent from input objects`` () =
    print draftType { Title = "Draft"; Note = Skip }
    |> equals (ValueSome "{title: \"Draft\"}")
    print draftType { Title = "Draft"; Note = Include "Note" }
    |> equals (ValueSome "{title: \"Draft\", note: \"Note\"}")

[<Fact>]
let ``Input object without a property for a field must have no literal`` () = print anonymousType (Anonymous "1") |> equals ValueNone

[<Fact>]
let ``Custom scalars must be printed as the JSON they serialize to`` () =
    let pointType : ScalarDefinition<Map<string, int>> =
        Define.Scalar ("Point", (fun _ -> Error "Not used"), (fun value -> Some (value :?> Map<string, int>)))
    print pointType (Map [ "x", 1; "y", -2 ])
    |> equals (ValueSome "{x: 1, y: -2}")
    // A JSON key that is not a GraphQL name cannot name an input object field
    print pointType (Map [ "not a name", 1 ])
    |> equals ValueNone

[<Fact>]
let ``Scalar that fails to serialize a value must leave it without a literal`` () =
    let failingType : ScalarDefinition<string> =
        Define.Scalar ("Failing", (fun _ -> Error "Not used"), (fun _ -> raise (InvalidOperationException "Cannot serialize")))
    print failingType "value" |> equals ValueNone
    let refusingType : ScalarDefinition<string> =
        Define.Scalar ("Refusing", (fun _ -> Error "Not used"), (fun _ -> None))
    print refusingType "value" |> equals ValueNone
