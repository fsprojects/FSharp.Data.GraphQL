module FSharp.Data.GraphQL.Tests.ReflectionHelperTests

open System.Text.Json.Serialization
open Xunit

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Types

[<Fact>]
let ``Option type checks must recognize the generic types, but not arrays of them`` () =
    Assert.True (ReflectionHelper.isOptionType typeof<int option>, "int option is an option")
    Assert.False (ReflectionHelper.isOptionType typeof<int option[]>, "int option[] is not an option")
    Assert.True (ReflectionHelper.isValueOptionType typeof<int voption>, "int voption is a value option")
    Assert.False (ReflectionHelper.isValueOptionType typeof<int voption[]>, "int voption[] is not a value option")
    Assert.True (ReflectionHelper.isSkippableType typeof<int Skippable>, "int Skippable is skippable")
    Assert.False (ReflectionHelper.isSkippableType typeof<int Skippable[]>, "int Skippable[] is not skippable")
    Assert.True (ReflectionHelper.isListType typeof<int list>, "int list is a list")
    Assert.False (ReflectionHelper.isListType typeof<int[]>, "int[] is not a list")

[<Fact>]
let ``Option helpers must unwrap options`` () =
    Helpers.unwrap (Some 1) |> equals (box 1)
    Helpers.unwrap (ValueSome 1) |> equals (box 1)
    Helpers.unwrap (ValueNone : int voption) |> equals null
    Helpers.objectOptionCast (Some 1) |> equals (ValueSome (box 1))
    Helpers.objectOptionCast (ValueSome 1) |> equals (ValueSome (box 1))
    Helpers.toValueOption (Some 1) |> equals (ValueSome (box 1))

[<Fact>]
let ``Option helpers must take an array of options for a value`` () =
    let options = [| Some 1; None |]
    Helpers.unwrap options |> equals (box options)
    Helpers.objectOptionCast options |> equals ValueNone
    Helpers.toValueOption options |> equals (ValueSome (box options))

type InputScores = { Scores : int option[] }

[<Fact>]
let ``Input object with an array of options must be created`` () =
    let inputScoresType =
        Define.InputObject<InputScores> (
            "InputScores",
            [ Define.Input ("scores", (ListOf (Nullable IntType) : ListOfDef<int option, int option[]>)) ]
        )
    let root =
        Define.Object (
            "Query",
            [
                Define.Field (
                    "scores",
                    StringType,
                    "",
                    [ Define.Input ("input", inputScoresType) ],
                    fun ctx _ ->
                        let input = ctx.Arg<InputScores> "input"
                        $"%A{input.Scores}"
                )
            ]
        )
    let result =
        sync
        <| Executor(Schema (root)).AsyncExecute (parse "{ scores(input: { scores: [1, null] }) }", getMockInputContext)
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast NameValueLookup.ofList [ "scores", upcast "[|Some 1; None|]" ])

type NullableInputScores = { Scores : int option[] }

[<Fact>]
let ``Nullable list bound to an array of options must be rejected like a list of options`` () =
    let inputScoresType =
        Define.InputObject<NullableInputScores> (
            "NullableInputScores",
            [ Define.Input ("scores", Nullable (ListOf (Nullable IntType) : ListOfDef<int option, int option[]>)) ]
        )
    let root =
        Define.Object ("Query", [ Define.Field ("scores", StringType, "", [ Define.Input ("input", inputScoresType) ], fun _ _ -> "") ])
    let error = throws<InvalidInputTypeException> (fun () -> Executor (Schema (root)) |> ignore)
    Assert.Contains ("constructor parameters for optional GraphQL fields 'scores' are not optional", error.Message)