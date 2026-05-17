module FSharp.Data.GraphQL.Tests.TypeWrappersKindSafetyTests

open FSharp.Data.GraphQL.Types
open Xunit

type private InputOnly = { Value : int }
type private OutputOnly = { Value : int }

let private InputOnlyType =
    Define.InputObject<InputOnly>(
        name = "InputOnlyType",
        fields = [ Define.Input("value", IntType) ]
    )

let private OutputOnlyType =
    Define.Object<OutputOnly>(
        name = "OutputOnlyType",
        fields = [ Define.Field("value", IntType, fun _ x -> x.Value) ]
    )

[<Fact>]
let ``ListOf keeps input-output direction`` () =
    let inputList : InputDef<InputOnly list> = ListOf InputOnlyType
    let outputList : OutputDef<OutputOnly list> = ListOf OutputOnlyType
    Assert.Equal ("[InputOnlyType!]!", inputList.ToString ())
    Assert.Equal ("[OutputOnlyType!]!", outputList.ToString ())

[<Fact>]
let ``Nullable keeps input-output direction`` () =
    let nullableInput : InputDef<InputOnly option> = Nullable InputOnlyType
    let nullableOutput : OutputDef<OutputOnly option> = Nullable OutputOnlyType
    Assert.Equal ("InputOnlyType", nullableInput.ToString ())
    Assert.Equal ("OutputOnlyType", nullableOutput.ToString ())

[<Fact>]
let ``StructNullable keeps input-output direction`` () =
    let nullableInput : InputDef<InputOnly voption> = StructNullable InputOnlyType
    let nullableOutput : OutputDef<OutputOnly voption> = StructNullable OutputOnlyType
    Assert.Equal ("InputOnlyType", nullableInput.ToString ())
    Assert.Equal ("OutputOnlyType", nullableOutput.ToString ())
