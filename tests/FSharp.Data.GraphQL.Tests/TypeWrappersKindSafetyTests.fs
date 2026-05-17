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
    let _ : InputDef<InputOnly list> = ListOf InputOnlyType
    let _ : OutputDef<OutputOnly list> = ListOf OutputOnlyType
    Assert.True true

[<Fact>]
let ``Nullable keeps input-output direction`` () =
    let _ : InputDef<InputOnly option> = Nullable InputOnlyType
    let _ : OutputDef<OutputOnly option> = Nullable OutputOnlyType
    Assert.True true

[<Fact>]
let ``StructNullable keeps input-output direction`` () =
    let _ : InputDef<InputOnly voption> = StructNullable InputOnlyType
    let _ : OutputDef<OutputOnly voption> = StructNullable OutputOnlyType
    Assert.True true
