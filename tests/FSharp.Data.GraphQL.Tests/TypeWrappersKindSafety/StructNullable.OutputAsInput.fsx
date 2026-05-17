#load "References.fsx"

open FSharp.Data.GraphQL.Types

type InputOnly = { Value: int }
type OutputOnly = { Value: int }

let outputOnlyType =
    Define.Object<OutputOnly>(
        name = "OutputOnlyType",
        fields = [ Define.Field("value", IntType, fun _ x -> x.Value) ]
    )

// This should fail: OutputDef cannot be assigned to InputDef
let _ : InputDef<OutputOnly voption> = StructNullable outputOnlyType
