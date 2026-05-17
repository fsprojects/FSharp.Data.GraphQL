#load "References.fsx"

open FSharp.Data.GraphQL.Types

type InputOnly = { Value: int }
type OutputOnly = { Value: int }

let inputOnlyType =
    Define.InputObject<InputOnly>(
        name = "InputOnlyType",
        fields = [ Define.Input("value", IntType) ]
    )

// This should fail: InputDef cannot be assigned to OutputDef
let _ : OutputDef<InputOnly option> = Nullable inputOnlyType
