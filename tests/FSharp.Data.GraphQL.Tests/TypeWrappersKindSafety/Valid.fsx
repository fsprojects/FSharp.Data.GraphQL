#load "References.fsx"

open FSharp.Data.GraphQL.Types

type InputOnly = { Value : int }
type OutputOnly = { Value : int }

let inputOnlyType =
    Define.InputObject<InputOnly>(
        name = "InputOnlyType",
        fields = [ Define.Input("value", IntType) ]
    )

let outputOnlyType =
    Define.Object<OutputOnly>(
        name = "OutputOnlyType",
        fields = [ Define.Field("value", IntType, fun _ x -> x.Value) ]
    )

// These are all valid assignments and must compile successfully
let _inputList    : InputDef<InputOnly list>    = ListOf inputOnlyType
let _outputList   : OutputDef<OutputOnly list>  = ListOf outputOnlyType
let _inputNullable  : InputDef<InputOnly option>  = Nullable inputOnlyType
let _outputNullable : OutputDef<OutputOnly option> = Nullable outputOnlyType
let _inputStruct  : InputDef<InputOnly voption>  = StructNullable inputOnlyType
let _outputStruct : OutputDef<OutputOnly voption> = StructNullable outputOnlyType
