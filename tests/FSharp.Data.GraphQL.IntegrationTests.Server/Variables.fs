module FSharp.Data.GraphQL.IntegrationTests.Server.Variables

open System
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open Newtonsoft.Json.Linq

let private makeOption (t : Type) = typedefof<_ option>.MakeGenericType(t)
let private makeArray (t : Type) = t.MakeArrayType()
let private makeArrayOption = makeArray >> makeOption

let read (schema : ISchema) (vardefs : VariableDefinition list) (variables : Map<string, obj>) =
    let scalarTypes =
        [| "Int", typeof<int>
           "Boolean", typeof<bool>
           "Date", typeof<DateTime>
           "Float", typeof<float>
           "ID", typeof<string>
           "String", typeof<string>
           "URI", typeof<Uri> |]
        |> Map.ofArray
    let schemaTypes =
        schema.TypeMap.ToSeq()
        |> Seq.choose (fun (name, def) -> match def with | :? InputDef as idef -> Some (name, idef.Type) | _ -> None)
        |> Map.ofSeq
    let unwrapOption (t : Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>
        then t.GetGenericArguments().[0]
        else failwithf "Expected type to be an Option type, but it is %s." t.Name
    let rec resolveVariableType (inputType : InputType) =
        match inputType with
        | NamedType tname ->
            match scalarTypes.TryFind tname with
            | Some t -> makeOption t
            | None ->
                match schemaTypes.TryFind tname with
                | Some t -> makeOption t
                | None -> failwithf "Could not determine variable type \"%s\"." tname
        | NonNullType t -> resolveVariableType t |> unwrapOption
        | ListType t -> resolveVariableType t |> makeArrayOption
    let resolveVariableValue (t : Type) (value : obj) =
        match value with
        | :? JToken as token -> token.ToObject(t, jsonSerializer)
        | _ -> value
    variables
    |> Seq.map (|KeyValue|)
    |> Seq.choose (fun (key, value) ->
        vardefs
        |> List.tryFind (fun def -> def.VariableName = key)
        |> Option.map (fun def -> key, resolveVariableValue (resolveVariableType def.Type) value))
    |> Map.ofSeq