/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Validation

open FSharp.Data.GraphQL.Types

type ValidationResult =
    | Success
    | Error of string list

let (@) res1 res2 =
    match res1, res2 with
    | Success, Success -> Success
    | Success, _ -> res2
    | _, Success -> res1
    | Error e1, Error e2 -> Error (e1 @ e2)

let validateImplements (objdef: ObjectDef) (idef: InterfaceDef) =
    let objectFields =
        objdef.Fields
    let errors =
        idef.Fields
        |> Array.fold (fun acc f ->
            match Map.tryFind f.Name objectFields with
            | None -> (sprintf "'%s' field is defined by interface %s, but not implemented in object %s" f.Name idef.Name objdef.Name)::acc
            | Some objf when objf = f -> acc
            | Some _ -> (sprintf "'%s.%s' field signature does not match it's definition in interface %s" objdef.Name f.Name idef.Name)::acc) []
    match errors with
    | [] -> Success
    | err -> Error err
            
let validateType (namedTypes: Map<string, NamedDef>) typedef =
    match typedef with
    | Scalar scalardef -> Success
    | Object objdef -> 
        let nonEmptyResult = if objdef.Fields.Count > 0 then Success else Error [ objdef.Name + " must have at least one field defined" ]
        let implementsResult =
            objdef.Implements
            |> Array.fold (fun acc i -> acc @ validateImplements objdef i) Success
        nonEmptyResult @ implementsResult
    | InputObject indef -> 
        let nonEmptyResult = if indef.Fields.Length > 0 then Success else Error [ indef.Name + " must have at least one field defined" ]
        nonEmptyResult
    | Union uniondef ->
        let nonEmptyResult = if uniondef.Options.Length > 0 then Success else Error [ uniondef.Name + " must have at least one type definition option" ]
        nonEmptyResult
    | Enum enumdef -> 
        let nonEmptyResult = if enumdef.Options.Length > 0 then Success else Error [ enumdef.Name + " must have at least one enum value defined" ]
        nonEmptyResult
    | Interface idef -> 
        let nonEmptyResult = if idef.Fields.Length > 0 then Success else Error [ idef.Name + " must have at least one field defined" ]
        nonEmptyResult

let validate (namedTypes: Map<string, NamedDef>) : ValidationResult =
    namedTypes
    |> Map.toArray
    |> Array.map snd
    |> Array.fold (fun acc namedDef -> acc @ validateType namedTypes namedDef) Success