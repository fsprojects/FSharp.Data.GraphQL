module FSharp.Data.GraphQL.Server.AspNetCore.Ast

open System.Collections.Immutable
open FSharp.Data.GraphQL

let metaTypeFields =
    seq {
        "__type"
        "__schema"
        "__typename"
    }
    |> ImmutableHashSet.CreateRange

let private getOperation astDef =
    match astDef with
    | Ast.OperationDefinition odef -> Some odef
    | _ -> None

let findOperationByName operationName (astDoc: Ast.Document)  =
    match astDoc.Definitions |> List.choose getOperation, operationName with
    | [ def ], _ -> Some def
    | defs, name -> defs |> List.tryFind (fun def -> def.Name = name)

let containsFieldsBeyond
    (allowedFields: ImmutableHashSet<_>)
    (whenContains: Ast.Field -> unit)
    (whenNotContains: unit -> unit)
    (astOpDef: Ast.OperationDefinition)
    =
    astOpDef.SelectionSet
    |> List.exists (fun def ->
        match def with
        | Ast.Field fd ->
            whenContains fd
            not <| allowedFields.Contains fd.Name
        | _ ->
            whenNotContains ()
            false)
