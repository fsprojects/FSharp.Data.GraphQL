module FSharp.Data.GraphQL.Server.Ast

open System.Collections.Immutable
open FSharp.Data.GraphQL

/// A list of fields that are reserved for the GraphQL introspection system.
[<CompiledName "MetaTypeFields">]
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

/// Find an operation by its name.
[<CompiledName "TryFindOperationByName">]
let tryFindOperationByName operationName (astDoc: Ast.Document)  =
    match astDoc.Definitions |> List.choose getOperation, operationName with
    | [ def ], _ -> Some def
    | defs, name -> defs |> List.tryFind (fun def -> def.Name = name)

/// Determines if the operation contains fields beyond the allowed fields.
/// And executes actions based on the result.
[<CompiledName "ContainsFieldsBeyond">]
let internal containsFieldsBeyond
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
