// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.SchemaTests

open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

[<Fact>]
let ``Object type should be able to merge fields with matching signatures from different interfaces`` () =
    let MovableType = Define.Interface ("Movable", [ Define.Field ("speed", IntType) ])
    let Movable2Type =
        Define.Interface ("Movable2", [ Define.Field ("speed", IntType); Define.Field ("acceleration", IntType) ])
    let PersonType =
        Define.Object (
            name = "Person",
            interfaces = [ MovableType; Movable2Type ],
            fields = [
                Define.Field ("name", StringType)
                Define.Field ("speed", IntType)
                Define.Field ("acceleration", IntType)
            ]
        )
    equals [ MovableType :> InterfaceDef; upcast Movable2Type ] (PersonType.Implements |> Array.toList)
    let expected =
        //NOTE: under normal conditions field order shouldn't matter in object definitions
        [
            Define.Field ("acceleration", IntType) :> FieldDef
            upcast Define.Field ("name", StringType)
            upcast Define.Field ("speed", IntType)
        ]
    equals
        expected
        ((PersonType :> ObjectDef).Fields
         |> Map.toList
         |> List.map snd)

[<Fact>]
let ``Schema config should be able to override default error handling`` () =
    let mutable idx = 0
    let conf = {
        SchemaConfig.Default with
            ParseError =
                (fun path ex ->
                    let i = idx
                    idx <- idx + 1
                    [ { new IGQLError with
                            member __.Message = i.ToString ()
                            member __.Exception = None } ])
    }
    let TestType =
        Define.Object<obj> (
            "TestType",
            [
                Define.Field ("passing", StringType, (fun _ _ -> "ok"))
                Define.Field ("failing1", Nullable StringType, (fun _ _ -> failwith "not ok"))
                Define.Field ("failing2", Nullable StringType, (fun _ _ -> failwith "not ok"))
            ]
        )
    let schema =
        Schema (Define.Object ("Query", [ Define.Field ("test", TestType, (fun _ () -> obj ())) ]), config = conf)
    let query =
        """
    {
        test {
            failing1
            passing
            failing2
        }
    }
    """
    let result = sync <| Executor(schema).AsyncExecute query
    let expected =
        NameValueLookup.ofList [ "test", box <| NameValueLookup.ofList [ "failing1", null; "passing", box "ok"; "failing2", null ] ]
    let expectedErrors = [
        GQLProblemDetails.CreateWithKind ("0", Execution, [ box "test"; "failing1" ])
        GQLProblemDetails.CreateWithKind ("1", Execution, [ box "test"; "failing2" ])
    ]
    ensureDirect result
    <| fun data errors ->
        data |> equals (upcast expected)
        errors |> equals expectedErrors
