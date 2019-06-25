/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.TypeValidationTests

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Validation
open FSharp.Data.GraphQL.Validation.Types
open FSharp.Data.GraphQL.Types
open Xunit

type ITestInterface = 
    interface
        abstract TestProperty : string
        abstract TestMethod : int -> string -> string
    end

type TestDataType = 
    { TestProperty : string }
    interface ITestInterface with
        member x.TestProperty = x.TestProperty
        member x.TestMethod i y = x.TestProperty + y + i.ToString()

let TestInterface = 
    Define.Interface<ITestInterface>("TestInterface", 
                                     [ Define.Field("property", String)
                                       Define.Field("method", String, "Test method", 
                                                    [ Define.Input("x", Int)
                                                      Define.Input("y", String) ], (fun _ _ -> "")) ])

[<Fact>]
let ``Validation should inform about not implemented fields``() = 
    let TestData = 
        Define.Object<TestDataType>
            (name = "TestData", fields = [ Define.Field("property", String, (fun _ d -> d.TestProperty)) ], 
             interfaces = [ TestInterface ])
    let expected = 
        ValidationError [ "'method' field is defined by interface TestInterface, but not implemented in object TestData" ]
    let result = validateImplements TestData TestInterface
    equals expected result

[<Fact>]
let ``Validation should inform about fields with not matching signatures``() = 
    let TestData = 
        Define.Object<TestDataType>
            (name = "TestData", 
             fields = [ Define.Field("property", Int, (fun _ d -> 1))
                        Define.Field("method", String, "Test method", [ Define.Input("x", Int) ], (fun _ _ -> "res")) ], 
             interfaces = [ TestInterface ])
    
    let expected = 
        ValidationError 
            [ "'TestData.method' field signature does not match it's definition in interface TestInterface"; 
              "'TestData.property' field signature does not match it's definition in interface TestInterface" ]
    let result = validateImplements TestData TestInterface
    equals expected result
    
[<Fact>]
let ``Validation should succeed if object implements interface correctly``() = 
    let TestData = 
        Define.Object<TestDataType>
            (name = "TestData", 
             fields = [ Define.Field("property", String, (fun _ d -> d.TestProperty))
                        Define.Field("method", String, "Test method", [ Define.Input("x", Int); Define.Input("y", String) ], (fun _ _ -> "res")) ], 
             interfaces = [ TestInterface ])
    
    let result = validateImplements TestData TestInterface
    equals Success result
