/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
namespace FSharp.Data.GraphQL

open System
open System.Reflection
open FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Parser

[<TypeProvider>]
type OuterProvider() as this = 
    inherit TypeProviderForNamespaces()
    let ns = "FSharp.Data.GraphQL"
    let asm = Assembly.GetExecutingAssembly()
    let createProperty name =
        ProvidedProperty
            (name, typeof<obj>, IsStatic = false, GetterCode = (fun args -> <@@ (%%args.[0] : obj) @@>))
    let createConstructor fields = 
        ProvidedConstructor([], InvokeCode = (fun args -> <@@ args @@>))
    
    let fieldName = 
        function 
        | Field f -> Some f.Name
        | _ -> None
    
    let createQueryType name selectionSet = 
        let queryType = ProvidedTypeDefinition(name, Some typeof<obj>)
        let fields = 
            selectionSet
            |> List.choose fieldName
        fields
        |> List.map createProperty
        |> queryType.AddMembers
        let ctor = createConstructor fields
        queryType.AddMember ctor
        queryType
    
    let createTypeFromDefintion = 
        function 
        | OperationDefinition def -> 
            match def.OperationType with
            | Query when def.Name.IsSome -> Some(createQueryType def.Name.Value def.SelectionSet)
            | _ -> None
        | _ -> None
    
    let createTypes query = 
        let document = parse query
        document.Definitions
        |> List.map createTypeFromDefintion
        |> List.choose id
    
    let createType typeName (parameters : obj []) = 
        let query = parameters.[0] :?> string
        let containerType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, HideObjectMethods = true)
        let types = createTypes query
        containerType.AddMembers types
        containerType
    
    let provider = ProvidedTypeDefinition(asm, ns, "GraphQLProvider", Some typeof<obj>)
    let parameters = [ ProvidedStaticParameter("GraphQLString", typeof<string>) ]
    do 
        provider.DefineStaticParameters(parameters, createType)
        this.AddNamespace(ns, [ provider ])

[<assembly:TypeProviderAssembly>]
do ()
