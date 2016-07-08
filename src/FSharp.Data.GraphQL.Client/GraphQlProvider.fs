/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System
open System.IO
open System.Net
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices

open FSharp.Data.GraphQL.Types.Introspection
open TypeCompiler
open System.Collections.Generic
open Newtonsoft.Json.Linq
open Newtonsoft.Json
open Microsoft.FSharp.Quotations
open QuotationHelpers

module Util =
    open System.Text.RegularExpressions
    open FSharp.Data.GraphQL
    open QuotationHelpers

    let getOrFail (err: string) = function
        | Some v -> v
        | None -> failwith err

    let tryOrFail (err: string) (f: unit->'T) =
        try f()
        with ex -> Exception(err, ex) |> raise

    let firstToUpper (str: string) =
        if str <> null && str.Length > 0
        then str.[0].ToString().ToUpper() + str.Substring(1)
        else str

    let requestSchema (url: string) =
        async {
            let requestUrl = Uri(Uri(url), ("/?query=" + FSharp.Data.GraphQL.Introspection.introspectionQuery))
            let req = WebRequest.CreateHttp(requestUrl)
            req.Method <- "GET"
            use! resp = req.GetResponseAsync() |> Async.AwaitTask
            use stream = resp.GetResponseStream()
            use reader = new StreamReader(stream)
            let! json = reader.ReadToEndAsync() |> Async.AwaitTask
            let result = Serialization.fromJson json
            match result.Errors with
            | None ->
                let introspectionSchema = result.Data.__schema
                return Choice1Of2 introspectionSchema
            | Some errors ->
                return Choice2Of2 errors
        }

    let compileTypesFromSchema asm ns (schema: IntrospectionSchema) = 
        let underlyingType (t: TypeReference) =
            t.UnderlyingType
        let ctx = {
            Assembly = asm
            Namespace = ns
            KnownTypes = ProviderSessionContext.CoreTypes }
        let typeDefinitions =
            (ctx.KnownTypes, schema.Types)
            ||> Array.fold (fun acc t ->
                if acc.ContainsKey t.Name
                then acc
                else Map.add t.Name (ProvidedType (initType ctx t, t)) acc) 
        let defctx = { ctx with KnownTypes = typeDefinitions }
        typeDefinitions
        |> Seq.iter (fun kv ->
            match kv.Value with
            | NativeType t -> ()
            | ProvidedType (t, itype) ->
                genType defctx itype t)
        typeDefinitions

    let createMethod (tdef: ProvidedTypeDefinition) (schemaTypes: Map<string,TypeReference>)
                           (serverUrl: string) (query: IntrospectionField) =
        let findType (t: IntrospectionTypeRef) =
            TypeReference.findType t schemaTypes
        let resType = findType query.Type
        let asyncType = typedefof<Async<obj>>.MakeGenericType(resType)
        let args =
            query.Args
            |> Seq.map (fun x -> ProvidedParameter(x.Name, findType x.Type))
            |> Seq.toList
        let m = ProvidedMethod(firstToUpper query.Name, args, asyncType, IsStaticMethod=true)
        let sargs = [ProvidedStaticParameter("query", typeof<string>)]
        m.DefineStaticParameters(sargs, fun methName sargValues ->
            match sargValues with 
            | [| :? string as queryFields |] ->
                // This will fail if the query is not well formed
                do Parser.parse queryFields |> ignore
                let queryName = query.Name
                let argNames = args |> Seq.map (fun x -> x.Name) |> Seq.toArray
                let m2 = ProvidedMethod(methName, args, asyncType, IsStaticMethod = true) 
                m2.InvokeCode <-
                    if resType.Name = "FSharpOption`1" then
                        fun argValues ->
                        <@@
                            (%%makeExprArray argValues: obj[])
                            |> buildQuery queryName queryFields argNames
                            |> launchQuery serverUrl queryName Option.ofObj
                        @@>
                    else
                        fun argValues ->
                        <@@
                            (%%makeExprArray argValues: obj[])
                            |> buildQuery queryName queryFields argNames
                            |> launchQuery serverUrl queryName id
                        @@>
                tdef.AddMember m2
                m2
            | _ -> failwith "unexpected parameter values")
        m.InvokeCode <- fun _ -> <@@ null @@> // Dummy code
        m

    let createMethods (tdef: ProvidedTypeDefinition) (serverUrl: string)
                      (schema: IntrospectionSchema) (schemaTypes: Map<string,TypeReference>)
                      (opType: IntrospectionTypeRef option) (wrapperName: string) =
        match opType with
        | Some op when op.Name.IsSome ->
            let opName = op.Name.Value
            let wrapper = ProvidedTypeDefinition(wrapperName, Some typeof<obj>)
            schema.Types
            |> Seq.collect (fun t ->
                if t.Name = opName then defaultArg t.Fields [||] else [||])
            |> Seq.map (createMethod wrapper schemaTypes serverUrl)
            |> Seq.toList
            |> function
            | [] -> ()
            | ops ->
                wrapper.AddMembers ops
                tdef.AddMember wrapper
        | _ -> ()

type internal ProviderSchemaConfig =
    { Namespace: string 
      DefinedTypes: Map<string, ProvidedTypeDefinition option> }

[<TypeProvider>]
type GraphQlProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()
    
    let asm = System.Reflection.Assembly.GetExecutingAssembly()

    do
        let ns = "FSharp.Data.GraphQL"
        let generator = ProvidedTypeDefinition(asm, ns, "GraphQLProvider", Some typeof<obj>)
        generator.DefineStaticParameters([ProvidedStaticParameter("url", typeof<string>)], fun typeName parameterValues ->
            match parameterValues with 
            | [| :? string as serverUrl|] ->
                let choice = Util.requestSchema(serverUrl) |> Async.RunSynchronously
                match choice with
                | Choice1Of2 schema ->
                    let tdef = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)
                    let schemaTypes =
                        Util.compileTypesFromSchema asm "GraphQLTypes" schema
                    // Inner types
                    let typesWrapper = ProvidedTypeDefinition("Types", Some typeof<obj>)
                    schemaTypes
                    |> Seq.choose (fun kv ->
                        match kv.Value with
                        | ProvidedType(t,_) -> Some t
                        | NativeType _ -> None)
                    |> Seq.toList
                    |> typesWrapper.AddMembers
                    tdef.AddMember typesWrapper
                    // Static methods
                    Util.createMethods tdef serverUrl schema schemaTypes (Some schema.QueryType) "Queries"
                    Util.createMethods tdef serverUrl schema schemaTypes schema.MutationType "Mutations"
                    Util.createMethods tdef serverUrl schema schemaTypes schema.SubscriptionType "Subscriptions"
                    // Generic query method
                    let m = ProvidedMethod("Query", [ProvidedParameter("query", typeof<string>)], typeof<Async<obj>>)
                    m.IsStaticMethod <- true
                    m.InvokeCode <- fun argValues ->
                        <@@ launchQuery serverUrl null id (%%argValues.[0]: string) @@>
                    tdef.AddMember m
                    tdef
                | Choice2Of2 ex -> String.concat "\n" ex |> failwithf "%s"
            | _ -> failwith "unexpected parameter values")
        this.AddNamespace(ns, [generator])

[<assembly:TypeProviderAssembly>]
do ()