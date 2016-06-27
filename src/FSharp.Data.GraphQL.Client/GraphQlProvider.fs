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
        |> Seq.choose (fun kv -> 
            match kv.Value with
            | NativeType _ -> None
            | ProvidedType (t, itype) ->
                genType defctx itype t
                Some t)
        |> Seq.toList

    let rec jsonToObject (token: JToken) =
        match token.Type with
        | JTokenType.Object ->
            token.Children<JProperty>()
            |> Seq.map (fun prop -> prop.Name, jsonToObject prop.Value)
            |> dict :> obj
        | JTokenType.Array ->
            token |> Seq.map jsonToObject |> Seq.toArray :> obj
        | _ ->
            (token :?> JValue).Value

    let launchQuery (serverUrl: string) (query: string) =
        async {
            use client = new WebClient()
            let queryJson = Map["query", query] |> JsonConvert.SerializeObject
            let! json = client.UploadStringTaskAsync(Uri(serverUrl), queryJson) |> Async.AwaitTask
            let res = JToken.Parse json |> jsonToObject :?> IDictionary<string,obj>
            if res.ContainsKey("errors") then
                res.["errors"] :?> string[] |> String.concat "\n" |> failwith
            // TODO: Find a more structured way to get the query name
            let queryName = Regex.Match(query, "^.*?(\w+)").Groups.[1].Value
            return (res.["data"] :?> IDictionary<string,obj>).[queryName]
        }

    let createStaticMethod (tdef: ProvidedTypeDefinition) (resType: Type) (serverUrl) =
        let asyncType = typeof<Async<obj>>.GetGenericTypeDefinition().MakeGenericType(resType)
        let m = ProvidedMethod("Query" + resType.Name, [], asyncType, IsStaticMethod=true)
        let sargs = [ProvidedStaticParameter("query", typeof<string>)]
        m.DefineStaticParameters(sargs, fun methName parameterValues ->
            match parameterValues with 
            | [| :? string as query |] ->
                // Make a first flight to be sure the query is accepted by the server
                launchQuery serverUrl query |> Async.Catch |> Async.RunSynchronously |> function
                | Choice1Of2 _ -> ()
                | Choice2Of2 ex -> failwith ex.Message
                let m2 = ProvidedMethod(methName, [], asyncType, IsStaticMethod = true) 
                m2.InvokeCode <- fun _ ->
                    <@@ launchQuery serverUrl query @@>
                tdef.AddMember m2
                m2
            | _ -> failwith "unexpected parameter values")
        m.InvokeCode <- fun _ -> <@@ null @@> // Dummy code
        m


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
                    let types = Util.compileTypesFromSchema asm "GraphQLTypes" schema
                    tdef.AddMembers(types)
                    for typ in types do
                        Util.createStaticMethod tdef typ serverUrl
                        |> tdef.AddMember
                    tdef
                | Choice2Of2 ex -> String.concat "\n" ex |> failwithf "%s"
            | _ -> failwith "unexpected parameter values")
        this.AddNamespace(ns, [generator])

[<assembly:TypeProviderAssembly>]
do ()