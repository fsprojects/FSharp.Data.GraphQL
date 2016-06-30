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

    let launchQuery (serverUrl: string) (queryName: string) (cont: obj->'T) (query: string) =
        async {
            use client = new WebClient()
            let queryJson = Map["query", query] |> JsonConvert.SerializeObject
            let! json = client.UploadStringTaskAsync(Uri(serverUrl), queryJson) |> Async.AwaitTask
            let res = JToken.Parse json |> jsonToObject :?> IDictionary<string,obj>
            if res.ContainsKey("errors") then
                res.["errors"] :?> string[] |> String.concat "\n" |> failwith
            let data = (res.["data"] :?> IDictionary<string,obj>).[queryName]
            return cont(data)
        }

    let buildQuery (queryName: string) (queryFields: string)
                   (argNames: string[]) (argValues: obj[]) =
        Seq.zip argNames argValues
        |> Seq.map (fun (k,v) -> sprintf "%s: %s" k (JsonConvert.SerializeObject v))
        |> String.concat ", "
        |> sprintf "{ %s(%s) %s }" queryName <| queryFields

    let createStaticMethod (tdef: ProvidedTypeDefinition) (schemaTypes: Map<string,TypeReference>)
                           (serverUrl: string) (query: IntrospectionField) =
        let findType (t: IntrospectionTypeRef) =
            TypeReference.findType t schemaTypes
        let makeExprArray (exprs: Expr list) =
            Expr.NewArray(typeof<obj>, exprs |> List.map (fun e -> Expr.Coerce(e, typeof<obj>)))
        let resType = findType query.Type
        let asyncType = typedefof<Async<obj>>.MakeGenericType(resType)
        let args =
            query.Args
            |> Seq.map (fun x -> ProvidedParameter(x.Name, findType x.Type))
            |> Seq.toList
        let m = ProvidedMethod("Query" + firstToUpper query.Name, args, asyncType, IsStaticMethod=true)
        let sargs = [ProvidedStaticParameter("query", typeof<string>)]
        m.DefineStaticParameters(sargs, fun methName sargValues ->
            match sargValues with 
            | [| :? string as queryFields |] ->
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

    let getQueries (schema: IntrospectionSchema) =
        let queryType = schema.QueryType.Name |> getOrFail "No queryType.name"
        schema.Types
        |> Seq.collect (fun t ->
            if t.Name = queryType
            then defaultArg t.Fields [||]
            else [||])
        |> Seq.toList

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
                    schemaTypes
                    |> Seq.choose (fun kv ->
                        match kv.Value with
                        | ProvidedType(t,_) -> Some t
                        | NativeType _ -> None)
                    |> Seq.toList
                    |> tdef.AddMembers
                    // Static methods
                    Util.getQueries schema
                    |> List.map (Util.createStaticMethod tdef schemaTypes serverUrl)
                    |> tdef.AddMembers
                    tdef
                | Choice2Of2 ex -> String.concat "\n" ex |> failwithf "%s"
            | _ -> failwith "unexpected parameter values")
        this.AddNamespace(ns, [generator])

[<assembly:TypeProviderAssembly>]
do ()