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
                res.["errors"] :?> obj[] |> Seq.map string |> String.concat "\n" |> failwith
            let data =
                // Options are problematic within quotations so we just use null here
                if queryName <> null
                then (res.["data"] :?> IDictionary<string,obj>).[queryName]
                else res.["data"]
            return cont(data)
        }

    let buildQuery (queryName: string) (queryFields: string)
                   (argNames: string[]) (argValues: obj[]) =
        let queryFields, queryFragments =
            let mutable i = 0
            let mutable openBraces = 0
            let mutable closeBraces = 0
            while closeBraces = 0 || closeBraces < openBraces do
                match queryFields.Chars(i) with
                | '{' -> openBraces <- openBraces + 1
                | '}' -> closeBraces <- closeBraces + 1
                | _ -> ()
                i <- i + 1
            queryFields.Substring(0, i), queryFields.Substring(i)
        Seq.zip argNames argValues
        |> Seq.map (fun (k,v) -> sprintf "%s: %s" k (JsonConvert.SerializeObject v))
        |> String.concat ", "
        |> fun args -> sprintf "{ %s(%s) %s }%s" queryName args queryFields queryFragments

    let createMethod (tdef: ProvidedTypeDefinition) (schemaTypes: Map<string,TypeReference>)
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
                        <@@ Util.launchQuery serverUrl null id (%%argValues.[0]: string) @@>
                    tdef.AddMember m
                    tdef
                | Choice2Of2 ex -> String.concat "\n" ex |> failwithf "%s"
            | _ -> failwith "unexpected parameter values")
        this.AddNamespace(ns, [generator])

[<assembly:TypeProviderAssembly>]
do ()