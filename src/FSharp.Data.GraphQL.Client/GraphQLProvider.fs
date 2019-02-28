namespace FSharp.Data.GraphQL.Client

open System
open System.Reflection
open System.IO
open FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open System.Net
open FSharp.Data.GraphQL.Types.Introspection
open TypeCompiler
open QuotationHelpers
open FSharp.Data.GraphQL

[<TypeProvider>]
type GraphQLTypeProvider(config) as this =
    inherit TypeProviderForNamespaces(config)

    let ns = "FSharp.Data.GraphQL"
    let asm = Assembly.GetExecutingAssembly()

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
            | NativeType _ -> ()
            | ProvidedType (t, itype) -> genType defctx itype t)
        typeDefinitions

    let generate typeName schema =
        let tdef = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)
        let schemaTypes = compileTypesFromSchema asm "GraphQLTypes" schema
        let typesWrapper = ProvidedTypeDefinition("Types", Some typeof<obj>)
        schemaTypes
        |> Seq.choose (fun kv ->
            match kv.Value with
            | ProvidedType (t, _) -> Some t
            | NativeType _ -> None)
        |> Seq.toList
        |> typesWrapper.AddMembers
        tdef.AddMember typesWrapper; tdef

    let generator = 
        let gen = ProvidedTypeDefinition(asm, ns, "GraphQLProvider", Some typeof<obj>)
        let prm = [ProvidedStaticParameter("introspectionFile", typeof<string>)]
        let genfn (tname : string) (pvalues : obj []) =
            let introspectionFile = pvalues.[0] :?> string
            let introspectionJson = IO.readTextFile config.ResolutionFolder introspectionFile
            let schema = Serialization.deserializeSchema introspectionJson
            generate tname schema
        gen.DefineStaticParameters(prm, genfn); gen

    do this.AddNamespace(ns, [generator])

[<assembly:TypeProviderAssembly>] 
do()