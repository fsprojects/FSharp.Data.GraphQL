namespace FSharp.Data.GraphQL.Client

open System
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types.Introspection

[<TypeProvider>]
type GraphQLTypeProvider (config) as this =
    inherit TypeProviderForNamespaces(config)

    let ns = "FSharp.Data.GraphQL"
    let asm = Assembly.GetExecutingAssembly()

    let context = 
        let tdef = GraphQLContextBase.MakeProvidedType(asm, ns)
        tdef

    let provider = 
        let generator = ProvidedTypeDefinition(asm, ns, "GraphQLProvider", None)
        let prm = [ProvidedStaticParameter("introspectionFile", typeof<string>)]
        generator.DefineStaticParameters(prm, fun tname args ->
            let introspectionFile = args.[0] :?> string
            let introspectionJson = IO.readTextFile config.ResolutionFolder introspectionFile
            let schema = <@@ Serialization.deserializeSchema introspectionJson @@>
            let tdef = ProvidedTypeDefinition(asm, ns, tname, None)
            let ctxmdef =
                let prm = [ProvidedParameter("serverUrl", typeof<string>)]
                let invoker (args : Expr list) =
                    let serverUrl = args.[0]
                    Expr.NewObject(GraphQLContextBase.Constructor, [serverUrl; schema])
                ProvidedMethod("GetContext", prm, context, invoker, true)
            let schemapdef = ProvidedProperty("Schema", typeof<IntrospectionSchema>, (fun _ -> schema), isStatic = true)
            let members : MemberInfo list = [ctxmdef; schemapdef]
            tdef.AddMembers(members)
            tdef)
        generator

    do this.AddNamespace(ns, [context; provider])

[<assembly:TypeProviderAssembly>] 
do()