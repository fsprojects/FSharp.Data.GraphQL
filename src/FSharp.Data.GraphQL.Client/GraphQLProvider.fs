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

    let context = GraphQLContextBase.MakeProvidedType(asm, ns)

    let provider = 
        let generateStatic typeName (schema : Expr) =
            let tdef = ProvidedTypeDefinition(asm, ns, typeName, None)
            let mdef = 
                let prm = [ProvidedParameter("serverUrl", typeof<string>)]
                let invoker (args : Expr list) =
                    let serverUrl = args.[0]
                    Expr.NewObject(GraphQLContextBase.Constructor, [serverUrl; schema])
                ProvidedMethod("GetContext", prm, context, invoker, true)
            let pdef = ProvidedProperty("Schema", typeof<IntrospectionSchema>, (fun _ -> schema), isStatic = true)
            let members : MemberInfo list = [mdef; pdef]
            tdef.AddMembers(members); tdef
        let generator = ProvidedTypeDefinition(asm, ns, "GraphQLProvider", None)
        let prm = [ProvidedStaticParameter("introspectionFile", typeof<string>)]
        let genfn (tname : string) (args : obj []) =
            let introspectionFile = args.[0] :?> string
            let introspectionJson = IO.readTextFile config.ResolutionFolder introspectionFile
            let schema = <@@ Serialization.deserializeSchema introspectionJson @@>
            generateStatic tname schema
        generator.DefineStaticParameters(prm, genfn); generator

    do this.AddNamespace(ns, [context; provider])

[<assembly:TypeProviderAssembly>] 
do()