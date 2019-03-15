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

    let provider = 
        let generator = ProvidedTypeDefinition(asm, ns, "GraphQLProvider", None)
        let prm = 
            [ ProvidedStaticParameter("serverUrl", typeof<string>)
              ProvidedStaticParameter("customHeaders", typeof<seq<string * string>>, Seq.empty<string * string>) ]
        generator.DefineStaticParameters(prm, fun tname args ->
            let serverUrl = args.[0] :?> string
            let customHeaders = args.[1] :?> seq<string * string>
            let introspectionRequest =
                { ServerUrl = serverUrl
                  CustomHeaders = Some customHeaders
                  OperationName = None
                  Query = Introspection.introspectionQuery
                  Variables = None }
            let introspectionJson = GraphQLClient.sendRequest introspectionRequest
            let tdef = ProvidedTypeDefinition(asm, ns, tname, None)
            let schemaVal = Serialization.deserializeSchema introspectionJson
            let schemaExpr = <@@ Serialization.deserializeSchema introspectionJson @@>
            let ctxdef = GraphQLContextBase.MakeProvidedType(asm, ns, schemaVal, serverUrl, customHeaders)
            let ctxmdef =
                let prm = [ProvidedParameter("serverUrl", typeof<string>, optionalValue = serverUrl)]
                let invoker (args : Expr list) =
                    let serverUrl = args.[0]
                    Expr.NewObject(GraphQLContextBase.Constructor, [serverUrl; schemaExpr])
                ProvidedMethod("GetContext", prm, ctxdef, invoker, true)
            let schemapdef = ProvidedProperty("Schema", typeof<IntrospectionSchema>, (fun _ -> schemaExpr), isStatic = true)
            let members : MemberInfo list = [ctxdef; ctxmdef; schemapdef]
            tdef.AddMembers(members)
            tdef)
        generator

    let types = [provider]

    do this.AddNamespace(ns, types)

[<assembly:TypeProviderAssembly>] 
do()