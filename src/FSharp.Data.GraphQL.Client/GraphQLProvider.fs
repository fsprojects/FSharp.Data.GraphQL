namespace FSharp.Data.GraphQL.Client

open System
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types.Introspection

[<TypeProvider>]
type GraphQLTypeProvider(config) as this =
    inherit TypeProviderForNamespaces(config)

    let ns = "FSharp.Data.GraphQL"
    let asm = Assembly.GetExecutingAssembly()

    let context = 
        let tdef = ProvidedTypeDefinition(asm, ns, "GraphQLContext", Some typeof<GraphQLContextBase>)
        let mdef =
            let sprm = [ProvidedStaticParameter("query", typeof<string>)]
            let smdef = ProvidedMethod("Query", [], typeof<Async<string>>)
            let genfn (mname : string) (args : obj []) =
                let query = args.[0] :?> string
                let prm = 
                    [ ProvidedParameter("headers", typeof<(string * string) seq>, optionalValue = None)
                      ProvidedParameter("variables", typeof<(string * obj) seq>, optionalValue = None) ]
                let invoker (args : Expr list) =
                    <@@ let this = %%args.[0] : GraphQLContextBase
                        let headers = Option.ofObj (%%args.[1] : (string * string) seq)
                        let variables = Option.ofObj (%%args.[2] : (string * obj) seq)
                        GraphQLServer.makeRequest this.ServerUrl headers query variables @@>
                let mdef = ProvidedMethod(mname, prm, typeof<Async<string>>, invoker)
                tdef.AddMember(mdef); mdef
            smdef.DefineStaticParameters(sprm, genfn); smdef
        let members : MemberInfo list = [mdef]
        tdef.AddMembers(members); tdef

    let provider = 
        let generateStatic typeName (schema : Expr) =
            let tdef = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)
            let mdef = 
                let prm = [ProvidedParameter("serverUrl", typeof<string>)]
                let invoker (args : Expr list) =
                    let serverUrl = args.[0]
                    let ctor = typeof<GraphQLContextBase>.GetConstructors().[0]
                    Expr.NewObject(ctor, [serverUrl; schema])
                ProvidedMethod("GetContext", prm, context, invoker, true)
            let pdef = ProvidedProperty("Schema", typeof<IntrospectionSchema>, (fun _ -> schema), isStatic = true)
            let members : MemberInfo list = [mdef; pdef]
            tdef.AddMembers(members); tdef
        let generator = ProvidedTypeDefinition(asm, ns, "GraphQLProvider", Some typeof<obj>)
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