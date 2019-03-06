/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open FSharp.Data.GraphQL.Types.Introspection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Quotations
open FSharp.Data.GraphQL.Client
open System.Reflection

type GraphQLContextBase (serverUrl : string, schema : IntrospectionSchema) =
    member __.ServerUrl = serverUrl
    member __.Schema = schema
    static member MakeProvidedType(asm, ns) =
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
                    <@@ let request =
                            { ServerUrl = (%%args.[0] : GraphQLContextBase).ServerUrl
                              CustomHeaders = Option.ofObj %%args.[1]
                              OperationName = None
                              Query = query
                              Variables = Option.ofObj %%args.[2] }
                        GraphQLClient.sendRequest request @@>
                let mdef = ProvidedMethod(mname, prm, typeof<Async<string>>, invoker)
                tdef.AddMember(mdef); mdef
            smdef.DefineStaticParameters(sprm, genfn); smdef
        let members : MemberInfo list = [mdef]
        tdef.AddMembers(members)
        tdef
    static member internal Constructor =
        typeof<GraphQLContextBase>.GetConstructors().[0]

type EnumBase (name : string, value : string) =
    member __.Name = name
    member __.Value = value
    static member MakeProvidedType(asm, ns, name, items : string seq) =
        let bdef = typeof<EnumBase>
        let tdef = ProvidedTypeDefinition(asm, ns, name, Some bdef, nonNullable = true, isSealed = true)
        for item in items do
            let getterCode (_ : Expr list) =
                Expr.NewObject(EnumBase.Constructor, [ <@@ name @@>; <@@ item @@> ])
            let idef = ProvidedProperty(item, tdef, getterCode, isStatic = true)
            tdef.AddMember(idef)
        tdef
    static member internal Constructor =
        typeof<EnumBase>.GetConstructors().[0]
    override x.ToString() = x.Value