namespace FSharp.Data.GraphQL.RemoteSchema

open System    
open System.Reflection
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open Microsoft.FSharp.Quotations
open Newtonsoft.Json
open Newtonsoft.Json.Serialization

[<AutoOpen>]
module RemoteSchema =

    [<Literal>]
    let query = "query"
    
    [<Literal>]
    let mutation = "mutation"

    type IRemoteSchemaHandler =
        interface
            abstract Resolve : ResolveFieldContext -> obj -> AsyncVal<obj>
        end

    type FSharp.Data.GraphQL.Types.SchemaDefinitions.Define with
        static member RemoteSchemaField(name : string, remoteSchema: IRemoteSchemaHandler) : FieldDef<'Val> = 
            Define.CustomField(name, remoteSchema.Resolve)
