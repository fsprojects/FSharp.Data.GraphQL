namespace FSharp.Data.GraphQL.Samples.GiraffeServer

open Newtonsoft.Json
open System.Reflection
open Microsoft.FSharp.Reflection

type OptionConverter() =
    inherit JsonConverter()
    
    override __.CanConvert(t) = 
        t.GetTypeInfo().IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

    override __.WriteJson(writer, value, serializer) =
        let getFields value =
            let _, fields = FSharpValue.GetUnionFields(value, value.GetType())
            fields.[0]
        let value = 
            match value with
            | null ->null
            | _ -> getFields value
        serializer.Serialize(writer, value)

    override __.ReadJson(_, _, _, _) = failwith "Operation is not supported."
