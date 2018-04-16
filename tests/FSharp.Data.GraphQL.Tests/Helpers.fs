/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
[<AutoOpen>]
module Helpers

open System
open System.Collections.Generic
open Xunit
open FSharp.Data.GraphQL.Execution

let equals (expected : 'x) (actual : 'x) = 
    Assert.True((actual = expected), sprintf "expected %+A\nbut got %+A" expected actual)
let notEquals (expected : 'x) (actual : 'x) = 
    Assert.True((actual <> expected), sprintf "unexpected %+A" expected)
let noErrors (result: IDictionary<string, obj>) =
    match result.TryGetValue("errors") with
    | true, errors -> failwithf "expected ExecutionResult to have no errors but got %+A" errors
    | false, _ -> ()
let single (xs : 'a seq) =
    Assert.Single(xs)
let empty (xs: 'a seq) =
    Assert.Empty(xs)
let fail (message: string) =
    Assert.True(false, message)
let throws<'e when 'e :> exn> (action : unit -> unit) = Assert.Throws<'e>(action)
let sync = Async.RunSynchronously
let is<'t> (o: obj) = o :? 't
let hasError errMsg (errors: Error seq) =
    let containsMessage = errors |> Seq.exists (fun (message, _) -> message.Contains(errMsg))
    Assert.True (containsMessage, sprintf "expected to contain message '%s', but no such message was found. Messages found: %A" errMsg errors)
let (<??) opt other = 
    match opt with
    | None -> Some other
    | _ -> opt
let undefined (value: 't) = 
    Assert.True((value = Unchecked.defaultof<'t>), sprintf "Expected value to be undefined, but was: %A" value)
let contains (expected : 'a) (xs : 'a seq) =
    Assert.Contains(expected, xs); xs

open System
open System.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Microsoft.FSharp.Reflection
open FSharp.Data.GraphQL.Types

type internal OptionConverter() =
    inherit JsonConverter()
    
    override x.CanConvert(t) = 
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>
    override x.WriteJson(writer, value, serializer) =
        let value = 
            if value = null then null
            else 
                let _,fields = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(value, value.GetType())
                fields.[0]  
        serializer.Serialize(writer, value)

    override x.ReadJson(reader, t, existingValue, serializer) = 
        let innerType = t.GetGenericArguments().[0]
        let innerType = 
            if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
            else innerType
        let value = serializer.Deserialize(reader, innerType)
        let cases = FSharpType.GetUnionCases(t)
        if value = null then FSharpValue.MakeUnion(cases.[0], [||])
        else FSharpValue.MakeUnion(cases.[1], [|value|])

let internal settings = JsonSerializerSettings()
settings.Converters <- [| OptionConverter() |]
settings.ContractResolver <- CamelCasePropertyNamesContractResolver()

/// Serializes provided object to JSON format.
let toJson (o: 't) : string = JsonConvert.SerializeObject(o, settings)

/// Deserializes provided JSON string to object of given type.
let fromJson<'t> (json: string) : 't = JsonConvert.DeserializeObject<'t>(json, settings)

