/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types.Introspection

type internal GraphQLResponse<'T> = {
    Data: 'T
    Errors: string [] option
}

type internal IntrospectionResult = {
    __schema: IntrospectionSchema
}


module internal TypeMapping =
    let scalar =
        [| "Int", typeof<int>
           "Boolean", typeof<bool>
           "Date", typeof<DateTime>
           "Float", typeof<float>
           "ID", typeof<string>
           "String", typeof<string>
           "URI", typeof<Uri>
           "Guid", typeof<Guid> |]
        |> Map.ofArray

    let getSchemaTypes (introspection : IntrospectionSchema) =
        let schemaTypeNames =
            [| "__TypeKind"
               "__DirectiveLocation"
               "__Type"
               "__InputValue"
               "__Field"
               "__EnumValue"
               "__Directive"
               "__Schema" |]

        let isScalarType (name : string) =
            scalar |> Map.containsKey name
        let isIntrospectionType (name : string) =
            schemaTypeNames |> Array.contains name
        introspection.Types
        |> Array.choose (fun t ->
            if not (isIntrospectionType t.Name) && not (isScalarType t.Name)
            then Some(t.Name, t)
            else None)
        |> Map.ofArray

    let mapScalarType uploadInputTypeName tname =
        match uploadInputTypeName with
        | Some uploadInputTypeName when uploadInputTypeName = tname -> typeof<Upload>
        | _ ->
            // Unknown scalar types will be mapped to a string type.
            if scalar.ContainsKey(tname)
            then scalar.[tname]
            else typeof<string>

    let makeOption (t : Type) = typedefof<_ option>.MakeGenericType(t)

    let makeArray (t : Type) = t.MakeArrayType()

    let unwrapOption (t : Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>
        then t.GetGenericArguments().[0]
        else failwithf "Expected type to be an Option type, but it is %s." t.Name

    let makeAsync (t : Type) = typedefof<Async<_>>.MakeGenericType(t)



type SchemaTypes (schema: IntrospectionSchema, ?uploadInputTypeName: string) =
    let schemaTypes = TypeMapping.getSchemaTypes schema
    let uploadInputType =
        uploadInputTypeName
        |> Option.map (fun name -> Map.tryFind name schemaTypes)
        |> Option.flatten
    do  match uploadInputType with
        | Some(schemaType) when schemaType.Kind = TypeKind.SCALAR -> ()
        | None _ -> ()
        | Some schemaType -> failwithf "Upload type '%s' must be a scalar." schemaType.Name

    member _.UploadInputType = uploadInputType

    member _.Types = schemaTypes

    member _.Introspection = schema

    member _.ContainsType (name: string) =
        schemaTypes.ContainsKey(name)

    member _.TryFindType(name: string) =
        schemaTypes.TryFind(name)

    member _.FindType(name: string) =
        match schemaTypes.TryFind(name) with
        | Some itype -> itype
        | None -> failwithf "Type \"%s\" was not found on the schema custom types." name

    member this.FindByTypeRef(tref: IntrospectionTypeRef) =
        match tref.Name with
        | Some name -> this.FindType name
        | None -> failwith "Expected schema type to have a name, but it does not have one."


[<AutoOpen>]
module internal Patterns =
    let (|NamedTypeRef|ContainerTypeRef|) (x: IntrospectionTypeRef) =
        match x.Name, x.OfType with
        | Some name, None -> NamedTypeRef(x.Kind, name)
        | None, Some innerType -> ContainerTypeRef(x.Kind, innerType)
        | _, _ -> failwithf "Invalid introspection type ref. Expected named or container type %A" x
