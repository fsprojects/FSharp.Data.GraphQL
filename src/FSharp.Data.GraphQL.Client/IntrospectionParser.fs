/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

//TODO: intermediate representation of object returned from graphql endpoint using introspectionQuery

open System
open System.Reflection
open System.Collections.Generic
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Introspection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open QuotationHelpers

type GraphQLReply<'t> = {
    Data: 't
    Errors: string [] option
}

type IntrospectionResult = {
    __schema: IntrospectionSchema
}

module TypeCompiler =
    open System.Collections.Generic

    let private optionType = typedefof<option<obj>>

    type TypeReference =
        | NativeType of Type
        | ProvidedType of ProvidedTypeDefinition * IntrospectionType
        member x.UnderlyingType =
            match x with
            | NativeType t -> t
            | ProvidedType (t, _) -> upcast t
        static member findType (t: IntrospectionTypeRef) (schemaTypes: Map<string,TypeReference>) =
            let findType (t: IntrospectionTypeRef) =
                t.Name
                |> Option.bind (fun name -> Map.tryFind name schemaTypes)
                |> function Some t -> t.UnderlyingType | None -> typeof<obj>
            let rec receiveType nullable t: Type =
                match t.Kind, t.OfType with
                | TypeKind.NON_NULL, Some inner ->
                    receiveType false inner
                | TypeKind.LIST, Some inner -> 
                    let ofType = receiveType false inner
                    ofType.MakeArrayType()
                | other when nullable -> 
                    optionType.MakeGenericType [| findType t |]
                | other -> findType t
            receiveType true t

    type ProviderSessionContext = 
        { Assembly: Assembly
          Namespace: string
          KnownTypes: Map<string, TypeReference> }
        static member CoreTypes : Map<string, TypeReference> =
            Map.ofList [
                "String", NativeType typeof<string>
                "Int", NativeType typeof<int>
                "Boolean", NativeType typeof<bool>
                "Float", NativeType typeof<float>
                "ID", NativeType typeof<string>
                "__Schema", NativeType typeof<IntrospectionSchema>
                "__Type", NativeType typeof<IntrospectionTypeRef> ]

    let genProperty (ctx: ProviderSessionContext) docPrefix forceNullable ifield =
        let ptype =
            let ptype = TypeReference.findType ifield.Type ctx.KnownTypes
            if forceNullable && ptype.Name <> "FSharpOption`1"
            then optionType.MakeGenericType [| ptype |]
            else ptype
        // It's important to get the name of the field outside the quotation
        // in order to prevent errors at runtime
        let name = ifield.Name
        let getter = 
            match forceNullable, ifield.Type.Kind with
            | false, TypeKind.NON_NULL -> fun (args:Expr list) ->
                getDynamicField name args.[0]
            | _ -> fun args ->
                getDynamicField name args.[0]
                |> makeOption ptype
        let p = ProvidedProperty(ifield.Name, ptype, getterCode=getter)
        p.AddXmlDoc(docPrefix + (defaultArg ifield.Description ""))
      
        p

//    let genParam (ctx: ProviderSessionContext) (t: IntrospectionType) (iarg: IntrospectionInputVal) =
//        let ptype = TypeReference.findType iarg.Type ctx.KnownTypes
//        let param = ProvidedParameter(iarg.Name,ptype, false, iarg.DefaultValue)
//        param
//
//    let genMethod (ctx: ProviderSessionContext) (t: IntrospectionType) ifield =
//        let parameters = 
//            ifield.Args
//            |> Array.map (genParam ctx t)
//            |> Array.toList
//        let mtype = TypeReference.findType ifield.Type ctx.KnownTypes
//        let m = ProvidedMethod(ifield.Name, parameters,mtype)
//        m.IsStaticMethod <- true
//        if t.Description.IsSome then m.AddXmlDoc(t.Description.Value)
//        m

//    let genInputObject (ctx: ProviderSessionContext) (itype: IntrospectionType) (t: ProvidedTypeDefinition) =
//        if itype.Description.IsSome then t.AddXmlDoc(itype.Description.Value)
//        itype.Fields.Value
//        |> Array.map (fun field ->
//            if field.Args.Length = 0
//            then (genProperty ctx "" false field) :> MemberInfo
//            else upcast genMethod ctx itype field)
//        |> Array.toList
//        |> t.AddMembers

//    let genScalar (ctx: ProviderSessionContext) (itype: IntrospectionType) (t: ProvidedTypeDefinition) = 
//        if itype.Description.IsSome then t.AddXmlDoc(itype.Description.Value)

    let genEnum (ctx: ProviderSessionContext) (itype: IntrospectionType) (t: ProvidedTypeDefinition) = 
        let genEnumValue (enumVal: IntrospectionEnumVal) =
            // Name must be obtained outside the quotation
            let name = enumVal.Name
            let getter = fun _ -> <@@ name @@>
            let value = ProvidedProperty(name, t, getterCode=getter, isStatic=true)
            if enumVal.Description.IsSome then value.AddXmlDoc(enumVal.Description.Value)
            value
        if itype.Description.IsSome then
            t.AddXmlDoc("[ENUM] " + itype.Description.Value)
        itype.EnumValues.Value
        |> Seq.map genEnumValue
        |> Seq.toList
        |> t.AddMembers

    let genObject (ctx: ProviderSessionContext) (itype: IntrospectionType) (t: ProvidedTypeDefinition) = 
        if itype.Description.IsSome then t.AddXmlDoc(itype.Description.Value)
        itype.Fields.Value
        |> Seq.choose (fun field ->
            if field.Args.Length = 0
            then (genProperty ctx "" false field) :> MemberInfo |> Some
            else None //TODO: upcast genMethod ctx itype field
        )
        |> Seq.toList
        |> t.AddMembers

    let genUnion (ctx: ProviderSessionContext) (itype: IntrospectionType) (t: ProvidedTypeDefinition) =
        let (|MapTryFind|_|) (map: Map<'k,'v>) (key: 'k) =
            Map.tryFind key map
        let optionToSeq (opt: #seq<'T> option): 'T seq =
            Option.toList opt |> Seq.concat
        if itype.Description.IsSome then t.AddXmlDoc(itype.Description.Value)
        let unionTypes = optionToSeq itype.PossibleTypes |> Seq.toList
        (Map.empty, unionTypes)
        ||> Seq.fold (fun fields typ ->
            match typ.Name with
            | Some(MapTryFind ctx.KnownTypes (ProvidedType (_, itype))) ->
                (fields, optionToSeq itype.Fields)
                ||> Seq.fold (fun fields field ->
                    match Map.tryFind field.Name fields with
                    | Some (field, typs) ->
                        Map.add field.Name (field, typ.Name.Value::typs) fields
                    | None when field.Args.Length = 0 ->
                        Map.add field.Name (field, [typ.Name.Value]) fields
                    | None-> fields)
            | _ -> fields)
        |> Seq.map (fun kv ->
            match kv.Value with
            | (field, typs) ->
                // Add type owners of this property to comment
                let docPrefix = sprintf "[%s] " (FSharp.Core.String.concat "/" (List.rev typs))
                // If the property doesn't belong to all types, force it to be nullable
                let forceNullable = typs.Length < unionTypes.Length
                genProperty ctx docPrefix forceNullable field :> MemberInfo)
        |> Seq.toList
        |> t.AddMembers

    let genType (ctx: ProviderSessionContext) (itype: IntrospectionType) (t: ProvidedTypeDefinition) = 
        match itype.Kind with
        | TypeKind.OBJECT | TypeKind.INTERFACE -> genObject ctx itype t
        | TypeKind.UNION -> genUnion ctx itype t
        | TypeKind.ENUM -> genEnum ctx itype t
        | _ -> () // TODO: Complete other type kinds
//        | TypeKind.INPUT_OBJECT -> genInputObject ctx itype t
          // TODO: Parse scalars? They shouldn't reach this point
//        | TypeKind.SCALAR -> genScalar ctx itype t
//        | _ -> failwithf "Illegal type kind %s" (itype.Kind.ToString())

    let initType (ctx: ProviderSessionContext) (itype: IntrospectionType) = 
        match itype.Kind with
        | TypeKind.OBJECT ->
            let typeName = itype.Name
            let t = ProvidedTypeDefinition(typeName, Some typeof<obj>)
            let funType = typedefof<obj->obj>.MakeGenericType(t, typeof<Fields>)
            let invokeCode = 
                fun (args:Expr list) ->
                    <@@ InlineFragment(typeName, %%args.Head) @@>
            let m = ProvidedMethod("On", [ProvidedParameter("selection", funType)], typeof<obj>, invokeCode, true)
            t.AddMember m
            t
        | TypeKind.INPUT_OBJECT -> ProvidedTypeDefinition(itype.Name, Some typeof<obj>)
        | TypeKind.SCALAR -> ProvidedTypeDefinition(itype.Name, Some typeof<obj>)
        | TypeKind.UNION -> ProvidedTypeDefinition(itype.Name, Some typeof<obj>)
        | TypeKind.ENUM -> ProvidedTypeDefinition(itype.Name, Some typeof<obj>)
        | TypeKind.INTERFACE -> ProvidedTypeDefinition(itype.Name, Some typeof<obj>)
        | _ -> failwithf "Illegal type kind %s" (itype.Kind.ToString())