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

module QuotationHelpers =
    let getDynamicField (name: string) (expr: Expr) =
        let dicType = typeof<IDictionary<string,obj>>
        let mi = dicType.GetMethod("get_Item")
        Expr.Call(Expr.Coerce(expr, dicType), mi, [Expr.Value(name)])

    let makeOption (optType: Type) (expr: Expr) =
        let optArg = optType.GetGenericArguments().[0]
        let cases =
            FSharpType.GetUnionCases(optType)
            |> Seq.map (fun case -> case.Name, case)
            |> Map
        let var = Var("instance", typeof<obj>)
        Expr.Let(var, expr,
            Expr.IfThenElse(
                <@@ %%Expr.Var(var) = null @@>,
                Expr.NewUnionCase(cases.["None"], []),
                Expr.NewUnionCase(cases.["Some"], [Expr.Coerce(Expr.Var var, optArg)])))

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
        let p = ProvidedProperty(ifield.Name, ptype)
        p.AddXmlDoc(docPrefix + (defaultArg ifield.Description ""))
        // It's important to get the name of the field outside the quotation
        // in order to prevent errors at runtime
        let name = ifield.Name
        p.GetterCode <- 
            match forceNullable, ifield.Type.Kind with
            | false, TypeKind.NON_NULL -> fun args ->
                getDynamicField name args.[0]
            | _ -> fun args ->
                getDynamicField name args.[0]
                |> makeOption ptype
        p

    let genParam (ctx: ProviderSessionContext) (t: IntrospectionType) (iarg: IntrospectionInputVal) =
        let ptype = TypeReference.findType iarg.Type ctx.KnownTypes
        let param = ProvidedParameter(iarg.Name,ptype, false, iarg.DefaultValue)
        param

    let genMethod (ctx: ProviderSessionContext) (t: IntrospectionType) ifield =
        let parameters = 
            ifield.Args
            |> Array.map (genParam ctx t)
            |> Array.toList
        let mtype = TypeReference.findType ifield.Type ctx.KnownTypes
        let m = ProvidedMethod(ifield.Name, parameters,mtype)
        m.IsStaticMethod <- true
        if t.Description.IsSome then m.AddXmlDoc(t.Description.Value)
        m

    let genEnumValue (ctx: ProviderSessionContext) (t: IntrospectionType) (enumVal: IntrospectionEnumVal) =
        let value = ProvidedProperty(enumVal.Name, typeof<int>)
        if t.Description.IsSome then value.AddXmlDoc(t.Description.Value)
        value.IsStatic <- true
        value

    let genScalar (ctx: ProviderSessionContext) (itype: IntrospectionType) (t: ProvidedTypeDefinition) = 
        if itype.Description.IsSome then t.AddXmlDoc(itype.Description.Value)

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

    let genInputObject (ctx: ProviderSessionContext) (itype: IntrospectionType) (t: ProvidedTypeDefinition) =
        if itype.Description.IsSome then t.AddXmlDoc(itype.Description.Value)
        itype.Fields.Value
        |> Array.map (fun field ->
            if field.Args.Length = 0
            then (genProperty ctx "" false field) :> MemberInfo
            else upcast genMethod ctx itype field)
        |> Array.toList
        |> t.AddMembers

    let genEnum (ctx: ProviderSessionContext) (itype: IntrospectionType) (t: ProvidedTypeDefinition) = 
        if itype.Description.IsSome then t.AddXmlDoc(itype.Description.Value)
        itype.EnumValues.Value
        |> Array.map (genEnumValue ctx itype)
        |> Array.toList
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
                    | Some (prop, typs) ->
                        Map.add field.Name (field, typ.Name.Value::typs) fields
                    | None when field.Args.Length = 0 ->
                        Map.add field.Name (field, [typ.Name.Value]) fields
                    | None-> fields)
            | _ -> fields)
        |> Seq.map (fun kv ->
            match kv.Value with
            | (field, typs) ->
                // Add type owners of this property to comment
                let docPrefix = sprintf "(%s) " (FSharp.Core.String.concat "/" typs)
                // If the property doesn't belong to all types, force it to be nullable
                let forceNullable = typs.Length < unionTypes.Length
                genProperty ctx docPrefix forceNullable field :> MemberInfo)
        |> Seq.toList
        |> t.AddMembers

    let genType (ctx: ProviderSessionContext) (itype: IntrospectionType) (t: ProvidedTypeDefinition) = 
        match itype.Kind with
        | TypeKind.OBJECT | TypeKind.INTERFACE -> genObject ctx itype t
        | TypeKind.UNION -> genUnion ctx itype t
//        | TypeKind.ENUM -> genEnum ctx itype t
        | _ -> () // TODO: Complete other type kinds
//        | TypeKind.INPUT_OBJECT -> genInputObject ctx itype t
          // TODO: Parse scalars? They shouldn't reach this point
//        | TypeKind.SCALAR -> genScalar ctx itype t
//        | _ -> failwithf "Illegal type kind %s" (itype.Kind.ToString())
        
    let initType (ctx: ProviderSessionContext) (itype: IntrospectionType) = 
        match itype.Kind with
        | TypeKind.OBJECT -> ProvidedTypeDefinition(itype.Name, Some typeof<obj>)
        | TypeKind.INPUT_OBJECT -> ProvidedTypeDefinition(itype.Name, Some typeof<obj>)
        | TypeKind.SCALAR -> ProvidedTypeDefinition(itype.Name, Some typeof<obj>)
        | TypeKind.UNION -> ProvidedTypeDefinition(itype.Name, Some typeof<obj>)
        | TypeKind.ENUM -> ProvidedTypeDefinition(itype.Name, Some typeof<Enum>)
        | TypeKind.INTERFACE -> ProvidedTypeDefinition(itype.Name, Some typeof<obj>)
        | _ -> failwithf "Illegal type kind %s" (itype.Kind.ToString())