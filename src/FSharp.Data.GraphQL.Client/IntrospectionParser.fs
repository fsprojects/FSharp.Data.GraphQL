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
        member x.GetKnownType (t: IntrospectionTypeRef) =
            let findType (t: IntrospectionTypeRef) =
                match t.Name with
                | Some name -> Map.tryFind name x.KnownTypes
                | _ -> None
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

    let genProperty (ctx: ProviderSessionContext) ifield =
        let ptype = ctx.GetKnownType ifield.Type
        let p = ProvidedProperty(ifield.Name, ptype)
        if ifield.Description.IsSome then p.AddXmlDoc(ifield.Description.Value)
        // It's important to get the name of the field outside the quotation
        // in order to prevent errors at runtime
        let name = ifield.Name
        p.GetterCode <- 
            // TODO Union types. Also, Option must be of the specific property type, not obj 
            match ifield.Type.Kind with
            | TypeKind.NON_NULL -> fun args ->
                getDynamicField name args.[0]
            | TypeKind.LIST -> fun args ->
                let eltype = ptype.GetElementType()
                Expr.Coerce(getDynamicField name args.[0], eltype)
            | _ -> fun args ->
                getDynamicField name args.[0]
                |> makeOption ptype
        p

    let genParam (ctx: ProviderSessionContext) (t: IntrospectionType) (iarg: IntrospectionInputVal) =
        let param = ProvidedParameter(iarg.Name, ctx.GetKnownType iarg.Type, false, iarg.DefaultValue)
        param

    let genMethod (ctx: ProviderSessionContext) (t: IntrospectionType) ifield =
        let parameters = 
            ifield.Args
            |> Array.map (genParam ctx t)
            |> Array.toList
        let m = ProvidedMethod(ifield.Name, parameters, ctx.GetKnownType ifield.Type)
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
            then (genProperty ctx field) :> MemberInfo |> Some
            else None //TODO: upcast genMethod ctx itype field
        )
        |> Seq.toList
        |> t.AddMembers

    let genInterface (ctx: ProviderSessionContext) (itype: IntrospectionType) (t: ProvidedTypeDefinition) = 
        if itype.Description.IsSome then t.AddXmlDoc(itype.Description.Value)
        itype.Fields.Value
        |> Array.map (fun field ->
            if field.Args.Length = 0
            then (genProperty ctx field) :> MemberInfo
            else upcast genMethod ctx itype field)
        |> Array.toList
        |> t.AddMembers

    let genInputObject (ctx: ProviderSessionContext) (itype: IntrospectionType) (t: ProvidedTypeDefinition) =
        if itype.Description.IsSome then t.AddXmlDoc(itype.Description.Value)
        itype.Fields.Value
        |> Array.map (fun field ->
            if field.Args.Length = 0
            then (genProperty ctx field) :> MemberInfo
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
        if itype.Description.IsSome then t.AddXmlDoc(itype.Description.Value)
        (Map.empty, itype.PossibleTypes.Value)
        ||> Seq.fold (fun fields typ ->
            if typ.Name.IsSome && ctx.KnownTypes.ContainsKey(typ.Name.Value) then
                match ctx.KnownTypes.[typ.Name.Value] with
                | ProvidedType (_, itype) when itype.Fields.IsSome ->
                    (fields, itype.Fields.Value) ||> Seq.fold (fun fields field ->
                        if field.Args.Length = 0 && not(fields.ContainsKey(field.Name))
                        then Map.add field.Name ((genProperty ctx field) :> MemberInfo) fields
                        else fields)
                | _ -> fields
            else
                fields)
        |> Map.toList
        |> List.map snd
        |> t.AddMembers

    let genType (ctx: ProviderSessionContext) (itype: IntrospectionType) (t: ProvidedTypeDefinition) = 
        match itype.Kind with
        | TypeKind.OBJECT -> genObject ctx itype t
        | TypeKind.UNION -> genUnion ctx itype t
        | _ -> () // TODO: Complete other type kinds
//        | TypeKind.INPUT_OBJECT -> genInputObject ctx itype t
//        | TypeKind.SCALAR -> genScalar ctx itype t
//        | TypeKind.ENUM -> genEnum ctx itype t
//        | TypeKind.INTERFACE -> genInterface ctx itype t
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