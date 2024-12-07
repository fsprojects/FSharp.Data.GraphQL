// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Reflection
open FSharp.Reflection
open System.Collections.Generic
open System.Linq
open FSharp.Data.GraphQL.Types
open FSharp.Quotations

type internal Methods =
    interface
        abstract member Type: Type
        abstract member Select : MethodInfo
        abstract member Where : MethodInfo
        abstract member Skip : MethodInfo
        abstract member Take : MethodInfo
        abstract member OrderBy : MethodInfo
        abstract member OrderByDesc : MethodInfo
    end

module internal Gen =

    let (|List|_|) (t: Type) =
        let typeParam = t.GetGenericArguments().[0]
        let tList = typedefof<_ list>.MakeGenericType [| typeParam |]
        if t = tList then Some typeParam
        else None

    let (|Array|_|) (t: Type) =
        if t.IsArray then Some (t.GetGenericArguments().[0])
        else None

    let (|Set|_|) (t: Type) =
        let typeParam = t.GetGenericArguments().[0]
        let tArray = typedefof<Set<_>>.MakeGenericType [| typeParam |]
        if t = tArray then Some typeParam
        else None

    let inline defaultArgOnNull a b = if isNull a |> not then a else b
    let private optionType = typedefof<option<_>>
    let (|Option|_|) (t: Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = optionType
        then Some (t.GetGenericArguments().[0])
        else None

    let (|Enumerable|_|) t =
        if typeof<System.Collections.IEnumerable>.IsAssignableFrom t
        then
            let e = defaultArgOnNull (t.GetInterface("IEnumerable`1")) t
            Some (e.GetGenericArguments().[0])
        else None

    let (|Queryable|_|) t =
        if typeof<IQueryable>.IsAssignableFrom t
        then Some (Queryable (t.GetInterface("IQueryable`1").GetGenericArguments().[0]))
        else None

    let genericType<'t> typeParams = typedefof<'t>.MakeGenericType typeParams

    let genericMethod<'t> methodName typeParams =
        let methods = typeof<'t>.GetMethods().Where(System.Func<MethodInfo,bool>(fun x -> x.Name = methodName))
        methods.First().MakeGenericMethod typeParams

    let private getCallInfo = function
        | Patterns.Call(_, info, _) -> info
        | _ -> failwith "Unexpected Quotation!"

    let listOfSeq =
        let info = getCallInfo <@ List.ofSeq<_> Seq.empty @>
        info.GetGenericMethodDefinition()

    let arrayOfSeq =
        let info = getCallInfo <@ Array.ofSeq<_> Seq.empty @>
        info.GetGenericMethodDefinition()

    let setOfSeq =
        let info = getCallInfo <@ Set.ofSeq<_> Seq.empty @>
        info.GetGenericMethodDefinition()

    let private em = typeof<Enumerable>.GetMethods()
    let private qm = typeof<Queryable>.GetMethods()

    let enumerableMethods = { new Methods with
        member _.Type = typedefof<IEnumerable<_>>
        member _.Select =
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Select"))
            methods.First()
        member _.Where =
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Where"))
            methods.First()
        member _.Skip =
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Skip"))
            methods.First()
        member _.Take =
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Take"))
            methods.First()
        member _.OrderBy =
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderBy"))
            methods.First()
        member _.OrderByDesc =
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderByDescending"))
            methods.First()
        }

    let queryableMethods = { new Methods with
        member _.Type = typedefof<IQueryable<_>>
        member _.Select =
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Select"))
            methods.First()
        member _.Where =
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Where"))
            methods.First()
        member _.Skip =
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Skip"))
            methods.First()
        member _.Take =
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Take"))
            methods.First()
        member _.OrderBy =
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderBy"))
            methods.First()
        member _.OrderByDesc =
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderByDescending"))
            methods.First()
        }

module internal ReflectionHelper =

    let [<Literal>] OptionTypeName = "Microsoft.FSharp.Core.FSharpOption`1"
    let [<Literal>] ValueOptionTypeName = "Microsoft.FSharp.Core.FSharpValueOption`1"
    let [<Literal>] ListTypeName = "Microsoft.FSharp.Collections.FSharpList`1"
    let [<Literal>] ArrayTypeName = "System.Array`1"
    let [<Literal>] IEnumerableTypeName = "System.Collections.IEnumerable"
    let [<Literal>] IEnumerableGenericTypeName = "System.Collections.Generic.IEnumerable`1"

    let isParameterOptional (p: ParameterInfo) =
        p.IsOptional
        || p.ParameterType.FullName.StartsWith OptionTypeName
        || p.ParameterType.FullName.StartsWith ValueOptionTypeName

    let isPrameterMandatory = not << isParameterOptional

    let unwrapOptions (ty : Type) =
        if ty.FullName.StartsWith OptionTypeName || ty.FullName.StartsWith ValueOptionTypeName then
            ty.GetGenericArguments().[0]
        else ty

    let rec isAssignableWithUnwrap (from: Type) (``to``: Type) =

        let checkCollections (from: Type) (``to``: Type) =
            if
                // TODO: Implement support of other types of collections using collection initializers
                (``to``.FullName.StartsWith ListTypeName || ``to``.FullName.StartsWith ArrayTypeName)
                && (from.IsGenericType
                    && from.GenericTypeArguments[0].IsAssignableTo(``to``.GenericTypeArguments[0])
                    && from.GetInterfaces()
                       |> Array.exists (
                        fun i -> i.FullName.StartsWith IEnumerableGenericTypeName
                                    || i.FullName = IEnumerableTypeName
                       )
                    )

            then
                let fromType = from.GetGenericArguments()[0]
                let toType = ``to``.GetGenericArguments()[0]
                fromType.IsAssignableTo toType
            else
                false

        let actualFrom =
            if from.FullName.StartsWith OptionTypeName || from.FullName.StartsWith ValueOptionTypeName then
                from.GetGenericArguments()[0]
            else from
        let actualTo =
            if ``to``.FullName.StartsWith OptionTypeName ||
               ``to``.FullName.StartsWith ValueOptionTypeName
            then
                ``to``.GetGenericArguments()[0]
            else ``to``

        let result = actualFrom.IsAssignableTo actualTo || checkCollections actualFrom actualTo
        if result then true
        elif actualFrom <> from || actualTo <> ``to`` then isAssignableWithUnwrap actualFrom actualTo
        else false

    let matchConstructor (t: Type) (fields: string []) =
        if FSharpType.IsRecord(t, true) then FSharpValue.PreComputeRecordConstructorInfo(t, true)
        else
            let constructors = t.GetConstructors(BindingFlags.NonPublic|||BindingFlags.Public|||BindingFlags.Instance)
            let inputFieldNames =
                fields
                |> Set.ofArray

            let constructorsWithParameters =
                constructors
                |> Seq.map (fun ctor -> struct(ctor, ctor.GetParameters()))
                // start from most complete constructors
                |> Seq.sortBy (fun struct(_, parameters) -> -parameters.Length)

            let getMandatoryParammeters = Seq.where isPrameterMandatory

            let struct(ctor, _) =
                seq {
                    // match all constructors with all parameters
                    yield! constructorsWithParameters |> Seq.map (fun struct(ctor, parameters) -> struct(ctor, parameters |> Seq.map (fun p -> p.Name)))
                    // match all constructors with non optional parameters
                    yield! constructorsWithParameters |> Seq.map (fun struct(ctor, parameters) -> struct(ctor, parameters |> getMandatoryParammeters |> Seq.map (fun p -> p.Name)))
                }
                // try match field with params by name
                // at last, default constructor should be used if defined
                |> Seq.find (fun struct(_, ctorParamsNames) -> Set.isSubset (Set.ofSeq ctorParamsNames) inputFieldNames)
            ctor

    let parseUnion (t: Type) (u: string) =
        if t.IsEnum then Enum.Parse(t, u, ignoreCase = true)
        else
            try
                match FSharpType.GetUnionCases(t, (BindingFlags.NonPublic ||| BindingFlags.Public))|> Array.filter(fun case -> case.Name.ToLower() = u.ToLower()) with
                | [|case|] -> FSharpValue.MakeUnion(case, [||], (BindingFlags.NonPublic ||| BindingFlags.Public))
                | _ -> null
            with _ -> null

