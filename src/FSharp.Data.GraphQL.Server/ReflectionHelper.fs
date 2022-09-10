/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

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

    let matchConstructor (t: Type) (fields: string []) =
        if FSharpType.IsRecord(t, true) then FSharpValue.PreComputeRecordConstructorInfo(t, true)
        else
            let constructors = t.GetConstructors(BindingFlags.NonPublic|||BindingFlags.Public|||BindingFlags.Instance)
            let fieldNames =
                fields
                |> Set.ofArray
            let (ctor, _) =
                constructors
                |> Array.map (fun ctor -> (ctor, ctor.GetParameters() |> Array.map (fun param -> param.Name)))
                // start from most complete constructors
                |> Array.sortBy (fun (_, paramNames) -> -paramNames.Length)
                // try match field with params by name
                // at last, default constructor should be used if defined
                |> Array.find (fun (_, paramNames) -> Set.isSubset (Set.ofArray paramNames) fieldNames)
            ctor

    let parseUnion (t: Type) (u: string) =
        if t.IsEnum then Enum.Parse(t, u, ignoreCase = true)
        else
            try
                match FSharpType.GetUnionCases(t, (BindingFlags.NonPublic ||| BindingFlags.Public))|> Array.filter(fun case -> case.Name.ToLower() = u.ToLower()) with
                | [|case|] -> FSharpValue.MakeUnion(case, [||], (BindingFlags.NonPublic ||| BindingFlags.Public))
                | _ -> null
            with _ -> null

