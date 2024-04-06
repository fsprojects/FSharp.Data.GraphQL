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

    let isParameterOptional (p: ParameterInfo) =
        p.IsOptional
        || p.ParameterType.FullName.StartsWith OptionTypeName
        || p.ParameterType.FullName.StartsWith ValueOptionTypeName

    let isPrameterMandatory = not << isParameterOptional


(*
    let makeConstructor (fields: string[]) =
        { new ConstructorInfo() as this with
            let typ = typeof<Dynamic.DynamicObject>
            let ctor =
                typ.GetConstructors(BindingFlags.NonPublic|||BindingFlags.Public|||BindingFlags.Instance)
                |> Seq.filter (fun ctor -> ctor.GetParameters().Length = 0)
                |> Seq.head
            override _.GetParameters() = [||]
            override _.Attributes = ctor.Attributes
            override _.MethodHandle = ctor.MethodHandle
            override _.GetMethodImplementationFlags() = ctor.GetMethodImplementationFlags()
            member _.Invoke(args: obj []) = Activator.CreateInstance(typeof<obj>, args)
            member _.Name = "DynamicConstructorInfo"
            member _.DeclaringType = typeof<obj>
            override _.Invoke(bindingFlags, binder, args, cultureInfo) = obj()
        }
*)

    type DynamicConstructorInfo(typ:System.Type, fields: string[]) =
        inherit ConstructorInfo()
        let ctor =
            typ.GetConstructors(BindingFlags.NonPublic|||BindingFlags.Public|||BindingFlags.Instance)
            |> Seq.filter (fun ctor -> ctor.GetParameters().Length = 0)
            |> Seq.head
        override _.GetParameters() =
            printfn "DynamicConstructorInfo.GetParameters() -> %A" (fields |> String.concat ",")
            fields
            |> Array.mapi (fun i fName ->
                { new ParameterInfo() with
                    member _.Name = fName
                    member _.Position = i
                    member _.ParameterType = typeof<obj>
                    member _.Attributes = ParameterAttributes.Optional
                }
            )
        override _.Attributes = ctor.Attributes
        override _.MethodHandle = ctor.MethodHandle
        override _.GetMethodImplementationFlags() = ctor.GetMethodImplementationFlags()
        override _.GetCustomAttributes(_inherit) = typ.GetCustomAttributes(_inherit)
        override _.GetCustomAttributes(x, _inherit) = typ.GetCustomAttributes(x, _inherit)
        override _.IsDefined(x, _inherit) = typ.IsDefined(x, _inherit)
        override _.Name = "DynamicConstructorInfo"
        override _.DeclaringType = typ.DeclaringType
        override _.ReflectedType = typ.ReflectedType
        member this.Invoke(args: obj []) =
            printfn "DynamicConstructorInfo.Invoke(%A) nArgs=%d nFields=%d" args args.Length fields.Length
            let o = Activator.CreateInstance(typ)
            let dict = o :?> IDictionary<string, obj>
            (fields, args)
            ||> Seq.iter2 (fun k v -> dict.Add(k, v))
            o
        override this.Invoke(bindingFlags, binder, args, cultureInfo) = this.Invoke(args)
        override this.Invoke(o, bindingFlags, binder, args, cultureInfo) = this.Invoke(bindingFlags, binder, args, cultureInfo)

    let matchConstructor (t: Type) (fields: string []) =
        if FSharpType.IsRecord(t, true) then FSharpValue.PreComputeRecordConstructorInfo(t, true)
        //else if t = typeof<Dynamic.DynamicObject> then
        else if typeof<Collections.Generic.IDictionary<string,obj>>.IsAssignableFrom(t) then
            eprintfn "matched DynamicConstructorInfo for type %A" t
            upcast DynamicConstructorInfo(t, fields)
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

