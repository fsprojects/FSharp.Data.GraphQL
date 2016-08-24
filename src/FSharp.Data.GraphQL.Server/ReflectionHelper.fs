/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Reflection
open FSharp.Data.GraphQL.Types

module internal ReflectionHelper =

    let matchConstructor (t: Type) (fields: string []) =
        let constructors = t.GetConstructors()
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

open System.Collections
open System.Collections.Generic
open System.Linq
open System.Linq.Expressions
open Microsoft.FSharp.Quotations.Patterns

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

    let (|Enumerable|_|) t =
        if typeof<System.Collections.IEnumerable>.IsAssignableFrom t 
        then Some (Enumerable (t.GetInterface("IEnumerable`1").GetGenericArguments().[0]))
        else None

    let (|Queryable|_|) t =
        if typeof<IQueryable>.IsAssignableFrom t 
        then Some (Queryable (t.GetInterface("IQueryable`1").GetGenericArguments().[0]))
        else None

    let genericType<'t> typeParams = typedefof<'t>.MakeGenericType typeParams

    let genericMethod<'t> methodName typeParams = 
        let methods = typeof<'t>.GetMethods().Where(System.Func<MethodInfo,bool>(fun x -> x.Name = methodName))
        methods.First().MakeGenericMethod typeParams
    
    let listOfSeq = 
        let (Call(_, info, _)) = <@ List.ofSeq<_> Seq.empty @>
        info.GetGenericMethodDefinition()

    let arrayOfSeq = 
        let (Call(_, info, _)) = <@ Array.ofSeq<_> Seq.empty @>
        info.GetGenericMethodDefinition()

    let setOfSeq = 
        let (Call(_, info, _)) = <@ Set.ofSeq<_> Seq.empty @>
        info.GetGenericMethodDefinition()

    let private select tIn tOut =        
        match tIn with
        | Queryable tSource ->
            let tFunc = genericType<Func<_,_>> [| tSource; tOut |]
            let tExpr = genericType<Expression<_>> [| tFunc |]
            let tSelect = genericMethod<Queryable> "Select" [| tSource; tOut |] 
            tSelect
        | Enumerable tSource ->
            let tFunc = genericType<Func<_,_>> [| tSource; tOut |]
            let tSelect = genericMethod<Enumerable> "Select" [| tSource; tFunc |]
            tSelect
        | _ -> raise (InvalidOperationException <| sprintf "Type %O is not enumerable" tIn)

    let enumerableMethods = typeof<Enumerable>.GetMethods()
    let queryableMethods = typeof<Queryable>.GetMethods()

type internal EnumerableMethods =

    static member Type = typedefof<IEnumerable<_>>

    static member Select = 
        let methods = Gen.enumerableMethods.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Select"))
        methods.First()

    static member Where = 
        let methods = Gen.enumerableMethods.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Where"))
        methods.First()
        
    static member Skip = 
        let methods = Gen.enumerableMethods.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Skip"))
        methods.First()
        
    static member Take = 
        let methods = Gen.enumerableMethods.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Take"))
        methods.First()
        
    static member OrderBy = 
        let methods = Gen.enumerableMethods.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderBy"))
        methods.First()
                
    static member OrderByDesc =  
        let methods = Gen.enumerableMethods.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderByDescending"))
        methods.First()
        
type internal QueryableMethods =

    static member Type = typedefof<IQueryable<_>>

    static member Select = 
        let methods = Gen.queryableMethods.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Select"))
        methods.First()

    static member Where = 
        let methods = Gen.queryableMethods.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Where"))
        methods.First()
        
    static member Skip = 
        let methods = Gen.queryableMethods.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Skip"))
        methods.First()
        
    static member Take = 
        let methods = Gen.queryableMethods.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Take"))
        methods.First()
        
    static member OrderBy = 
        let methods = Gen.queryableMethods.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderBy"))
        methods.First()
                
    static member OrderByDesc =  
        let methods = Gen.queryableMethods.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderByDescending"))
        methods.First()