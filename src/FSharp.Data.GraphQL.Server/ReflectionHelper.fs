/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Reflection
open FSharp.Data.GraphQL.Types
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Linq.Expressions
open Microsoft.FSharp.Quotations

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
        let (Patterns.Call(_, info, _)) = <@ List.ofSeq<_> Seq.empty @>
        info.GetGenericMethodDefinition()

    let arrayOfSeq = 
        let (Patterns.Call(_, info, _)) = <@ Array.ofSeq<_> Seq.empty @>
        info.GetGenericMethodDefinition()

    let setOfSeq = 
        let (Patterns.Call(_, info, _)) = <@ Set.ofSeq<_> Seq.empty @>
        info.GetGenericMethodDefinition()
        
    let private em = typeof<Enumerable>.GetMethods()
    let private qm = typeof<Queryable>.GetMethods()

    let enumerableMethods = { new Methods with    
        member __.Type = typedefof<IEnumerable<_>>
        member __.Select = 
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Select"))
            methods.First()
        member __.Where = 
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Where"))
            methods.First()        
        member __.Skip = 
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Skip"))
            methods.First()        
        member __.Take = 
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Take"))
            methods.First()        
        member __.OrderBy = 
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderBy"))
            methods.First()                
        member __.OrderByDesc =  
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderByDescending"))
            methods.First()
        }

    let queryableMethods = { new Methods with    
        member __.Type = typedefof<IQueryable<_>>
        member __.Select = 
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Select"))
            methods.First()
        member __.Where = 
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Where"))
            methods.First()        
        member __.Skip = 
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Skip"))
            methods.First()        
        member __.Take = 
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Take"))
            methods.First()        
        member __.OrderBy = 
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderBy"))
            methods.First()                
        member __.OrderByDesc =  
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderByDescending"))
            methods.First()
        }

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

module Tracking =

    /// Data structure, that is used to tracking property getters in `collectGetters` function.
    type Tracker = 
        | Direct of string * Set<Tracker>
        | Collection of string * Set<Tracker>
        static member name =
            function
            | Direct(n, _)     -> n
            | Collection(n, _) -> n
        static member children =
            function
            | Direct(_, t)     -> t
            | Collection(_, t) -> t
    
    let addChild child =
        function
        | Direct(n, t)     -> Direct(n, Set.add child t)
        | Collection(n, t) -> Collection(n, Set.add child t)

    [<CustomComparison; CustomEquality>]
    type private Track = 
        { Name: string
          From: Type
          To: Type }
        override x.ToString() = sprintf "(%s: %s -> %s)" x.Name x.From.Name x.To.Name
        interface IEquatable<Track> with
            member x.Equals y = (x.Name = y.Name && x.From.FullName = y.From.FullName && x.To.FullName = y.To.FullName)
        interface IComparable with
            member x.CompareTo y = 
                match y with
                | :? Track as t -> 
                    let c = compare x.Name t.Name
                    if c = 0
                    then 
                        let c = compare x.From.FullName t.From.FullName
                        if c = 0 then compare x.To.FullName t.To.FullName else c
                    else c                        
                | _ -> invalidArg "y" "Track cannot be compared to other type"

    let private mkTrack name src dst = { Name = name; From = src; To = dst }

    let private (|IsDirect|IsCollection|) (tRoot: Type) = 
        if tRoot.IsGenericType
        then 
            if typeof<IEnumerable>.IsAssignableFrom tRoot
            then IsCollection
            else IsDirect
        else IsDirect

    let private canJoin (tRoot: Type) (tFrom: Type) =
        if tFrom = tRoot then true
        elif tRoot.IsGenericType && (typeof<IEnumerable>.IsAssignableFrom tRoot || typedefof<Option<_>>.IsAssignableFrom tRoot)
        then tFrom = (tRoot.GetGenericArguments().[0])
        else false

    let rec private foldTracks trackSet (tRoot: Type) =
        let members = trackSet |> Set.filter (fun track -> canJoin tRoot track.From)
        let remaining = Set.difference trackSet members
        members
        |> Set.map(fun track ->
            let children = foldTracks remaining track.To
            match track.To with
            | IsDirect -> Direct(track.Name, children)
            | IsCollection -> Collection(track.Name, children))
             
    /// Takes function with 2 parameters and applies them in reversed order           
    let inline private flip fn a b = fn b a

    /// Traverses a provided F# quotation in order to catch all 
    let tracker (root: Var) (expr: Expr) : Tracker  = 
        let rec track set e =
            match e with
            | Patterns.PropertyGet(Some subject, propertyInfo, _) -> Set.add (mkTrack propertyInfo.Name propertyInfo.DeclaringType propertyInfo.PropertyType) (track set subject)
            | Patterns.Lambda(_, body) -> track set body
            | Patterns.FieldGet(Some subject, fieldInfo) -> Set.add (mkTrack fieldInfo.Name fieldInfo.DeclaringType fieldInfo.FieldType) (track set subject)
            | Patterns.Var(_) -> set
            | Patterns.Application(var, body) -> (flip track var >> flip track body) set
            | Patterns.Call(subject, _, args) -> args |> List.fold track (match subject with Some x -> track set x | None -> set)
            | Patterns.Coerce(expr, _) -> track set expr
            | Patterns.ForIntegerRangeLoop(_, lower, upper, iter) -> (flip track lower >> flip track upper >> flip track iter) set
            | Patterns.IfThenElse(condition, ifTrue, ifFalse) -> (flip track condition >> flip track ifTrue >> flip track ifFalse) set          
            | Patterns.Let(_, expr, body) -> (flip track expr >> flip track body) set
            | Patterns.LetRecursive(bindings, body) -> bindings |> List.fold (fun acc (_, e) -> track acc e) (track set body)
            | Patterns.NewArray(_, exprs) -> exprs |> List.fold track set
            | Patterns.NewDelegate(_, _, body) -> track set body
            | Patterns.NewObject(_, args) -> args |> List.fold track set
            | Patterns.NewRecord(_, args) -> args |> List.fold track set
            | Patterns.NewTuple(args) -> args |> List.fold track set
            | Patterns.NewUnionCase(_, args) -> args |> List.fold track set
            | Patterns.QuoteRaw(expr) -> track set expr
            | Patterns.QuoteTyped(expr) -> track set expr
            | Patterns.Sequential(prev, next) -> (flip track prev >> flip track next) set
            | Patterns.TryFinally(tryBlock, finalBlock) -> (flip track tryBlock >> flip track finalBlock) set
            | Patterns.TryWith(tryBlock, _, filter, _, handler) -> (flip track tryBlock >> flip track filter >> flip track handler) set 
            | Patterns.TupleGet(expr, _) -> track set expr
            | Patterns.TypeTest(expr, _) -> track set expr
            | Patterns.UnionCaseTest(expr, _) -> track set expr
            | Patterns.VarSet(_, expr) -> track set expr
            | Patterns.WhileLoop(condition, body) -> (flip track condition >> flip track body) set
            | Patterns.WithValue(_, _, expr) -> track set expr
            //TODO: move all unnecessary calls into else `_` case
            | Patterns.AddressOf(_) -> set
            | Patterns.AddressSet(_, _) -> set
            | Patterns.DefaultValue(_) -> set
            | Patterns.FieldSet(_, _, _) -> set
            | Patterns.PropertySet(_, _, _, _) -> set
            | Patterns.Value(_, _) -> set
            | Patterns.ValueWithName(_, _, _) -> set
            | _ -> set
            
        let init = Set.empty
        let tracks = track init expr
        let shaked = foldTracks tracks root.Type
        Direct(root.Name, shaked)