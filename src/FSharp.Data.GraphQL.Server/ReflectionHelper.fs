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

    /// Data structure, that is used to tracking property getters in `collectGetters` function.
    type Tracker = Tracker of MemberExpression * Tracker []

    /// Traverses a provided F# quotation in order to catch all 
    let collectGetters (e: Expr) : Expression list = 
        let rec collect tracked (e : Expr) : Expression list =
            match e with
            | Patterns.PropertyGet(Some subject, propertyInfo, _) -> 
                let exprs = collect tracked subject
                let head = exprs |> List.head
                let property = Expression.Property(head, propertyInfo)
                [ property ]
            | Patterns.Lambda(arg, body) -> collect tracked body
            | Patterns.FieldGet(Some subject, fieldInfo) -> 
                let exprs = collect tracked subject
                let head = exprs |> List.head
                let field = Expression.Field(head, fieldInfo)
                [ field ]
            | Patterns.Var(var) -> [ Expression.Variable(var.Type, var.Name) ]
            | Patterns.Application(body, arg) -> (collect tracked body) @ (collect tracked arg)
            | Patterns.Call(subject, _, args) -> (defaultArg (subject |> Option.map (collect tracked)) []) @ (List.collect (collect tracked) args)
            | Patterns.Coerce(expr, _) -> collect tracked expr
            | Patterns.ForIntegerRangeLoop(indexer, lower, upper, iter) -> (collect tracked lower) @ (collect tracked upper) @ (collect tracked iter)
            | Patterns.IfThenElse(condition, ifTrue, ifFalse) -> (collect tracked condition) @ (collect tracked ifTrue) @ (collect tracked ifFalse)
            | Patterns.Let(variable, expr, body) -> (collect tracked expr) @ (collect tracked body)
            | Patterns.LetRecursive(bindings, body) -> (collect tracked body)
            | Patterns.NewArray(_, exprs) -> exprs |> List.collect (collect tracked) 
            | Patterns.NewDelegate(_, _, body) -> collect tracked body
            | Patterns.NewObject(_, args) -> args |> List.collect (collect tracked)
            | Patterns.NewRecord(_, args) -> args |> List.collect (collect tracked)
            | Patterns.NewTuple(args) -> args |> List.collect (collect tracked)
            | Patterns.NewUnionCase(_, args) -> args |> List.collect (collect tracked)
            | Patterns.QuoteRaw(expr) -> collect tracked expr
            | Patterns.QuoteTyped(expr) -> collect tracked expr
            | Patterns.Sequential(prev, next) -> (collect tracked prev) @ (collect tracked next)
            | Patterns.TryFinally(tryBlock, finalBlock) -> (collect tracked tryBlock) @ (collect tracked finalBlock)
            | Patterns.TryWith(tryBlock, var1, filter, var2, handler) -> 
                (collect tracked tryBlock) @ (collect tracked filter) @ (collect tracked handler)
            | Patterns.TupleGet(expr, _) -> collect tracked expr
            | Patterns.TypeTest(expr, _) -> collect tracked expr
            | Patterns.UnionCaseTest(expr, _) -> collect tracked expr
            | Patterns.VarSet(_, expr) -> collect tracked expr
            | Patterns.WhileLoop(condition, body) -> (collect tracked condition) @ (collect tracked body)
            | Patterns.WithValue(_, _, expr) -> collect tracked expr
            //TODO: move all unnecessary calls into else `_` case
            | Patterns.AddressOf(_) -> []
            | Patterns.AddressSet(_, _) -> []
            | Patterns.DefaultValue(_) -> []
            | Patterns.FieldSet(_, _, _) -> []
            | Patterns.PropertySet(_, _, _, _) -> []
            | Patterns.Value(_, _) -> []
            | Patterns.ValueWithName(_, _, _) -> []
            | _ -> []
        match e with
        | Patterns.Lambda(root, expr) -> collect [ root ] expr
        | _ -> failwithf "Provided F# quotation must be Lambda"