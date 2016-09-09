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

    type Scope = 
        { Parent: Scope option
          Children: List<Scope>
          Tracked: HashSet<Expr>
          Vars: Dictionary<Var, Expr> }

    let empty = { Parent = None; Tracked = null; Vars = null; Children = null }
    let mkChild parent tracked = 
        let child = { Parent = Some parent; Tracked = HashSet<_>(); Vars = Dictionary<_,_>(); Children = List<_>() }
        parent.Children.Add child 
        child.Tracked.Add tracked |> ignore
        child

    let rec getTrack var scope =
        if scope.Tracked.Contains var 
        then Some scope
        else match scope.Parent with
             | None -> None
             | Some parent -> getTrack var parent

    let rec getVar var scope =
        match scope.Vars.TryGetValue var with
        | true, e -> Some e
        | false, _ ->
            match scope.Parent with
            | None -> None
            | Some parent -> getVar var parent

    /// Data structure, that is used to tracking property getters in `collectGetters` function.
    type Tracker = 
        | Complex of string * Tracker list
        | Leaf of string

    /// Traverses a provided F# quotation in order to catch all 
    let track (e: Expr) : Tracker  = 
        let rec track scope e =
            match e with
            | Patterns.PropertyGet(Some subject, propertyInfo, _) ->
                match getTrack subject scope with
                | None -> ()
                | Some scope -> scope.Tracked.Add e |> ignore
            | Patterns.Lambda(arg, body) -> 
                //TODO: create child scope with a
                let nestedScope = mkChild scope (Expr.Var arg)
                track nestedScope body
            | Patterns.FieldGet(Some subject, fieldInfo) -> 
                match getTrack subject scope with
                | None -> ()
                | Some scope -> scope.Tracked.Add e |> ignore
            | Patterns.Var(var) ->
                if getTrack e scope |> Option.isSome then scope.Tracked.Add e |> ignore
            | Patterns.Application(_, _) ->
                apply scope Map.empty 0 e
            | Patterns.Call(subject, _, args) -> 
                match subject with
                | Some self -> track scope self
                | None -> ()
                args
                |> List.iter (trackAll scope)
            | Patterns.Coerce(expr, _) -> ()
            | Patterns.ForIntegerRangeLoop(indexer, lower, upper, iter) -> ()
            | Patterns.IfThenElse(condition, ifTrue, ifFalse) ->
                track (mkChild scope condition) condition
                track (mkChild scope ifTrue) ifTrue
                track (mkChild scope ifFalse) ifFalse                
            | Patterns.Let(variable, expr, body) -> ()
            | Patterns.LetRecursive(bindings, body) -> 
                for (var, e) in bindings do
                    scope.Vars.Add(var, e)
                track scope body
            | Patterns.NewArray(_, exprs) -> ()
            | Patterns.NewDelegate(_, _, body) -> ()
            | Patterns.NewObject(_, args) -> ()
            | Patterns.NewRecord(_, args) -> ()
            | Patterns.NewTuple(args) -> ()
            | Patterns.NewUnionCase(_, args) -> ()
            | Patterns.QuoteRaw(expr) -> ()
            | Patterns.QuoteTyped(expr) -> ()
            | Patterns.Sequential(prev, next) -> ()
            | Patterns.TryFinally(tryBlock, finalBlock) -> ()
            | Patterns.TryWith(tryBlock, var1, filter, var2, handler) -> ()
            | Patterns.TupleGet(expr, _) -> ()
            | Patterns.TypeTest(expr, _) -> ()
            | Patterns.UnionCaseTest(expr, _) -> ()
            | Patterns.VarSet(_, expr) -> ()
            | Patterns.WhileLoop(condition, body) -> ()
            | Patterns.WithValue(_, _, expr) -> ()
            //TODO: move all unnecessary calls into else `_` case
            | Patterns.AddressOf(_) -> ()
            | Patterns.AddressSet(_, _) -> ()
            | Patterns.DefaultValue(_) -> ()
            | Patterns.FieldSet(_, _, _) -> ()
            | Patterns.PropertySet(_, _, _, _) -> ()
            | Patterns.Value(_, _) -> ()
            | Patterns.ValueWithName(_, _, _) -> ()
            | _ -> ()

        and apply scope index n e = 
            match e with
            | Patterns.Application(body, param) ->
                let newIndex =
                    if getTrack param scope |> Option.isSome
                    then Map.add n param index
                    else index
                apply scope newIndex (n+1) body
            | Patterns.Var(var) ->
                match getVar var scope with
                | Some lambda ->
                    apply scope index (n-1) lambda
                | None -> failwithf "Couldn't find lambda identifier %A in scope" var
            | Patterns.Lambda(arg, body) ->
                let argExpr = Expr.Var arg
                let lambdaScope = if getTrack argExpr scope |> Option.isSome then  mkChild scope argExpr else scope
                if n = 0 
                then track lambdaScope body
                else apply lambdaScope index (n-1) body

        and trackAll scope e =
            ()
            

        match e with
        | Patterns.Lambda(root, expr) -> 
            let scope = { Parent = None; Tracked = HashSet<_>(); Vars = Dictionary<_,_>(); Children = List<_>() }
            scope.Tracked.Add (Expr.Var root)
            track scope expr
            scope
        | _ -> failwithf "Provided F# quotation must be Lambda"