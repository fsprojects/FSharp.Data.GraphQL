/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Linq

open System
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Linq.Expressions
open FSharp.Reflection
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open Microsoft.FSharp.Quotations

let rec private collect (e: Expr) =
    match e with
    | Patterns.PropertyGet(Some subject, propertyInfo, _) ->
        let exprs = collect subject
        let head = exprs |> List.head
        let property = Expression.Property(null, propertyInfo)
        [ property ]
    | Patterns.Lambda(arg, body) ->
        collect body
    | Patterns.FieldGet(Some subject, fieldInfo) -> 
        let exprs = collect subject
        let head = exprs |> List.head
        let field = Expression.Field(null, fieldInfo)
        [ field ]
    | Patterns.Application(body, arg) -> 
        (collect body) @ (collect arg)
    | Patterns.Call(subject, _, args) -> 
        (defaultArg (subject |> Option.map collect) []) @ (List.collect collect args)
    | Patterns.Coerce(expr, _) -> 
        collect expr
    | Patterns.ForIntegerRangeLoop(indexer, lower, upper, iter) -> 
        (collect lower) @ (collect upper) @ (collect iter)
    | Patterns.IfThenElse(condition, ifTrue, ifFalse) -> 
        (collect condition) @ (collect ifTrue) @ (collect ifFalse)
    | Patterns.Let(variable, expr, body) -> 
        (collect expr) @ (collect body)
    | Patterns.LetRecursive(bindings, body) -> 
        (collect body)
    | Patterns.NewArray(_, exprs) ->
        exprs |> List.collect collect
    | Patterns.NewDelegate(_, _, body) -> 
        collect body
    | Patterns.NewObject(_, args) ->
        args |> List.collect collect
    | Patterns.NewRecord(_, args) ->
        args |> List.collect collect
    | Patterns.NewTuple(args) -> 
        args |> List.collect collect
    | Patterns.NewUnionCase(_, args) ->
        args |> List.collect collect
    | Patterns.QuoteRaw(expr) -> 
        collect expr
    | Patterns.QuoteTyped(expr) -> 
        collect expr
    | Patterns.Sequential(prev, next) -> 
        (collect prev) @ (collect next)
    | Patterns.TryFinally(tryBlock, finalBlock) -> 
        (collect tryBlock) @ (collect finalBlock)
    | Patterns.TryWith(tryBlock, var1, filter, var2, handler) -> 
        (collect tryBlock) @ (collect filter) @ (collect handler)
    | Patterns.TupleGet(expr, _) -> 
        collect expr
    | Patterns.TypeTest(expr, _) -> 
        collect expr
    | Patterns.UnionCaseTest(expr, _) -> 
        collect expr
    | Patterns.VarSet(_, expr) -> 
        collect expr
    | Patterns.WhileLoop(condition, body) -> 
        (collect condition) @ (collect body)
    | Patterns.WithValue(_, _, expr) -> 
        collect expr
    | Patterns.AddressOf(_) -> []
    | Patterns.AddressSet(_, _) -> []
    | Patterns.DefaultValue(_) -> []
    | Patterns.FieldSet(_, _, _) -> []
    | Patterns.PropertySet(_, _, _, _) -> []
    | Patterns.Value(value, tValue) -> []
    | Patterns.ValueWithName(value, tValue, name) -> []
    | Patterns.Var(var) -> []
    | _ -> []


type private Arg = 
    | Id of obj
    | OrderBy of obj
    | OrderByDesc of obj
    | Skip of obj
    | Take of obj
    | First of obj
    | Last of obj
    | Before of obj
    | After of obj

let private unwrap (resolve: Resolve) inParam: Expression =
    match resolve.Expr with
    | Patterns.WithValue(_, _, Patterns.Lambda(_, Patterns.Lambda(_, Patterns.PropertyGet(Some(_), propInfo, _)))) ->
        upcast Expression.Property(inParam, propInfo)
    | other -> FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.QuotationToExpression other

let inline private argVal vars argDef = 
    function 
    | Some arg -> Execution.argumentValue vars argDef arg
    | None -> argDef.DefaultValue

let private resolveLinqArg vars (name, argdef, arg) =
    match name with
    | "id"          -> argVal vars argdef arg |> Option.map (Id)
    | "orderBy"     -> argVal vars argdef arg |> Option.map (OrderBy)
    | "orderByDesc" -> argVal vars argdef arg |> Option.map (OrderByDesc)
    | "skip"        -> argVal vars argdef arg |> Option.map (Skip)
    | "take"        -> argVal vars argdef arg |> Option.map (Take)
    | "first"       -> argVal vars argdef arg |> Option.map (First)
    | "last"        -> argVal vars argdef arg |> Option.map (Last)
    | "before"      -> argVal vars argdef arg |> Option.map (Before)
    | "after"       -> argVal vars argdef arg |> Option.map (After)
    | _ -> None

let private memberExpr info memberName parameter =
    let (SelectFields(fields)) = info.Kind
    let fieldInfo =
        fields
        |> List.find (fun f -> f.Definition.Name = memberName)
    unwrap fieldInfo.Definition.Resolve parameter

let private argumentToQueryable (methods: Methods) tSource allArguments info expression =
    function
    | Id value -> 
        let p0 = Expression.Parameter tSource
        let idProperty = memberExpr info "id" p0
        // Func<tSource, bool> predicate = p0 => p0 == value
        let predicate = Expression.Lambda(Expression.Equal(idProperty, Expression.Constant value), p0)
        let where = methods.Where.MakeGenericMethod [| tSource |]
        Expression.Call(null, where, expression, predicate)
    | OrderBy value ->
        let p0 = Expression.Parameter tSource
        // Func<tSource, tResult> memberAccess = p0 => p0.<value>
        let property = memberExpr info (string value) p0        
        let memberAccess = Expression.Lambda(property, [| p0 |])
        let orderBy = methods.OrderBy.MakeGenericMethod [| tSource; memberAccess.ReturnType |]
        Expression.Call(null, orderBy, expression, memberAccess)
    | OrderByDesc value -> 
        let p0 = Expression.Parameter tSource
        // Func<tSource, tResult> memberAccess = p0 => p0.<value>
        let property = memberExpr info (string value) p0        
        let memberAccess = Expression.Lambda(property, [| p0 |])
        let orderByDesc = methods.OrderByDesc.MakeGenericMethod [| tSource; memberAccess.ReturnType |]
        Expression.Call(null, orderByDesc, expression, memberAccess)
    | Skip value -> 
        let skip = methods.Skip.MakeGenericMethod [| tSource |]
        Expression.Call(null, skip, expression, Expression.Constant(value))
    | Take value -> 
        let take = methods.Take.MakeGenericMethod [| tSource |]
        Expression.Call(null, take, expression, Expression.Constant(value))
    | First value -> 
        let p0 = Expression.Parameter tSource   
        // 1. Find ID field of the structure (info object is needed)
        let idProperty = memberExpr info "id" p0
        // 2. apply q.OrderBy(p0 => p0.<ID_field>) on the expression
        let idAccess = Expression.Lambda(idProperty, [| p0 |])
        let orderBy = methods.OrderBy.MakeGenericMethod [| tSource; idAccess.ReturnType |]
        let ordered = Expression.Call(null, orderBy, expression, idAccess)

        let afterOption = allArguments |> Array.tryFind (function After _ -> true | _ -> false)
        let result =
            match afterOption with
            | Some(After id) -> 
                //TODO: 3a. parse id value using Relay GlobalId and retrieve "actual" id value
                // 4a. apply q.Where(p0 => p0.<ID_field> > id) on the ordered expression
                let predicate = Expression.Lambda(Expression.GreaterThan(p0, Expression.Constant id), p0)
                let where = methods.Where.MakeGenericMethod [| tSource |]
                Expression.Call(null, where, ordered, predicate)
            | None -> ordered
        // 5. apply result.Take(value)
        let take = methods.Take.MakeGenericMethod [| tSource |]
        Expression.Call(null, take, result, Expression.Constant value)
    | Last value -> 
        let p0 = Expression.Parameter tSource   
        // 1. Find ID field of the structure (info object is needed)
        let idProperty = memberExpr info "id" p0
        // 2. apply q.OrderBy(p0 => p0.<ID_field>) on the expression
        let idAccess = Expression.Lambda(idProperty, [| p0 |])
        let orderByDesc = methods.OrderByDesc.MakeGenericMethod [| tSource; idAccess.ReturnType |]
        let ordered = Expression.Call(null, orderByDesc, expression, idAccess)

        let beforeOption = allArguments |> Array.tryFind (function Before _ -> true | _ -> false)
        let result =
            match beforeOption with
            | Some(Before id) -> 
                //TODO: 3a. parse id value using Relay GlobalId and retrieve "actual" id value
                // 4a. apply q.Where(p0 => p0.<ID_field> > id) on the ordered expression
                let predicate = Expression.Lambda(Expression.LessThan(p0, Expression.Constant id), p0)
                let where = methods.Where.MakeGenericMethod [| tSource |]
                Expression.Call(null, where, ordered, predicate)
            | None -> ordered
        // 5. apply result.Take(value)
        let take = methods.Take.MakeGenericMethod [| tSource |]
        Expression.Call(null, take, result, Expression.Constant value)
    | _ -> expression

let private linqArgs vars info =
    let argDefs = info.Definition.Args
    if Array.isEmpty argDefs then [||]
    else 
        let args = info.Ast.Arguments
        argDefs
        |> Array.map (fun a -> (a.Name, a, args |> List.tryFind (fun x -> x.Name = a.Name)))
        |> Array.choose (resolveLinqArg vars)

let private (|Object|Record|NotSupported|) (t: Type) =
    if FSharpType.IsRecord t then Record
    elif FSharpType.IsTuple t then NotSupported
    elif FSharpType.IsUnion t then NotSupported
    else Object
    
let private unwrapType = function
    | List inner -> inner.Type
    | Nullable inner -> inner.Type
    | tdef -> tdef.Type

let private castTo tCollection callExpr : Expression = 
    match tCollection with
    | Gen.List tRes ->
        let cast = Gen.listOfSeq.MakeGenericMethod [| tRes |]
        upcast Expression.Call(null, cast, [ callExpr ])
    | Gen.Array tRes ->
        let cast = Gen.arrayOfSeq.MakeGenericMethod [| tRes |]
        upcast Expression.Call(null, cast, [ callExpr ])
    | Gen.Set tRes ->
        let cast = Gen.setOfSeq.MakeGenericMethod [| tRes |]
        upcast Expression.Call(null, cast, [ callExpr ])
    | _ -> callExpr
        
let rec private eval vars tIn info (inputExpr: Expression) : Expression =
    if not <| info.Include vars
    then inputExpr
    else
        match info.Kind with
        | ResolveValue ->  inputExpr
        | SelectFields fields ->
            // construct new object initializer with bindings as list of assignments for each field
            let returnedType = unwrapType info.Definition.TypeDef
            constructObject vars returnedType fields inputExpr
        | ResolveCollection inner ->
            // apply Select on the Expr target
            // create a call, that will return either IEnumerable`1 or IQueryable`1
            let call =
                match tIn with
                | Gen.Queryable tSource -> 
                    constructCollection Gen.queryableMethods vars tSource inputExpr inner
                | Gen.Enumerable tSource ->
                    constructCollection Gen.enumerableMethods vars tSource inputExpr inner
                | _ -> raise (InvalidOperationException <| sprintf "Type %O is not enumerable" tIn)
            // enhance call with cast to result type
            castTo tIn call
        | ResolveAbstraction _ -> raise (NotSupportedException "Resolving abstract types is not supported for LINQ yet")

and private constructCollection (methods: Methods) vars tSource inputExpr inner = 
    let tResult = inner.ReturnDef.Type
    let args = linqArgs vars inner
    let p0 = Expression.Parameter(tSource)
    let body = eval vars inner.ParentDef.Type inner p0
    // call method - ((IQueryable<tSource>)inputExpr).Select(p0 => body)
    let call = 
        Expression.Call(
            // Select<tSource, tResult> - method to invoke
            methods.Select.MakeGenericMethod [| tSource; tResult |], 
            // `this` param - Convert(inputValue, IQueryable<tSource>)
            Expression.Convert(inputExpr, methods.Type.MakeGenericType [| tSource |]), 
            // `mapFunc` param - (p0 => body )
            Expression.Lambda(body, p0))
    args |> Array.fold (fun acc -> argumentToQueryable methods tSource args inner acc) call

and private constructObject vars (t: Type) (infos: ExecutionInfo list) inputExpr : Expression =
    let fieldMap = Dictionary()
    infos |> List.iter (fun f -> fieldMap.Add(f.Definition.Name.ToLower(), f)) 
    let ctor =
        match t with
        | Record -> FSharpValue.PreComputeRecordConstructorInfo t
        | Object -> 
            let fields = infos |> List.toArray |> Array.map (fun info -> info.Definition.Name)
            ReflectionHelper.matchConstructor t fields
        | NotSupported ->
            raise <| NotSupportedException (sprintf "LINQ conversion for type %O is not supported. Only POCOs and records are allowed." t)
    // try to match constructor arguments AND remove them from fieldMap
    let ctorArgs =
        ctor.GetParameters()
        |> Array.map (fun parameter -> 
            let paramName = parameter.Name.ToLower ()
            match fieldMap.TryGetValue paramName with
            | true, info -> 
                fieldMap.Remove paramName |> ignore
                let expr = unwrap info.Definition.Resolve inputExpr
                eval vars info.ReturnDef.Type info expr
            | false, _ -> upcast Expression.Default parameter.ParameterType)
    // if all query fields matched into constructor, invoke it with new expr
    // otherwise make member init expr, and pass remaining fields as member bindings
    if fieldMap.Count = 0
    then upcast Expression.New(ctor, ctorArgs)
    else 
        let members = 
            t.GetMembers()
            |> Array.map (fun m -> (m.Name.ToLower(), m))
            |> Map.ofArray
        let memberBindings : MemberBinding seq = 
            fieldMap
            |> Seq.map (fun kv -> 
                let m = Map.find kv.Key members
                upcast Expression.Bind(m, eval vars kv.Value.ParentDef.Type kv.Value inputExpr))
        upcast Expression.MemberInit(Expression.New(ctor, ctorArgs), memberBindings)        
        
let rec private toLinq info (query: IQueryable<'Source>) variables : IQueryable<'Result> =
    let collectionType = query.GetType()
    let parameter = Expression.Parameter(collectionType)
    let expr = eval variables collectionType info parameter
    let compiled =
        match expr with
        | :? MethodCallExpression as call -> 
            let lambda = Expression.Lambda(call, [| parameter |])
            let compiled = lambda.Compile()
            compiled
        | selector -> 
            let tSource = typeof<'Source>
            let tResult = typeof<'Result>
            let mSelect = Gen.queryableMethods.Select.MakeGenericMethod [| tSource; tResult |]
            let destinationType = Gen.queryableMethods.Type.MakeGenericType [| typeof<'Result> |]
            let call = Expression.Call(mSelect, Expression.Convert(parameter, destinationType), selector)
            Expression.Lambda(call, [| parameter |]).Compile()
    downcast compiled.DynamicInvoke [| box query |]

type FSharp.Data.GraphQL.Types.ExecutionInfo with
    
    /// Replaces top level type of the execution info with provided queryable source
    /// and constructs a LINQ expression from it, returing queryable with all applied 
    /// operations as the result.
    /// 
    /// GraphQL may define queries that will be interpreted in terms of LINQ operations,
    /// such as `orderBy`, `orderByDesc`, `skip`, `take, but also more advanced like:
    /// - `id` returning where comparison with object's id field.
    /// - `first`/`after` returning slice of the collection, ordered by id with id greater than provided in after param.
    /// - `last`/`before` returning slice of the collection, ordered by id descending with id less than provided in after param.
    member this.ToLinq(source: IQueryable<'Source>, ?variables: Map<string, obj>) : IQueryable<'Result> = 
        toLinq this source (defaultArg variables Map.empty)
        