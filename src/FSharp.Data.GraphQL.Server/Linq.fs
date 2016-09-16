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

/// Union type of arguments supported by LINQ interpreter
[<CustomComparison;CustomEquality>]
type private Arg = 
    | Id of IComparable
    | OrderBy of string
    | OrderByDesc of string
    | Skip of int
    | Take of int
    | First of int
    | Last of int
    | Before of string
    | After of string
    member x.Num = 
        match x with
        | Id _ -> 1
        | OrderBy _ -> 2
        | OrderByDesc _ -> 3
        | Skip _ -> 4
        | Take _ -> 5
        | First _ -> 6
        | Last _ -> 7
        | Before _ -> 8
        | After _ -> 9
    interface IEquatable<Arg> with
        member x.Equals y = x.Num = y.Num        
    interface IComparable with
        member x.CompareTo y =
            match y with
            | :? Arg as a -> x.Num.CompareTo a.Num
            | _ -> failwithf "Cannot compare Arg to %O" (y.GetType())
            
/// A track is used to represend a single member access statement in F# quotation expressions. 
/// It can be represented as (member: from -> to), where `member` in name of field/property getter,
/// `from` determines a type, which member is accessed and `to` a returned type of a member.
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

/// Data structure, that is used to tracking property getter trees. 
[<CustomComparison;CustomEquality>]
type private Tracker =
    { Name: string; 
      ParentType: Type;
      ReturnType: Type; 
      Args: Arg list; 
      Children: Set<Tracker> }
    interface IEquatable<Tracker> with
        member x.Equals y =
            x.Name = y.Name && x.ParentType = y.ParentType && x.ReturnType = y.ReturnType && x.Args = y.Args && x.Children = y.Children
    interface IComparable with
        member x.CompareTo y =
            match y with
            | :? Tracker as t -> 
                let c1 = x.Name.CompareTo t.Name
                if c1 <> 0
                then 
                    let c2 = (x.Args :> IComparable).CompareTo t.Args
                    if c2 <> 0
                    then  (x.Children :> IComparable).CompareTo t.Children
                    else c2
                else c1
            | _ -> failwithf "Cannot compare tracker to %O" (y.GetType())



/// Adds a child Tracker to it's new parent, returning updated parent as the result.
let private addChild child parent = { parent with Children = Set.add child parent.Children }

/// Helper function for creating Track structures.
let private mkTrack name src dst = { Name = name; From = src; To = dst }

/// Checks if `tFrom` somewhat matches `tRoot`, either by direct type comparison
/// or as a type argument in enumerable or option of root.
let private canJoin (tRoot: Type) (tFrom: Type) =
    if tFrom = tRoot then true
    elif tRoot.IsGenericType && (typeof<IEnumerable>.IsAssignableFrom tRoot || typedefof<Option<_>>.IsAssignableFrom tRoot)
    then tFrom = (tRoot.GetGenericArguments().[0])
    else false

/// Folds unrelated set of Tracks into a single Tracker tree, given the `tRoot`
/// as a root of the tree.
let rec private foldTracks trackSet (tRoot: Type) =
    let members = trackSet |> Set.filter (fun track -> canJoin tRoot track.From)
    let remaining = Set.difference trackSet members
    members
    |> Set.map(fun track ->
        let children = foldTracks remaining track.To
        { Name = track.Name; ParentType = track.From; ReturnType = track.To; Args = []; Children = children })
         
/// Takes function with 2 parameters and applies them in reversed order           
let inline private flip fn a b = fn b a

let inline private argVal<'t> vars argDef argOpt : 't option = 
    match argOpt with
    | Some arg -> Execution.argumentValue vars argDef arg
    | None -> argDef.DefaultValue
    |> Option.map (fun x -> x :?> 't)

/// Resolves an object representing one of the supported arguments
/// given a variables set and GraphQL input data.
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
    
let rec private unwrapType =
    function
    | List inner -> unwrapType inner
    | Nullable inner -> unwrapType inner
    | other -> other.Type

let private memberExpr tracker name parameter =
    let t = tracker.ReturnType
    match t.GetProperty name with
    | null -> 
        match t.GetField name with
        | null -> failwithf "Couldn't find property or field '%s' inside type '%O'" name t
        | field -> Expression.Field(parameter, field)
    | property -> Expression.Property(parameter, property)

/// Applies provided arguments data as a LINQ methods (either Queryable or Enumerable)
/// on a given source, constructing new LINQ expression in the result.
/// Supported arguments:
/// - Id(value)              -> .Where(p0 => p0.(idField) == value)
/// - OrderBy(fieldName)     -> .OrderBy(p0 => p0.(fieldName))
/// - OrderByDesc(fieldName) -> .OrderByDescending(p0 => p0.(fieldName))
/// - Skip(num)              -> .Skip(num)
/// - Take(num)              -> .Take(num)
/// - First(num)/After(id)   -> .Where(p0 => p0.(idField) > id).OrderBy(p0 => p0.(idField)).Take(num)
/// - Last(num)/Before(id)   -> .Where(p0 => p0.(idField) < id).OrderByDescending(p0 => p0.(idField)).Take(num)
let private argumentToQueryable (methods: Methods) tSource tracker expression arg =
    let allArguments = tracker.Args
    match arg with
    | Id value -> 
        let p0 = Expression.Parameter tSource
        let idProperty = memberExpr tracker "id" p0
        // Func<tSource, bool> predicate = p0 => p0 == value
        let predicate = Expression.Lambda(Expression.Equal(idProperty, Expression.Constant value), p0)
        let where = methods.Where.MakeGenericMethod [| tSource |]
        Expression.Call(null, where, expression, predicate)
    | OrderBy value ->
        let p0 = Expression.Parameter tSource
        // Func<tSource, tResult> memberAccess = p0 => p0.<value>
        let property = memberExpr tracker (string value) p0        
        let memberAccess = Expression.Lambda(property, [| p0 |])
        let orderBy = methods.OrderBy.MakeGenericMethod [| tSource; memberAccess.ReturnType |]
        Expression.Call(null, orderBy, expression, memberAccess)
    | OrderByDesc value -> 
        let p0 = Expression.Parameter tSource
        // Func<tSource, tResult> memberAccess = p0 => p0.<value>
        let property = memberExpr tracker (string value) p0        
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
        let idProperty = memberExpr tracker "id" p0
        // 2. apply q.OrderBy(p0 => p0.<ID_field>) on the expression
        let idAccess = Expression.Lambda(idProperty, [| p0 |])
        let orderBy = methods.OrderBy.MakeGenericMethod [| tSource; idAccess.ReturnType |]
        let ordered = Expression.Call(null, orderBy, expression, idAccess)

        let afterOption = allArguments |> List.tryFind (function After _ -> true | _ -> false)
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
        let idProperty = memberExpr tracker "id" p0
        // 2. apply q.OrderBy(p0 => p0.<ID_field>) on the expression
        let idAccess = Expression.Lambda(idProperty, [| p0 |])
        let orderByDesc = methods.OrderByDesc.MakeGenericMethod [| tSource; idAccess.ReturnType |]
        let ordered = Expression.Call(null, orderByDesc, expression, idAccess)

        let beforeOption = allArguments |> List.tryFind (function Before _ -> true | _ -> false)
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

/// Tries to resolve all supported LINQ args with values 
/// from a given ExecutionInfo and variables collection
let private linqArgs vars info =
    let argDefs = info.Definition.Args
    if Array.isEmpty argDefs then []
    else 
        let args = info.Ast.Arguments
        argDefs
        |> Array.map (fun a -> (a.Name, a, args |> List.tryFind (fun x -> x.Name = a.Name)))
        |> Array.choose (resolveLinqArg vars)
        |> Array.toList
        
let rec private track set e =
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

/// Creates a LINQ intermediate representation of ExecutionInfo by
/// traversing a provided F# quotation in order to catch all member 
/// accesses (fields / property getters) and tries to construct
/// a accessor dependency tree from them with root starting from `root` 
/// parameter.
///
/// This is a fast and naive way to resolve all properties that are possibily
/// accessed in resolver functions to include them inside LINQ query 
/// constructed later - this way we can potentially omit multiple database
/// calls as a result of underfetched data.
///
/// This technique doesn't track exact properties accessed from the `root`
/// and can can cause eager overfetching.
let rec private tracker vars (info: ExecutionInfo) : Tracker  =      
    match info.Kind with
    | ResolveValue -> rootTrack vars info
    | SelectFields fieldInfos -> 
        let root = rootTrack vars info
        fieldInfos |> List.fold (fun acc child -> branch acc info (tracker vars child) child) root
    | ResolveCollection innerInfo -> rootTrack vars info
    | ResolveAbstraction _ -> failwith "LINQ interpreter doesn't work with abstract types (interfaces/unions)"

and private rootTrack vars info = 
    let (Patterns.Lambda(_, Patterns.Lambda(root, expr))) = info.Definition.Resolve.Expr
    let tracks = track Set.empty expr
    { Name = root.Name; 
      ParentType = info.ParentDef.Type;
      ReturnType = info.ReturnDef.Type;
      Args = linqArgs vars info; 
      Children = foldTracks tracks root.Type }
        
/// Adds `childTrack` as a node to current `parentTrack`, using `parentInfo` 
/// and `childInfo` to determine to point of connection.
and private branch parentTrack (parentInfo: ExecutionInfo) childTrack (childInfo: ExecutionInfo) =
    let parentType = unwrapType parentInfo.ReturnDef
    if parentType = childInfo.ParentDef.Type
    then addChild parentTrack childTrack
    else
        // check if childTrack is a grand-child 
        let updated = parentTrack.Children |> Set.map (fun child -> branch child parentInfo childTrack childInfo)
        { parentTrack with Children = updated }  

let private (|Object|Record|NotSupported|) (t: Type) =
    if FSharpType.IsRecord t then Record
    elif FSharpType.IsTuple t then NotSupported
    elif FSharpType.IsUnion t then NotSupported
    else Object

/// Checks which collection type needs to be represented
/// and perfroms a necessary cast.
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

let rec private construct (tracker: Tracker) inParam =
    match tracker.ReturnType with
    | Gen.Queryable t -> constructCollection Gen.queryableMethods t tracker inParam
    | Gen.Enumerable t -> constructCollection Gen.enumerableMethods t tracker inParam
    | other -> constructObject tracker inParam

and private constructObject (tracker: Tracker) (inParam: ParameterExpression) : Expression =
    let tObj = tracker.ReturnType
    let trackerMap = 
        tracker.Children 
        |> Set.map (fun t -> (t.Name.ToLowerInvariant(), t)) 
        |> dict
    let fieldNames = trackerMap.Keys.ToArray()
    let ctor =
        match tObj with
        | Record -> FSharpValue.PreComputeRecordConstructorInfo tObj
        | Object -> 
            ReflectionHelper.matchConstructor tObj fieldNames
        | NotSupported ->
            raise <| NotSupportedException (sprintf "LINQ conversion for type %O is not supported. Only POCOs and records are allowed." tObj)
    let ctorArgs =
        ctor.GetParameters()
        |> Array.map (fun parameter -> 
            let paramName = parameter.Name.ToLower ()
            match trackerMap.TryGetValue paramName  with
            | true, childTracker -> 
                trackerMap.Remove paramName |> ignore
                construct childTracker inParam
            | false, _ -> upcast Expression.Default parameter.ParameterType)
    if trackerMap.Count = 0
    then upcast Expression.New(ctor, ctorArgs)
    else 
        let members = 
            tObj.GetMembers()
            |> Array.map (fun m -> (m.Name.ToLower(), m))
            |> Map.ofArray
        let memberBindings : MemberBinding seq = 
            trackerMap
            |> Seq.map (fun kv -> 
                let m = Map.find kv.Key members
                upcast Expression.Bind(m, construct kv.Value inParam))
        upcast Expression.MemberInit(Expression.New(ctor, ctorArgs), memberBindings) 
    
and private constructCollection methods (tSource: Type) (tracker: Tracker) (inParam: ParameterExpression) =
    let p0 = Expression.Parameter(tSource)
    let body = constructObject tracker p0
    // call method - ((IQueryable<tSource>)inputExpr).Select(p0 => body)
    let call = 
        Expression.Call(
            // Select<tSource, tResult> - method to invoke
            methods.Select.MakeGenericMethod [| tSource; tSource |], 
            // `this` param - Convert(inputValue, IQueryable<tSource>)
            Expression.Convert(inParam, methods.Type.MakeGenericType [| tSource |]), 
            // `mapFunc` param - (p0 => body )
            Expression.Lambda(body, p0))
    let final = tracker.Args |> List.fold (fun acc -> argumentToQueryable methods tSource tracker acc) call 
    upcast final
        
let private toLinq info (query: IQueryable<'Source>) variables : IQueryable<'Source> =
    let parameter = Expression.Parameter (query.GetType())
    let ir = tracker variables info
    let expr = construct ir parameter
    let compiled =
        match expr with
        | :? MethodCallExpression as call -> 
            let lambda = Expression.Lambda(call, [| parameter |])
            let compiled = lambda.Compile()
            compiled
        | selector -> 
            let tSource = typeof<'Source>
            let mSelect = Gen.queryableMethods.Select.MakeGenericMethod [| tSource; tSource |]
            let destinationType = Gen.queryableMethods.Type.MakeGenericType [| tSource |]
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
    member this.ToLinq(source: IQueryable<'Source>, ?variables: Map<string, obj>) : IQueryable<'Source> = 
        toLinq this source (defaultArg variables Map.empty)
        
//        
//let rec private eval vars tIn tracker info (inputExpr: Expression) : Expression =
//    if not <| info.Include vars
//    then inputExpr
//    else
//        match info.Kind with
//        | ResolveValue ->  inputExpr
//        | SelectFields fields ->
//            // construct new object initializer with bindings as list of assignments for each field
//            let returnedType = unwrapType info.Definition.TypeDef
//            constructObject vars returnedType fields inputExpr
//        | ResolveCollection inner ->
//            // apply Select on the Expr target
//            // create a call, that will return either IEnumerable`1 or IQueryable`1
//            let call =
//                match tIn with
//                | Gen.Queryable tSource -> 
//                    constructCollection Gen.queryableMethods vars tSource inputExpr inner
//                | Gen.Enumerable tSource ->
//                    constructCollection Gen.enumerableMethods vars tSource inputExpr inner
//                | _ -> raise (InvalidOperationException <| sprintf "Type %O is not enumerable" tIn)
//            // enhance call with cast to result type
//            castTo tIn call
//        | ResolveAbstraction _ -> raise (NotSupportedException "Resolving abstract types is not supported for LINQ yet")
//
//and private constructCollection (methods: Methods) vars tSource inputExpr inner = 
//    let tResult = inner.ReturnDef.Type
//    let args = linqArgs vars inner
//    let p0 = Expression.Parameter(tSource)
//    let body = eval vars inner.ParentDef.Type inner p0
//    // call method - ((IQueryable<tSource>)inputExpr).Select(p0 => body)
//    let call = 
//        Expression.Call(
//            // Select<tSource, tResult> - method to invoke
//            methods.Select.MakeGenericMethod [| tSource; tResult |], 
//            // `this` param - Convert(inputValue, IQueryable<tSource>)
//            Expression.Convert(inputExpr, methods.Type.MakeGenericType [| tSource |]), 
//            // `mapFunc` param - (p0 => body )
//            Expression.Lambda(body, p0))
//    args |> Array.fold (fun acc -> argumentToQueryable methods tSource args inner acc) call
//
//and private constructObject vars (t: Type) (infos: ExecutionInfo list) inputExpr : Expression =
//    let fieldMap = Dictionary()
//    infos |> List.iter (fun f -> fieldMap.Add(f.Definition.Name.ToLower(), f)) 
//    let ctor =
//        match t with
//        | Record -> FSharpValue.PreComputeRecordConstructorInfo t
//        | Object -> 
//            let fields = infos |> List.toArray |> Array.map (fun info -> info.Definition.Name)
//            ReflectionHelper.matchConstructor t fields
//        | NotSupported ->
//            raise <| NotSupportedException (sprintf "LINQ conversion for type %O is not supported. Only POCOs and records are allowed." t)
//    // try to match constructor arguments AND remove them from fieldMap
//    let ctorArgs =
//        ctor.GetParameters()
//        |> Array.map (fun parameter -> 
//            let paramName = parameter.Name.ToLower ()
//            match fieldMap.TryGetValue paramName with
//            | true, info -> 
//                fieldMap.Remove paramName |> ignore
//                let expr = unwrap info.Definition.Resolve inputExpr
//                eval vars info.ReturnDef.Type info expr
//            | false, _ -> upcast Expression.Default parameter.ParameterType)
//    // if all query fields matched into constructor, invoke it with new expr
//    // otherwise make member init expr, and pass remaining fields as member bindings
//    if fieldMap.Count = 0
//    then upcast Expression.New(ctor, ctorArgs)
//    else 
//        let members = 
//            t.GetMembers()
//            |> Array.map (fun m -> (m.Name.ToLower(), m))
//            |> Map.ofArray
//        let memberBindings : MemberBinding seq = 
//            fieldMap
//            |> Seq.map (fun kv -> 
//                let m = Map.find kv.Key members
//                upcast Expression.Bind(m, eval vars kv.Value.ParentDef.Type kv.Value inputExpr))
//        upcast Expression.MemberInit(Expression.New(ctor, ctorArgs), memberBindings) 