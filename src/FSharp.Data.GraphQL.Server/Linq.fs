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
      ParentType: Type
      ReturnType: Type }
    override x.ToString() = sprintf "(%s: %s -> %s)" x.Name x.ParentType.Name x.ReturnType.Name
    interface IEquatable<Track> with
        member x.Equals y = (x.Name = y.Name && x.ParentType.FullName = y.ParentType.FullName && x.ReturnType.FullName = y.ReturnType.FullName)
    interface IComparable with
        member x.CompareTo y = 
            match y with
            | :? Track as t -> 
                let c = compare x.Name t.Name
                if c = 0
                then 
                    let c = compare x.ParentType.FullName t.ParentType.FullName
                    if c = 0 then compare x.ReturnType.FullName t.ReturnType.FullName else c
                else c                        
            | _ -> invalidArg "y" "Track cannot be compared to other type"

type private Tracker = 
    | Direct of Track
    | Compose of Track * Set<Tracker>
    | Collection of Track * Arg list * Tracker
    member x.Track = 
        match x with
        | Direct track            -> track
        | Compose(track, _)       -> track
        | Collection(track, _, _) -> track

/// Adds a child Tracker to it's new parent, returning updated parent as the result.
let rec private addChild child = 
    function
    | Direct track -> Compose(track, Set.singleton child)
    | Compose(track, children) -> Compose(track, Set.add child children)
    | Collection(track, args, inner) -> Collection(track, args, addChild child inner)

/// Helper function for creating Track structures.
let private mkTrack name src dst = { Name = name; ParentType = src; ReturnType = dst }
             
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

open System.Reflection

let private bindingFlags = BindingFlags.Instance|||BindingFlags.IgnoreCase|||BindingFlags.Public
let private memberExpr (tracker: Tracker) name parameter =
    let t = tracker.Track.ReturnType
    match t.GetProperty(name, bindingFlags) with
    | null -> 
        match t.GetField(name, bindingFlags) with
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
let private argumentToQueryable (methods: Methods) tSource (tracker: Tracker) expression arg =
    match tracker with
    | Collection(track, allArguments, innerTracker) ->
        match arg with
        | Id value -> 
            let p0 = Expression.Parameter tSource
            let idProperty = memberExpr innerTracker "id" p0
            // Func<tSource, bool> predicate = p0 => p0 == value
            let predicate = Expression.Lambda(Expression.Equal(idProperty, Expression.Constant value), p0)
            let where = methods.Where.MakeGenericMethod [| tSource |]
            Expression.Call(null, where, expression, predicate)
        | OrderBy value ->
            let p0 = Expression.Parameter tSource
            // Func<tSource, tResult> memberAccess = p0 => p0.<value>
            let property = memberExpr innerTracker (string value) p0        
            let memberAccess = Expression.Lambda(property, [| p0 |])
            let orderBy = methods.OrderBy.MakeGenericMethod [| tSource; memberAccess.ReturnType |]
            Expression.Call(null, orderBy, expression, memberAccess)
        | OrderByDesc value -> 
            let p0 = Expression.Parameter tSource
            // Func<tSource, tResult> memberAccess = p0 => p0.<value>
            let property = memberExpr innerTracker (string value) p0        
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
            let idProperty = memberExpr innerTracker "id" p0
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
            let idProperty = memberExpr innerTracker "id" p0
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
    | _ -> raise <| NotSupportedException "Currently only collection arguments are supported"

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

/// Intermediate representation containing info about all resolved
/// member acesses. Contains execution info, which resolver function
/// was used, set of tracks found inside resolver function
/// and list of all children (empty for ResolveValue).
type private IR = IR of ExecutionInfo * Set<Tracker> * IR list

/// Checks if `tFrom` somewhat matches `tRoot`, either by direct type comparison
/// or as a type argument in enumerable or option of root.
let private canJoin (tRoot: Type) (tFrom: Type) =
    if tFrom = tRoot then true
    elif tRoot.IsGenericType && (typeof<IEnumerable>.IsAssignableFrom tRoot || typedefof<Option<_>>.IsAssignableFrom tRoot)
    then tFrom = (tRoot.GetGenericArguments().[0])
    else false

let rec private merge parent members =
    if Set.isEmpty members
    then parent
    else match parent with
         | Direct(track) -> Compose(track, members)
         | Compose(track, children) -> Compose(track, children + members)
         | Collection(track, args, inner) -> Collection(track, args, merge inner members)     

/// Composes trackers into tree within range of a single ExecutionInfo
/// `allTracks` is expected to be the list of Direct's (unrelated tracks)
/// which are going to be composed.
let rec private infoComposer (root: Tracker) (allTracks: Set<Tracker>) : Set<Tracker> = 
    let rootTrack = root.Track 
    let parentType = rootTrack.ReturnType  
    let members = allTracks |> Set.filter (fun (Direct track) -> canJoin parentType track.ParentType)
    if Set.isEmpty members
    then
        // check for artificial property
        // eg. given type Parent = { fname: string; lname: string }
        // and field definition "fullName": p -> p.fname + " " + p.lname
        // we don't want to return fullName Tracker (as such field doesn't exists)
        // but fname and lname instead
        let grandpaType = rootTrack.ParentType
        let members = allTracks |> Set.filter (fun (Direct track) -> canJoin grandpaType track.ParentType)
        if Set.isEmpty members
        then root |> Set.singleton
        else
            let remaining = Set.difference allTracks members
            members
            |> Set.map(fun tracker -> infoComposer tracker remaining)
            |> Seq.collect id
            |> Set.ofSeq
    else
        // compose remaining elements recursivelly under members
        let remaining = Set.difference allTracks members
        let results =
            members
            |> Set.map(fun tracker -> infoComposer tracker remaining)
            |> Seq.collect id
            |> Set.ofSeq
        let newRoot =
            match parentType with
            | Gen.Enumerable t -> Collection(rootTrack, [], merge root results)
            | _ -> merge root results
        newRoot |> Set.singleton

/// Composes tracks collected within a single ExecutionInfo
/// (but not across the ExecutionInfo boundaries)
let rec private compose ir = 
    let (IR(info, directs, children)) = ir
    let root = Direct { Name = info.Definition.Name; ParentType = info.ParentDef.Type; ReturnType = info.ReturnDef.Type }
    let composed = infoComposer root directs
    IR(info, composed, children |> List.map compose)

/// Get unrelated tracks from current info and its children (if any)
/// Returned set of trackers ALWAYS consists of Direct trackers only
let rec private getTracks info =
    let (Patterns.WithValue(_,_, (Patterns.Lambda(_, Patterns.Lambda(root, expr))))) = info.Definition.Resolve.Expr
    let tracks = track Set.empty expr |> Set.map Direct
    match info.Kind with
    | ResolveValue -> IR(info, tracks, [])
    | SelectFields fieldInfos -> IR(info, tracks, List.map getTracks fieldInfos)
    | ResolveCollection inner -> IR(info, tracks, [ getTracks inner ])
    | ResolveAbstraction _ -> failwith "LINQ interpreter doesn't work with abstract types (interfaces/unions)" 

let rec private joinToLeaves vars info child parent =
    let returnType = info.ReturnDef.Type
    let (IR(childInfo, trackSet, irs)) = child
    // join children to current parent
    match irs with
    | [] -> 
        trackSet
        |> Set.fold (fun acc tracker -> join returnType acc tracker) parent
    | grandChildren -> 
        trackSet
        |> Set.fold(fun acc tracker ->
            let childTracker = 
                grandChildren            
                |> List.fold (fun acc grandChild -> joinToLeaves vars childInfo grandChild acc) tracker
            match acc with
            | Direct track when track.ReturnType = returnType ->  
                match returnType with
                | Gen.Enumerable _ -> Collection(track, linqArgs vars info, childTracker)
                | _ -> Compose(track, Set.singleton childTracker)
            | Direct _ -> acc
            | Compose(track, children) when track.ReturnType = returnType -> Compose(track, Set.add childTracker children)
            | Compose(track, children) -> Compose(track, children |> Set.map (joinToLeaves vars info child))
            | Collection(track, args, inner) -> Collection(track, args, merge inner (Set.singleton childTracker))) parent

and private join returnType parent child =
    match parent with
    | Direct track when track.ReturnType = returnType ->    
        Compose(track, Set.singleton child)
    | Direct _ -> 
        parent
    | Compose(track, children) when track.ReturnType = returnType -> 
        Compose(track, Set.add child children)
    | Compose(track, children) ->
        Compose(track, children)
    | Collection(track, args, inner) -> 
        Collection(track, args, merge inner (Set.singleton child))

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
    let ir = getTracks info
    let composed = compose ir
    let (IR(_, tracker, children)) = composed
    children
    |> List.fold (fun acc child -> joinToLeaves vars info child acc) (Seq.head tracker)
            
/// Adds `childTrack` as a node to current `parentTrack`, using `parentInfo` 
/// and `childInfo` to determine to point of connection.
and private branch parentTrack (parentInfo: ExecutionInfo) childTrack (childInfo: ExecutionInfo) =
    let parentType = unwrapType parentInfo.ReturnDef
    if parentType = childInfo.ParentDef.Type
    then addChild childTrack parentTrack
    else
        match parentTrack with
        | Collection(_, _, inner) -> branch inner parentInfo childTrack childInfo
        | Compose(track, fields) -> Compose(track, fields |> Set.map (fun child -> branch child parentInfo childTrack childInfo))
        | Direct(_) -> parentTrack

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

let rec private construct tracker (inParam: Expression) : Expression =
    match tracker with
    | Direct(track) -> inParam// upcast Expression.PropertyOrField(inParam, track.Name)
    | Compose(track, fields) -> constructObject tracker inParam
    | Collection(track, args, inner) ->
        match track.ReturnType with
        | Gen.Queryable t -> constructCollection Gen.queryableMethods t tracker inParam |> castTo track.ReturnType
        | Gen.Enumerable t -> constructCollection Gen.enumerableMethods t tracker inParam |> castTo track.ReturnType
        | other -> failwithf "Type '%O' is neither IQueryable nor IEnumerable" other

and private constructObject (tracker: Tracker) (inParam: Expression) : Expression =
    let (Compose(track, fields)) = tracker
    let tObj = track.ReturnType
    let trackerMap = Dictionary<_,_>() 
    fields |> Set.iter (fun t -> trackerMap.Add(t.Track.Name.ToLowerInvariant(), t)) 

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
                let fieldOrProperty = memberExpr tracker paramName inParam
                construct childTracker fieldOrProperty
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
    
and private constructCollection methods (tSource: Type) tracker (inParam: Expression) : Expression =
    let (Collection(track, args, inner)) = tracker
    let p0 = Expression.Parameter(tSource)
    let body = construct inner p0
    // call method - ((IQueryable<tSource>)inputExpr).Select(p0 => body)
    let call = 
        Expression.Call(
            // Select<tSource, tResult> - method to invoke
            methods.Select.MakeGenericMethod [| tSource; tSource |], 
            // `this` param - Convert(inputValue, IQueryable<tSource>)
            Expression.Convert(inParam, methods.Type.MakeGenericType [| tSource |]), 
            // `mapFunc` param - (p0 => body )
            Expression.Lambda(body, p0))
    let final = args |> List.fold (fun acc -> argumentToQueryable methods tSource tracker acc) call 
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
    let result = compiled.DynamicInvoke [| box query |]
    match result with
    | :? IQueryable<'Source> as q -> q
    | :? IEnumerable<'Source> as e -> e.AsQueryable()
    | _ -> failwithf "Unrecognized type '%O' is neither IEnumerable<%O> nor IQueryable<%O>" (result.GetType()) typeof<'Source> typeof<'Source>

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