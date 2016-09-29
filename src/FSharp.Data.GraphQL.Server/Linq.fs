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

/// Record defining an argument resolved as part of the property tracker.
[<CustomComparison;CustomEquality>]
type Arg = 
    { /// Name of the argument. Matches field arguments from GraphQL type definitions.
      Name: string; 
      /// Value of the argument, provided with user request, query string or as default argument value.
      Value: obj }
    interface IEquatable<Arg> with
        member x.Equals y = x.Name = y.Name      
    interface IComparable with
        member x.CompareTo y =
            match y with
            | :? Arg as a -> x.Name.CompareTo a.Name
            | _ -> failwithf "Cannot compare Arg to %O" (y.GetType())
            
/// A track is used to represend a single member access statement in F# quotation expressions. 
/// It can be represented as (member: from -> to), where `member` in name of field/property getter,
/// `from` determines a type, which member is accessed and `to` a returned type of a member.
[<CustomComparison; CustomEquality>]
type Track = 
    { /// Name of the field or property. None is used only by root of the property tracker tree.
      Name: string option
      /// Type declaring field or property.
      ParentType: Type
      /// Type of value returned or stored by field or property.
      ReturnType: Type }
    override x.ToString() = sprintf "(%s: %s -> %s)" (defaultArg x.Name "") x.ParentType.Name x.ReturnType.Name
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

/// A representation of the property tree - describes a tree
/// of all properties and subproperties accessed in provided 
/// ExecutionInfo with top level argument given as a root.
type Tracker = 
    /// Leaf of the tree. Marks a direct field/property access with no sub-trees. 
    /// Consists of <see cref="Track"/> record and (neglible in this case) list of arguments.
    | Direct of Track * Arg list
    /// Marks branched field/property access - property value withh possible sub-trees.
    /// Consists of <see cref="Track"/> record list of arguments used to parametrize GraphQL 
    /// field definition and set of subtrees.
    | Compose of Track * Arg list * Set<Tracker>
    member x.Track = 
        match x with
        | Direct(track, _)     -> track
        | Compose(track, _, _) -> track

/// Adds a child Tracker to it's new parent, returning updated parent as the result.
let rec private addChild child = 
    function
    | Direct(track, args) -> Compose(track, args, Set.singleton child)
    | Compose(track, args, children) -> Compose(track, args, Set.add child children)

/// Helper function for creating Track structures.
let private mkTrack name src dst = { Name = Some name; ParentType = src; ReturnType = dst }
             
/// Takes function with 2 parameters and applies them in reversed order           
let inline private flip fn a b = fn b a

let inline private argVal vars argDef argOpt  = 
    match argOpt with
    | Some arg -> Execution.argumentValue vars argDef arg
    | None -> argDef.DefaultValue
    
/// Resolves an object representing one of the supported arguments
/// given a variables set and GraphQL input data.
let private resolveLinqArg vars (name, argdef, arg) =
    argVal vars argdef arg |> Option.map (fun v -> { Arg.Name = name; Value = v })
    
let rec private unwrapType =
    function
    | List inner -> unwrapType inner
    | Nullable inner -> unwrapType inner
    | other -> other.Type

open System.Reflection

let private bindingFlags = BindingFlags.Instance|||BindingFlags.IgnoreCase|||BindingFlags.Public
let private memberExpr (t: Type) name parameter =
    match t.GetProperty(name, bindingFlags) with
    | null -> 
        match t.GetField(name, bindingFlags) with
        | null -> failwithf "Couldn't find property or field '%s' inside type '%O'" name t
        | field -> Expression.Field(parameter, field)
    | property -> Expression.Property(parameter, property)

/// Input for custom argument applicators. Contains metadata associated with
/// current property call.
type CallableArg =
    { /// Resolved argument value related to current argument application.
      Argument: Arg
      /// List of all other arguments resolved as part of other possible 
      /// applications on the current property track.
      AllArguments: Arg list 
      /// Track describing field or property access.
      Track: Track
      /// Source type of the property - in case when track returns a collection or option,
      /// this is the type of the nested element.
      Type: Type
      /// Set of trackers defining nested property subtrees.
      Fields: Set<Tracker> }

/// Delegate describing application of the callable argument on the provided LINQ expression.
type ArgApplication = Expression -> CallableArg -> Expression

let private sourceAndMethods track =
    match track.ReturnType with
    | Gen.Queryable t -> t, Gen.queryableMethods
    | Gen.Enumerable t -> t, Gen.enumerableMethods
    | other -> failwithf "Type '%O' is neither queryable nor enumerable" other
    
/// Id(value) -> expression.Where(p0 => p0.(idField) == value)
let private applyId: ArgApplication = fun expression callable ->
    let tSource, methods = sourceAndMethods callable.Track
    let p0 = Expression.Parameter tSource
    let idProperty = memberExpr callable.Type "id" p0
    // Func<tSource, bool> predicate = p0 => p0 == value
    let predicate = Expression.Lambda(Expression.Equal(idProperty, Expression.Constant callable.Argument.Value), p0)
    let where = methods.Where.MakeGenericMethod [| tSource |]
    upcast Expression.Call(null, where, expression, predicate)
    
/// OrderBy(fieldName) -> expression.OrderBy(p0 => p0.(fieldName))
let private applyOrderBy: ArgApplication = fun expression callable ->
    let tSource, methods = sourceAndMethods callable.Track
    let p0 = Expression.Parameter tSource
    // Func<tSource, tResult> memberAccess = p0 => p0.<value>
    let property = memberExpr callable.Type (string callable.Argument.Value) p0        
    let memberAccess = Expression.Lambda(property, [| p0 |])
    let orderBy = methods.OrderBy.MakeGenericMethod [| tSource; memberAccess.ReturnType |]
    upcast Expression.Call(null, orderBy, expression, memberAccess)  
    
/// OrderByDesc(fieldName) -> expression.OrderByDescending(p0 => p0.(fieldName))
let private applyOrderByDesc: ArgApplication = fun expression callable ->
    let tSource, methods = sourceAndMethods callable.Track
    let p0 = Expression.Parameter tSource
    // Func<tSource, tResult> memberAccess = p0 => p0.<value>
    let property = memberExpr callable.Type (string callable.Argument.Value) p0        
    let memberAccess = Expression.Lambda(property, [| p0 |])
    let orderBy = methods.OrderByDesc.MakeGenericMethod [| tSource; memberAccess.ReturnType |]
    upcast Expression.Call(null, orderBy, expression, memberAccess)   
    
/// Skip(num) -> expression.Skip(num)
let private applySkip: ArgApplication = fun expression callable ->
    let tSource, methods = sourceAndMethods callable.Track
    let skip = methods.Skip.MakeGenericMethod [| tSource |]
    upcast Expression.Call(null, skip, expression, Expression.Constant(callable.Argument.Value))
    
/// Take(num) -> expression.Take(num)
let private applyTake: ArgApplication = fun expression callable ->
    let tSource, methods = sourceAndMethods callable.Track
    let skip = methods.Take.MakeGenericMethod [| tSource |]
    upcast Expression.Call(null, skip, expression, Expression.Constant(callable.Argument.Value))
    
/// First(num)/After(id) -> expression.Where(p0 => p0.(idField) > id).OrderBy(p0 => p0.(idField)).Take(num)
let private applyFirst: ArgApplication = fun expression callable ->
    let tSource, methods = sourceAndMethods callable.Track
    let p0 = Expression.Parameter tSource   
    // 1. Find ID field of the structure (info object is needed)
    let idProperty = memberExpr callable.Type "id" p0
    // 2. apply q.OrderBy(p0 => p0.<ID_field>) on the expression
    let idAccess = Expression.Lambda(idProperty, [| p0 |])
    let orderBy = methods.OrderBy.MakeGenericMethod [| tSource; idAccess.ReturnType |]
    let ordered = Expression.Call(null, orderBy, expression, idAccess)

    let afterOption = callable.AllArguments |> List.tryFind (fun a -> a.Name = "after")
    let result =
        match afterOption with
        | Some(after) -> 
            //TODO: 3a. parse id value using Relay GlobalId and retrieve "actual" id value
            // 4a. apply q.Where(p0 => p0.<ID_field> > id) on the ordered expression
            let predicate = Expression.Lambda(Expression.GreaterThan(p0, Expression.Constant after.Value), p0)
            let where = methods.Where.MakeGenericMethod [| tSource |]
            Expression.Call(null, where, ordered, predicate)
        | None -> ordered
    // 5. apply result.Take(value)
    let take = methods.Take.MakeGenericMethod [| tSource |]
    upcast Expression.Call(null, take, result, Expression.Constant callable.Argument.Value)
    
/// Last(num)/Before(id) -> expression.Where(p0 => p0.(idField) < id).OrderByDescending(p0 => p0.(idField)).Take(num)
let private applyLast: ArgApplication = fun expression callable ->
    let tSource, methods = sourceAndMethods callable.Track
    let p0 = Expression.Parameter tSource   
    // 1. Find ID field of the structure (info object is needed)
    let idProperty = memberExpr callable.Type "id" p0
    // 2. apply q.OrderBy(p0 => p0.<ID_field>) on the expression
    let idAccess = Expression.Lambda(idProperty, [| p0 |])
    let orderByDesc = methods.OrderByDesc.MakeGenericMethod [| tSource; idAccess.ReturnType |]
    let ordered = Expression.Call(null, orderByDesc, expression, idAccess)

    let beforeOption = callable.AllArguments |> List.tryFind (fun a -> a.Name = "after")
    let result =
        match beforeOption with
        | Some(before) -> 
            //TODO: 3a. parse id value using Relay GlobalId and retrieve "actual" id value
            // 4a. apply q.Where(p0 => p0.<ID_field> > id) on the ordered expression
            let predicate = Expression.Lambda(Expression.LessThan(p0, Expression.Constant before.Value), p0)
            let where = methods.Where.MakeGenericMethod [| tSource |]
            Expression.Call(null, where, ordered, predicate)
        | None -> ordered
    // 5. apply result.Take(value)
    let take = methods.Take.MakeGenericMethod [| tSource |]
    upcast Expression.Call(null, take, result, Expression.Constant callable.Argument.Value)

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

let private fieldOrProperty = MemberTypes.Field ||| MemberTypes.Property

/// Check if there exists a field or property described by the current track.
let private isOwn (track: Track) =
    match track.Name with
    | None -> false
    | Some name ->
        let t = 
            match track.ParentType with
            | Gen.Enumerable tparam -> tparam
            | tval -> tval
        match track.ParentType.GetMember(name, fieldOrProperty, bindingFlags) with
        | [||]  -> false
        | array -> 
            array |> Array.exists(fun m -> 
                match m with 
                | :? PropertyInfo as p -> p.PropertyType = track.ReturnType
                | :? FieldInfo as f -> f.FieldType = track.ReturnType
                | _ -> false)

let rec private merge parent members =
    if Set.isEmpty members
    then Set.singleton parent
    else match parent with
         | Direct(track, args) -> 
            if Option.isSome track.Name 
            then Compose(track, args, members) |> Set.singleton
            else match track.ReturnType with
                 | Gen.Enumerable _ -> Compose(track, args, members) |> Set.singleton
                 | _ -> members
         | Compose(track, args, children) -> 
            if Option.isSome track.Name 
            then Compose(track, args, children + members) |> Set.singleton
            else match track.ReturnType with
                 | Gen.Enumerable _ -> Compose(track, args, children + members) |> Set.singleton
                 | _ -> members

/// Composes trackers into tree within range of a single ExecutionInfo
/// `allTracks` is expected to be the list of Direct's (unrelated tracks)
/// which are going to be composed.
let rec private infoComposer (root: Tracker) (allTracks: Set<Tracker>) : Set<Tracker> = 
    let rootTrack = root.Track 
    let parentType = rootTrack.ReturnType  
    let members = allTracks |> Set.filter (fun (Direct(track, _)) -> canJoin parentType track.ParentType && isOwn track)
    if Set.isEmpty members
    then
        // check for artificial property
        // eg. given type Parent = { fname: string; lname: string }
        // and field definition "fullName": p -> p.fname + " " + p.lname
        // we don't want to return fullName Tracker (as such field doesn't exists)
        // but fname and lname instead
        let grandpaType = rootTrack.ParentType
        let members = allTracks |> Set.filter (fun (Direct(track, _)) -> canJoin grandpaType track.ParentType && isOwn track)
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
        let newRoot = merge root results
        newRoot

/// Composes tracks collected within a single ExecutionInfo
/// (but not across the ExecutionInfo boundaries)
let rec private compose vars ir = 
    let (IR(info, directs, children)) = ir
    let rootTrack = { Name = None; ParentType = info.ParentDef.Type; ReturnType = info.ReturnDef.Type }
    let root = Direct(rootTrack, linqArgs vars info)
    let composed = infoComposer root directs
    IR(info, composed, children |> List.map (compose vars))

/// Get unrelated tracks from current info and its children (if any)
/// Returned set of trackers ALWAYS consists of Direct trackers only
let rec private getTracks info =
    let (Patterns.WithValue(_,_, (Patterns.Lambda(_, Patterns.Lambda(root, expr))))) = info.Definition.Resolve.Expr
    let tracks = track Set.empty expr |> Set.map(fun track -> Direct(track, []))
    match info.Kind with
    | ResolveValue -> IR(info, tracks, [])
    | SelectFields fieldInfos -> IR(info, tracks, List.map getTracks fieldInfos)
    | ResolveCollection inner -> IR(info, tracks, [ getTracks inner ])
    | ResolveAbstraction _ -> failwith "LINQ interpreter doesn't work with abstract types (interfaces/unions)" 
    
let rec private join parentTracks childIRs =
    let childrenCombined =
        childIRs
        |> List.fold (fun acc (IR(_, childTracks, grandchildIRs)) ->
            acc + join childTracks grandchildIRs) Set.empty
    if Set.isEmpty childrenCombined
    then parentTracks
    else parentTracks |> Set.collect (fun parentTrack -> merge parentTrack childrenCombined)
            
/// Adds `childTrack` as a node to current `parentTrack`, using `parentInfo` 
/// and `childInfo` to determine to point of connection.
and private branch parentTrack (parentInfo: ExecutionInfo) childTrack (childInfo: ExecutionInfo) =
    let parentType = unwrapType parentInfo.ReturnDef
    if parentType = childInfo.ParentDef.Type
    then addChild childTrack parentTrack
    else
        match parentTrack with
        | Compose(track, args, fields) -> Compose(track, args, fields |> Set.map (fun child -> branch child parentInfo childTrack childInfo))
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

let rec private construct (argApplicators: Map<string, ArgApplication>) tracker (inParam: Expression) : Expression =
    match tracker with
    | Direct(track, _) -> inParam// upcast Expression.PropertyOrField(inParam, track.Name)
    | Compose(track, _, fields) -> 
        match track.ReturnType with
        | Gen.Enumerable _ -> constructCollection argApplicators tracker inParam |> castTo track.ReturnType
        | _ -> constructObject argApplicators track.ReturnType fields inParam

and private constructObject argApplicators tObj fields (inParam: Expression) : Expression =
    let trackerMap = Dictionary<_,_>() 
    fields 
    |> Set.filter (fun t -> t.Track.Name |> Option.isSome)
    |> Set.iter (fun t -> trackerMap.Add(t.Track.Name.Value.ToLowerInvariant(), t)) 

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
                let fieldOrProperty = memberExpr tObj paramName inParam
                construct argApplicators childTracker fieldOrProperty
            | false, _ -> upcast Expression.Default parameter.ParameterType)
    if trackerMap.Count = 0
    then upcast Expression.New(ctor, ctorArgs)
    else 
        let members = 
            tObj.GetProperties(BindingFlags.SetProperty|||BindingFlags.Instance|||BindingFlags.IgnoreCase|||BindingFlags.Public)
            |> Array.map (fun p -> p:> MemberInfo)
            |> Array.append (tObj.GetFields(BindingFlags.Instance|||BindingFlags.IgnoreCase|||BindingFlags.Public) |> Array.map (fun p -> p:> MemberInfo))
            |> Array.map (fun m -> (m.Name.ToLower(), m))
            |> Map.ofArray
        let memberBindings = 
            trackerMap
            |> Seq.map (fun kv -> 
                let m = Map.find kv.Key members
                let fieldOrProperty = memberExpr tObj kv.Key inParam
                Expression.Bind(m, construct argApplicators kv.Value fieldOrProperty) :> MemberBinding)
            |> Seq.toArray
        upcast Expression.MemberInit(Expression.New(ctor, ctorArgs), memberBindings) 
    
and private constructCollection argApplicators tracker (inParam: Expression) : Expression =
    let (Compose(track, args, fields)) = tracker
    let tSource, methods = sourceAndMethods track
    let p0 = Expression.Parameter(tSource)
    let body = constructObject argApplicators tSource fields p0
    // call method - ((IQueryable<tSource>)inputExpr).Select(p0 => body)
    let call: Expression = 
        upcast Expression.Call(
            // Select<tSource, tResult> - method to invoke
            methods.Select.MakeGenericMethod [| tSource; tSource |], 
            // `this` param - Convert(inputValue, IQueryable<tSource>)
            Expression.Convert(inParam, methods.Type.MakeGenericType [| tSource |]), 
            // `mapFunc` param - (p0 => body )
            Expression.Lambda(body, p0))
    let final = args |> List.fold (fun acc (arg: Arg) -> 
        match Map.tryFind (arg.Name.ToLowerInvariant()) argApplicators with
        | Some apply -> apply acc { AllArguments = args; Argument = arg; Track = track; Fields = fields; Type = tSource }
        | None -> acc) call 
    final

let private defaultArgApplicators: Map<string, ArgApplication> = 
    Map.ofList [
        "id", applyId
        "orderby", applyOrderBy
        "orderbydesc", applyOrderByDesc
        "skip", applySkip
        "take", applyTake
        "first", applyFirst
        "last", applyLast ]
    
/// <summary>
/// <para>
/// Creates an intermediate representation of ExecutionInfo by
/// traversing all F# quotations provided in field definition resolvers
/// in order to catch all member accesses (fields / property getters) and 
/// tries to construct a dependency tree from them with root starting from 
/// query `root` object parameter.
/// </para><para>
/// This is a fast and naive way to resolve all properties that are possibily
/// accessed in resolver functions to include them inside LINQ query 
/// constructed later - this way we can potentially omit multiple database
/// calls as a result of underfetched data.
/// </para><para>
/// NOTE: This technique doesn't track exact properties accessed from the `root`
/// and can can cause eager overfetching.
/// </para>
/// </summary>
let rec tracker (vars: Map<string, obj>) (info: ExecutionInfo) : Tracker  =       
    let ir = getTracks info
    let composed = compose vars ir
    let (IR(info, trackers, children)) = composed
    join trackers children |> Seq.head

let private toLinq info (query: IQueryable<'Source>) variables (argApplicators: Map<string, ArgApplication>) : IQueryable<'Source> =
    let parameter = Expression.Parameter (query.GetType())
    let ir = tracker variables info
    let expr = construct argApplicators ir parameter
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

type System.Linq.IQueryable<'Source> with

    /// <summary>
    /// <para>
    /// Replaces top level type of the execution info with provided queryable source
    /// and constructs a LINQ expression from it, returing queryable with all applied 
    /// operations as the result.
    /// </para>
    /// <para>
    /// By default, GraphQL may define queries that will be interpreted in terms of LINQ operations,
    /// such as `orderBy`, `orderByDesc`, `skip`, `take, but also more advanced like:
    /// <para>- `id` returning where comparison with object's id field.</para>
    /// <para>- `first`/`after` returning slice of the collection, ordered by id with id greater than provided in after param.</para>
    /// <para>- `last`/`before` returning slice of the collection, ordered by id descending with id less than provided in after param.</para>
    /// </para>
    /// </summary>
    /// <param name="info">Execution info data to be applied on the queryable.</param>
    /// <param name="variables">Optional map with client-provided arguments used to resolve argument values.</param>
    /// <param name="applicators">Map of applicators used to define LINQ expression mutations based on GraphQL arguments.</param>
    member this.Apply(info: ExecutionInfo, ?variables: Map<string, obj>, ?applicators: Map<string, ArgApplication>) : IQueryable<'Source> = 
        let appl = 
            match applicators with
            | None -> defaultArgApplicators
            | Some a -> a |> Map.fold (fun acc key value -> Map.add (key.ToLowerInvariant()) value acc) defaultArgApplicators
        toLinq info this (defaultArg variables Map.empty) appl