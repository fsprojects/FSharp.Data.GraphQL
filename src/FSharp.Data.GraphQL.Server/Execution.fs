/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Execution

open System
open System.Collections.Generic
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Helpers

type Error = string * obj list

type Output = IDictionary<string, obj>

type GQLResponse =
    { Content : GQLResponseContent
      Metadata : Metadata }
    static member Direct(data, errors, meta) =
        { Content = Direct (data, errors)
          Metadata = meta }
    static member Deferred(data, errors, deferred, meta) =
        { Content = Deferred (data, errors, deferred)
          Metadata = meta }
    static member Stream(data, meta) =
        { Content = Stream data
          Metadata = meta }
    static member Empty(meta) =
        GQLResponse.Direct(new Dictionary<string, obj>() :> Output, [], meta)
    static member Error(msg, meta) =
        GQLResponse.Direct(new Dictionary<string, obj>() :> Output, [ msg, [] ], meta)
    static member ErrorAsync(msg, meta) =
        asyncVal { return GQLResponse.Error(msg, meta) }

and GQLResponseContent =
    | Direct of data : Output * errors: Error list
    | Deferred of data : Output * errors : Error list * defer : IObservable<Output>
    | Stream of stream : IObservable<Output>

let (|Direct|Deferred|Stream|) (response : GQLResponse) =
    match response.Content with
    | Direct (data, errors) -> Direct (data, errors)
    | Deferred (data, errors, deferred) -> Deferred (data, errors, deferred)
    | Stream data -> Stream data

/// Name value lookup used as output to be serialized into JSON.
/// It has a form of a dictionary with fixed set of keys. Values under keys
/// can be set, but no new entry can be added or removed, once lookup
/// has been initialized.
/// This dicitionay implements structural equality.
type NameValueLookup(keyValues: KeyValuePair<string, obj> []) =
    let kvals = keyValues |> Array.distinctBy (fun kv -> kv.Key)
    let setValue key value =
        let mutable i = 0
        while i < kvals.Length do
            if kvals.[i].Key = key then
                kvals.[i] <- KeyValuePair<string, obj>(key, value)
                i <- Int32.MaxValue
            else i <- i+1
    let getValue key = (kvals |> Array.find (fun kv -> kv.Key = key)).Value
    let rec structEq (x: NameValueLookup) (y: NameValueLookup) =
        if Object.ReferenceEquals(x, y) then true
        elif Object.ReferenceEquals(y, null) then false
        elif Object.ReferenceEquals(x, null) then false
        elif x.Count <> y.Count then false
        else
            x.Buffer
            |> Array.forall2 (fun (a: KeyValuePair<string, obj>) (b: KeyValuePair<string, obj>) ->
                if a.Key <> b.Key then false
                else
                    match a.Value, b.Value with
                    | (:? NameValueLookup as x), (:? NameValueLookup as y) -> structEq x y
                    | (:? seq<obj> as x), (:? seq<obj> as y) -> 
                        if Seq.length x <> Seq.length y then false else Seq.forall2 (=) x y
                    | a1, b1 -> a1 = b1) y.Buffer
    let pad (sb: System.Text.StringBuilder) times =
        for _ in 0..times do sb.Append("\t") |> ignore
    let rec stringify (sb: System.Text.StringBuilder) deep (o:obj) =
        match o with
        | :? NameValueLookup as lookup ->
            if lookup.Count > 0 then
                sb.Append("{ ") |> ignore
                lookup.Buffer
                |> Array.iter (fun kv -> 
                    sb.Append(kv.Key).Append(": ") |> ignore
                    stringify sb (deep+1) kv.Value
                    sb.Append(",\r\n") |> ignore
                    pad sb deep)
                sb.Remove(sb.Length - 4 - deep, 4 + deep).Append(" }") |> ignore
        | :? string as s ->
            sb.Append("\"").Append(s).Append("\"") |> ignore
        | :? System.Collections.IEnumerable as s ->
            sb.Append("[") |> ignore
            for i in s do 
                stringify sb (deep + 1) i
                sb.Append(", ") |> ignore
            sb.Append("]") |> ignore
        | other ->
            if isNull other |> not
            then sb.Append(other.ToString()) |> ignore
            else sb.Append("null") |> ignore
        ()
    /// Returns raw content of the current lookup.
    member private __.Buffer : KeyValuePair<string, obj> [] = kvals
    /// Return a number of entries stored in current lookup. It's fixed size.
    member __.Count = kvals.Length
    /// Updates an entry's value under given key. It will throw an exception
    /// if provided key cannot be found in provided lookup.
    member __.Update key value = setValue key value
    override x.Equals(other) =
        match other with
        | :? NameValueLookup as lookup -> structEq x lookup
        | _ -> false
    override __.GetHashCode() =
        let mutable hash = 0
        for kv in kvals do
            hash <- (hash*397) ^^^ (kv.Key.GetHashCode()) ^^^ (if isNull kv.Value then 0 else kv.Value.GetHashCode())
        hash
    override x.ToString() =
        let sb =Text.StringBuilder()
        stringify sb 1 x
        sb.ToString()
    interface IEquatable<NameValueLookup> with
        member x.Equals(other) = structEq x other
    interface System.Collections.IEnumerable with
        member __.GetEnumerator() = (kvals :> System.Collections.IEnumerable).GetEnumerator()
    interface IEnumerable<KeyValuePair<string, obj>> with
        member __.GetEnumerator() = (kvals :> IEnumerable<KeyValuePair<string, obj>>).GetEnumerator()
    interface IDictionary<string, obj> with
        member __.Add(_, _) = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member __.Add(_) = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member __.Clear() = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member __.Contains(item) = kvals |> Array.exists ((=) item)
        member __.ContainsKey(key) = kvals |> Array.exists (fun kv -> kv.Key = key)
        member __.CopyTo(array, arrayIndex) = kvals.CopyTo(array, arrayIndex)
        member x.Count = x.Count
        member __.IsReadOnly = true
        member __.Item
            with get (key) = getValue key
            and set (key) v = setValue key v
        member __.Keys = upcast (kvals |> Array.map (fun kv -> kv.Key))
        member __.Values = upcast (kvals |> Array.map (fun kv -> kv.Value))
        member __.Remove(_:string) =
            raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
            false
        member __.Remove(_:KeyValuePair<string,obj>) =
            raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
            false
        member __.TryGetValue(key, value) =
            match kvals |> Array.tryFind (fun kv -> kv.Key = key) with
            | Some kv -> value <- kv.Value; true
            | None -> value <- null; false
    new(t: (string * obj) list) =
        NameValueLookup(t |> List.map (fun (k, v) -> KeyValuePair<string,obj>(k, v)) |> List.toArray)
    new(t: string []) =
        NameValueLookup(t |> Array.map (fun k -> KeyValuePair<string,obj>(k, null)))

module NameValueLookup =
    /// Create new NameValueLookup from given list of key-value tuples.
    let ofList (l: (string * obj) list) = NameValueLookup(l)

let private collectDefaultArgValue acc (argdef: InputFieldDef) =
    match argdef.DefaultValue with
    | Some defVal -> Map.add argdef.Name defVal acc
    | None -> acc

let internal argumentValue variables (argdef: InputFieldDef) (argument: Argument) =
    match argdef.ExecuteInput argument.Value variables  with
    | null -> argdef.DefaultValue
    | value -> Some value

let private getArgumentValues (argDefs: InputFieldDef []) (args: Argument list) (variables: Map<string, obj>) : Map<string, obj> =
    argDefs
    |> Array.fold (fun acc argdef ->
        match List.tryFind (fun (a: Argument) -> a.Name = argdef.Name) args with
        | Some argument ->
            match argumentValue variables argdef argument with
            | Some v -> Map.add argdef.Name v acc
            | None -> acc
        | None -> collectDefaultArgValue acc argdef
    ) Map.empty

let private getOperation = function
    | OperationDefinition odef -> Some odef
    | _ -> None

let internal findOperation doc opName =
    match doc.Definitions |> List.choose getOperation, opName with
    | [def], _ -> Some def
    | defs, name ->
        defs
        |> List.tryFind (fun def -> def.Name = name)

let private defaultResolveType possibleTypesFn abstractDef : obj -> ObjectDef =
    let possibleTypes = possibleTypesFn abstractDef
    let mapper = match abstractDef with Union u -> u.ResolveValue | _ -> id
    fun value ->
        let mapped = mapper value
        possibleTypes
        |> Array.find (fun objdef ->
            match objdef.IsTypeOf with
            | Some isTypeOf -> isTypeOf mapped
            | None -> false)

let private resolveInterfaceType possibleTypesFn (interfacedef: InterfaceDef) =
    match interfacedef.ResolveType with
    | Some resolveType -> resolveType
    | None -> defaultResolveType possibleTypesFn interfacedef

let private resolveUnionType possibleTypesFn (uniondef: UnionDef) =
    match uniondef.ResolveType with
    | Some resolveType -> resolveType
    | None -> defaultResolveType possibleTypesFn uniondef

let private createFieldContext objdef argDefs ctx (info: ExecutionInfo) =
    let fdef = info.Definition
    let args = getArgumentValues argDefs info.Ast.Arguments ctx.Variables
    { ExecutionInfo = info
      Context = ctx.Context
      ReturnType = fdef.TypeDef
      ParentType = objdef
      Schema = ctx.Schema
      Args = args
      Variables = ctx.Variables }         
                
let private resolveField (execute: ExecuteField) (ctx: ResolveFieldContext) (parentValue: obj) =
    if ctx.ExecutionInfo.IsNullable
    then
        execute ctx parentValue
        |> AsyncVal.map(optionCast)
    else
        execute ctx parentValue
        |> AsyncVal.map(fun v -> if isNull v then None else Some v)

// Deferred values require knowledge of their parent value
// Also, the values we return for the non-deferred values are all leaves in the resolution tree
// So what we do is build up a tree containing all of the result values, rather than just computing the leaves,
// Then we use that tree to resolve the original query, and pass it along to the deferred fields
// So that they know their parent values, and are able to properly resolve

/// Represents the materialized tree of all result values
type ResolverTree =
    | ResolverLeaf of ResolverLeaf
    | ResolverError of ResolverError
    | ResolverObjectNode of ResolverNode
    | ResolverListNode of ResolverNode
    member x.Name =
        match x with
        | ResolverLeaf leaf -> leaf.Name
        | ResolverError err -> err.Name
        | ResolverObjectNode node -> node.Name
        | ResolverListNode l -> l.Name
    member x.Value =
        match x with
        | ResolverLeaf leaf -> leaf.Value
        | ResolverError _ -> None
        | ResolverObjectNode node -> node.Value
        | ResolverListNode l -> l.Value
and ResolverLeaf = { Name: string; Value: obj option }
and ResolverError = { Name: string; Message: string; PathToOrigin: obj list; }
and ResolverNode = { Name: string; Value: obj option; Children: ResolverTree []; }

module ResolverTree =
    let fold leafOp errorOp nodeOp listOp =
        let rec helper = function
            | ResolverLeaf leaf ->
                leafOp leaf
            | ResolverError err ->
                errorOp err
            | ResolverObjectNode node ->
                let ts = node.Children |> Array.map(helper)
                nodeOp (node.Name, node.Value, ts)
            | ResolverListNode node ->
                let ts = node.Children |> Array.map(helper)
                listOp (node.Name, node.Value, ts)
        helper

    let rec pathFold leafOp errorOp nodeOp listOp =
        let rec helper path = function
            | ResolverLeaf leaf ->
                leafOp ((leaf.Name :> obj)::path) leaf
            | ResolverError err ->
                let origin = (err.PathToOrigin |> List.rev)
                let path' = if err.Name <> "__index" then origin@((err.Name :> obj)::path) else origin@path
                errorOp path' err
            | ResolverObjectNode node ->
                let path' = if node.Name <> "__index" then (node.Name :> obj)::path else path
                let ts = node.Children |> Array.map(helper path')
                nodeOp path' node.Name node.Value ts
            | ResolverListNode node ->
                let path' = (node.Name :> obj)::path
                let ts = node.Children |> Array.mapi(fun i c -> helper ((i :> obj)::path') c)
                listOp path' node.Name node.Value ts
        helper []

let private treeToDict =
    ResolverTree.pathFold
        (fun _ leaf -> KeyValuePair<_,_>(leaf.Name, match leaf.Value with | Some v -> v | None -> null), [])
        (fun path error ->
            let (e:Error) = (error.Message, path |> List.rev)
            KeyValuePair<_,_>(error.Name, null :> obj), [e])
        (fun _ name value children ->
            let dicts, errors = children |> Array.fold(fun (kvps, errs) (c, e) -> c::kvps, e@errs) ([], [])
            match value with
            | Some _ -> KeyValuePair<_,_>(name, NameValueLookup(dicts |> List.rev |> List.toArray) :> obj), errors
            | None -> KeyValuePair<_,_>(name, null), errors)
        (fun _ name value children ->
            let dicts, errors = children |> Array.fold(fun (kvps, errs) (c, e) -> c::kvps, e@errs) ([], [])
            match value with
            | Some _ -> KeyValuePair<_,_>(name, dicts |> List.map(fun d -> d.Value) |> List.rev |> List.toArray :> obj), errors
            | None -> KeyValuePair<_,_>(name, null), errors)

let private errorDict tree message path =
    let data, err = treeToDict tree
    (data, (message, path) :: err)

let private nullResolverError name = asyncVal { return ResolverError{ Name = name; Message = sprintf "Non-Null field %s resolved as a null!" name; PathToOrigin = []} }

let private propagateError name err = asyncVal { return ResolverError{ Name = name; Message = err.Message; PathToOrigin = (err.Name :> obj)::err.PathToOrigin} }

/// Builds the result tree for a given query
let rec private buildResolverTree (returnDef: OutputDef) (ctx: ResolveFieldContext) (fieldExecuteMap: FieldExecuteMap) (value: obj option) : AsyncVal<ResolverTree> =
    let name = ctx.ExecutionInfo.Identifier
    let filterDeferredTypeMap map =
        let filter info =
            if not ctx.ExecutionInfo.IsDeferred && info.IsDeferred
            then info.ResolveDeferred ()
            else info
        map |> Map.map (fun _ v ->  v |> List.map filter)
    let resolve kind  =
        match returnDef with
        | Object objdef ->
            match kind with
            | SelectFields fields ->
                match value with
                | Some v -> buildObjectFields fields objdef ctx fieldExecuteMap name v
                | None ->
                    if ctx.ExecutionInfo.IsNullable
                    then asyncVal { return ResolverObjectNode { Name = name; Value = None; Children = [| |] } }
                    else nullResolverError name
            | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind
        | Scalar scalardef ->
            let name = ctx.ExecutionInfo.Identifier
            let (coerce: obj -> obj option) = scalardef.CoerceValue
            asyncVal {
                return ResolverLeaf { Name = name; Value = value |> Option.bind(coerce) }
            }
        | Enum _ ->
            let name = ctx.ExecutionInfo.Identifier
            asyncVal {
                let value' = value |> Option.bind(fun v ->  coerceStringValue v |> Option.map(fun v' -> v' :> obj))
                return ResolverLeaf { Name = name; Value = value' }
            }
        | List (Output innerdef) ->
            let innerCtx =
                match kind with
                | ResolveCollection innerPlan -> { ctx with ExecutionInfo = innerPlan }
                | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind
            let rec build acc (items: obj list) =
                match items with
                | value::xs ->
                        if not innerCtx.ExecutionInfo.IsNullable && isNull value
                        then nullResolverError innerCtx.ExecutionInfo.Identifier
                        else
                            asyncVal {
                                let! tree = buildResolverTree innerdef innerCtx fieldExecuteMap (toOption value)
                                let! res =
                                    match tree with
                                    | ResolverError e when not innerCtx.ExecutionInfo.IsNullable -> propagateError name e
                                    | t -> build (t::acc) xs
                                return res
                            }
                | [] -> asyncVal { return ResolverListNode { Name = name; Value = value; Children = acc |> List.rev |> List.toArray } }
            match value with
            | None when not ctx.ExecutionInfo.IsNullable -> nullResolverError name
            | None -> asyncVal { return ResolverListNode { Name = name; Value = None; Children = [| |]; } }
            | ObjectOption (:? System.Collections.IEnumerable as enumerable) ->
                enumerable
                |> Seq.cast<obj>
                |> Seq.toList
                |> build []
            | _ -> raise <| GraphQLException (sprintf "Expected to have enumerable value in field '%s' but got '%O'" ctx.ExecutionInfo.Identifier (value.GetType()))
        | Nullable (Output innerdef) ->
            // Stop propagation of null values
            buildResolverTree innerdef ctx fieldExecuteMap value
        | Interface idef ->
            let possibleTypesFn = ctx.Schema.GetPossibleTypes
            let resolver = resolveInterfaceType possibleTypesFn idef
            let typeMap =
                match kind with
                | ResolveAbstraction typeMap -> filterDeferredTypeMap typeMap
                | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind
            match value with
            | Some v ->
                let resolvedDef = resolver v
                match Map.tryFind resolvedDef.Name typeMap with
                | Some fields -> buildObjectFields fields resolvedDef ctx fieldExecuteMap name v
                | None -> asyncVal { return ResolverError { Name = name; Message = ctx.Schema.ParseError (GraphQLException (sprintf "GraphQL Interface '%s' is not implemented by the type '%s'" idef.Name resolvedDef.Name)); PathToOrigin = [] } }
            | None ->
                if ctx.ExecutionInfo.IsNullable
                then asyncVal { return ResolverObjectNode { Name = name; Value = None; Children = [| |] } }
                else nullResolverError name
        | Union udef ->
            let possibleTypesFn = ctx.Schema.GetPossibleTypes
            let resolver = resolveUnionType possibleTypesFn udef
            let typeMap =
                match kind with
                | ResolveAbstraction typeMap -> filterDeferredTypeMap typeMap
                | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind
            match value with
            | Some v ->
                let resolvedDef = resolver v
                match Map.tryFind resolvedDef.Name typeMap with
                | Some fields ->
                    // Make sure to propagate the original union type to the object node
                    buildObjectFields fields resolvedDef ctx fieldExecuteMap name (udef.ResolveValue v)
                    |> AsyncVal.map(fun tree ->
                        match tree with
                        | ResolverObjectNode node ->  ResolverObjectNode { node with Value = value }
                        | t -> t)
                | None -> asyncVal { return ResolverError { Name = name; Message = ctx.Schema.ParseError (GraphQLException (sprintf "GraphQL Union '%s' is not implemented by the type '%s'" udef.Name resolvedDef.Name)); PathToOrigin = [] } }
            | None ->
                if ctx.ExecutionInfo.IsNullable
                then asyncVal { return ResolverObjectNode { Name = name; Value = None; Children = [| |] } }
                else nullResolverError name
        | _ -> failwithf "Unexpected value of returnDef: %O" returnDef
    match ctx.ExecutionInfo.Kind, returnDef, ctx.ExecutionInfo.IsDeferred with
    | ResolveDeferred _, (Scalar _ | Enum _ | Nullable _), false -> asyncVal { return ResolverLeaf { Name = name; Value = None } }
    | ResolveDeferred _, (Object _ | Interface _ | Union _), false -> asyncVal { return ResolverObjectNode { Name = name; Value = None; Children = [| |] } }
    | ResolveDeferred _, List _, false -> asyncVal { return ResolverListNode { Name = name; Value = Some (upcast [ ]); Children = [| |] } }
    | _ -> resolve ctx.ExecutionInfo.Kind

and buildObjectFields (fields: ExecutionInfo list) (objdef: ObjectDef) (ctx: ResolveFieldContext) (fieldExecuteMap: FieldExecuteMap) (name: string) (value: obj): AsyncVal<ResolverTree> =
    let rec build (acc: ResolverTree list) = function
        | info::xs ->
            let argDefs = fieldExecuteMap.GetArgs(objdef.Name, info.Definition.Name)
            let fieldCtx = createFieldContext objdef argDefs ctx info
            let execute = fieldExecuteMap.GetExecute(objdef.Name, info.Definition.Name)
            resolveField execute fieldCtx value
            |> AsyncVal.bind (buildResolverTree info.ReturnDef fieldCtx fieldExecuteMap)
            |> AsyncVal.rescue (fun e -> ResolverError { Name = info.Identifier; Message = ctx.Schema.ParseError e; PathToOrigin = []})
            |> AsyncVal.bind (fun tree ->
                match tree with
                | ResolverError e when not info.IsNullable -> propagateError name e
                | _ when not info.IsNullable && tree.Value.IsNone -> asyncVal { return ResolverError { Name = name; Message = ctx.Schema.ParseError(GraphQLException (sprintf "Non-Null field %s resolved as a null!" info.Identifier)); PathToOrigin = [info.Identifier]}}
                | t -> build (t::acc) xs)
        | [] -> asyncVal { return ResolverObjectNode { Name = name; Value = Some value; Children = acc |> List.rev |> List.toArray } }
    build [] fields

let internal compileSubscriptionField (subfield: SubscriptionFieldDef) = 
    match subfield.Resolve with
    | Resolve.BoxedFilterExpr(_, _, _, filter) -> fun ctx a b -> filter ctx a b |> AsyncVal.wrap |> AsyncVal.toAsync
    | Resolve.BoxedAsyncFilterExpr(_, _, _, filter) -> filter
    | _ -> raise <| GraphQLException ("Invalid filter expression for subscription field!")

let internal compileField (fieldDef: FieldDef) : ExecuteField =
    match fieldDef.Resolve with
    | Resolve.BoxedSync(_, _, resolve) ->
        fun resolveFieldCtx value ->
            try
                resolve resolveFieldCtx value
                |> AsyncVal.wrap
            with e -> AsyncVal.Failure(e)

    | Resolve.BoxedAsync(_, _, resolve) ->
        fun resolveFieldCtx value ->
            asyncVal {
                return! resolve resolveFieldCtx value
            }
    | Resolve.BoxedExpr (resolve) ->
        fun resolveFieldCtx value ->
            downcast resolve resolveFieldCtx value
    | _ ->
        fun _ _ -> raise (InvalidOperationException(sprintf "Field '%s' has been accessed, but no resolve function for that field definition was provided. Make sure, you've specified resolve function or declared field with Define.AutoField method" fieldDef.Name))

let private (|String|Other|) (o : obj) =
    match o with
    | :? string as s -> String s
    | _ -> Other

let formatErrors (errors : Error list) =
    errors 
    |> List.map (fun err ->
        let (message, path) = err
        NameValueLookup.ofList ["message", upcast message; "path", upcast path])

let private executeQueryOrMutation (resultSet: (string * ExecutionInfo) []) (ctx: ExecutionContext) (objdef: ObjectDef) (fieldExecuteMap: FieldExecuteMap) value =
    let resultTrees =
        resultSet
        |> Array.map (fun (name, info) ->
            let fdef = info.Definition
            let argDefs = fieldExecuteMap.GetArgs(ctx.ExecutionPlan.RootDef.Name, info.Definition.Name)
            let args = getArgumentValues argDefs info.Ast.Arguments ctx.Variables
            let fieldCtx =
                { ExecutionInfo = info
                  Context = ctx
                  ReturnType = fdef.TypeDef
                  ParentType = objdef
                  Schema = ctx.Schema
                  Args = args
                  Variables = ctx.Variables }
            let execute = fieldExecuteMap.GetExecute(ctx.ExecutionPlan.RootDef.Name, info.Definition.Name)
            execute fieldCtx value
            |> AsyncVal.bind(fun r -> buildResolverTree info.ReturnDef fieldCtx fieldExecuteMap (toOption r))
            |> AsyncVal.rescue(fun e -> ResolverError { Name = name; Message = ctx.Schema.ParseError e; PathToOrigin = []}))
    let dict =
        asyncVal {
            let! trees =
                match ctx.ExecutionPlan.Strategy with
                | ExecutionStrategy.Parallel -> resultTrees |> AsyncVal.collectParallel
                | ExecutionStrategy.Sequential -> resultTrees |> AsyncVal.collectSequential
            let dicts, errors =
                trees
                |> Array.fold(fun (kvps, errs) (tree) ->
                    let k, e = treeToDict tree
                    k::kvps, e@errs) ([],[])
            return NameValueLookup(dicts |> List.rev |> List.toArray), (errors |> List.rev)
        }
    let rec traversePath (d : DeferredExecutionInfo) (fieldCtx : ResolveFieldContext) (path: obj list) (tree: AsyncVal<ResolverTree>) (pathAcc: obj list): AsyncVal<(ResolverTree * obj list) []> =
        let removeDuplicatedIndexes (path : obj list) =
            let value = Some ("__index" :> obj)
            let rec remove (path : obj list) last =
                match path with
                | [] -> []
                | x :: xs when last = Some x && last = value -> remove xs <| Some x
                | x :: xs -> x :: (remove xs <| Some x)
            remove path None
        asyncVal {
            let! tree' = tree
            let! res =
                match List.tail (removeDuplicatedIndexes path), tree' with
                | [], t ->
                    asyncVal {
                        let! res = buildResolverTree d.Info.ReturnDef fieldCtx fieldExecuteMap t.Value
                        match d.Info.Kind with
                        | SelectFields [f] -> return! async { return [|res, List.rev (f.Identifier :> obj :: pathAcc)|] }
                        | _ -> return! async { return [|res, List.rev ((List.head path)::pathAcc)|] }
                    }
                | [String p], t ->
                    asyncVal {
                        let! res = buildResolverTree d.Info.ReturnDef fieldCtx fieldExecuteMap t.Value
                        match res with
                        | ResolverError _ -> return! async { return [||] }
                        | _ -> return! async { return [|res, List.rev((p :> obj)::pathAcc)|] }
                    }
                | ([p; String "__index"] | [p]), t ->
                    asyncVal {
                        let! res = buildResolverTree d.Info.ReturnDef fieldCtx fieldExecuteMap t.Value
                        return! async { return [|res, List.rev(p::pathAcc)|] }
                    }
                | [head'; String "__index"; head; String "__index"] as p, ResolverObjectNode n ->
                    asyncVal {
                        let next = n.Children |> Array.tryFind(fun c' -> c'.Name = head.ToString())
                        let! res =
                            match next with
                            | Some next' -> traversePath d fieldCtx p (AsyncVal.wrap next') (head'::pathAcc)
                            | None -> AsyncVal.empty
                        return res
                    }
                | p, ResolverObjectNode n ->
                    asyncVal {
                        let head = p |> List.head
                        let next = n.Children |> Array.tryFind(fun c' -> c'.Name = head.ToString())
                        let! res =
                            match next with
                            | Some next' -> traversePath d fieldCtx p (AsyncVal.wrap next') (head::pathAcc)
                            | None -> AsyncVal.empty
                        return res
                    }
                | p, ResolverListNode l ->
                    asyncVal {
                        let! res =
                            l.Children
                            |> Array.mapi(fun i c ->
                                traversePath d fieldCtx p (AsyncVal.wrap c) (i :> obj :: pathAcc))
                            |> AsyncVal.collectParallel
                        return res |> Array.fold (Array.append) [||]
                    }
                | _ ,_ -> raise <| GraphQLException("Path terminated unexpectedly!")
            return res
        }
    let nvli (path : obj list) (index : int) (err : Error list) data =
        let data' = [ data ] :> obj
        match err with
        | [] -> NameValueLookup.ofList ["data", data'; "path", upcast (path @ [index])]
        | _ -> NameValueLookup.ofList ["data", data'; "errors", upcast (formatErrors err); "path", upcast (path @ [index])]
    let nvl (path : obj list) (err : Error list) data =
        match err with
        | [] -> NameValueLookup.ofList ["data", data; "path", upcast path]
        | _ -> NameValueLookup.ofList ["data", data; "errors", upcast (formatErrors err); "path", upcast path]
    let mapResult (d : KeyValuePair<string, obj>) (e : Error list) (path : obj list) (kind : DeferredExecutionInfoKind) =
        match kind with
        | DeferredExecution | LiveExecution ->
            match d.Value, e with
            | null, [] -> Seq.empty
            | :? NameValueLookup as x, _ -> x |> Seq.map (fun x -> nvl path e x.Value)
            | x, _ -> nvl path e x |> Seq.singleton
        | StreamedExecution ->
            match d.Value, e with
            | null, [] -> Seq.empty
            | :? NameValueLookup as x, _ ->
                x |> Seq.map (fun x ->
                    match x.Value with
                    | :? IEnumerable<obj> as x -> x |> Seq.mapi (fun i d -> nvli path i e d)
                    | _ -> nvl path e x.Value |> Seq.singleton)
                  |> Seq.collect id
            | x, _ -> nvl path e x |> Seq.singleton
    let mapLiveResult (tree : ResolverTree) (path : obj list) (d : DeferredExecutionInfo) (fieldCtx : ResolveFieldContext) =
        let getFieldName (node : ResolverNode) =
            match node.Children |> Array.tryHead with
            | Some c -> c.Name
            | None -> failwithf "Expected a child for the object %A, but got none." node.Value
        let rec getObjectName (returnDef : OutputDef) (value : obj) (possibleTypesFn : TypeDef -> ObjectDef []) =
            match returnDef with
            | Object objdef -> objdef.Name
            | Scalar scalardef -> scalardef.Name
            | Enum enumdef -> enumdef.Name
            | Nullable (Output innerdef) -> getObjectName innerdef value possibleTypesFn
            | Interface idef -> idef.Name
            | Union udef ->
                let resolver = resolveUnionType possibleTypesFn udef
                match toOption value with
                | Some v -> getObjectName (resolver v) value possibleTypesFn
                | None -> failwithf "Unexpected value of returnDef: %O" returnDef
            | _ -> failwithf "Unexpected value of returnDef: %O" returnDef
        match d.Kind with
        | LiveExecution ->
            match tree with
            | ResolverObjectNode node ->
                let value = tree.Value
                let typeName = getObjectName d.Info.ReturnDef value ctx.Schema.GetPossibleTypes
                let fieldName = getFieldName node
                let provider = ctx.Schema.LiveFieldSubscriptionProvider
                let identity = provider.TryFind typeName fieldName |> Option.map (fun x -> x.Identity)
                match identity, toOption value with
                | Some identity, Some value ->
                    provider.Add (identity value) typeName fieldName
                    |> Observable.map (fun v ->
                        let tree = AsyncVal.get (buildResolverTree d.Info.ReturnDef fieldCtx fieldExecuteMap (Some v))
                        let data, err = treeToDict tree
                        mapResult data err path d.Kind)
                    |> Observable.toSeq
                    |> Seq.concat
                | _ -> Seq.empty
            | _ -> Seq.empty
        | _ -> Seq.empty
    let rec deferredResult (tree : ResolverTree) (d : DeferredExecutionInfo) =
        let fdef = d.Info.Definition
        let args = getArgumentValues fdef.Args d.Info.Ast.Arguments ctx.Variables
        let fieldCtx =
            { ExecutionInfo = d.Info
              Context = ctx
              ReturnType = fdef.TypeDef
              ParentType = objdef
              Schema = ctx.Schema
              Args = args
              Variables = ctx.Variables }
        let path = d.Path |> List.map box
        let traversed = traversePath d fieldCtx path (AsyncVal.wrap tree) [ List.head path ]
        traversed
        |> AsyncVal.bind (Array.map (fun (tree, path) ->
            let outerResult =
                asyncVal {
                    let data, err = treeToDict tree
                    let deferred = 
                        match d.Kind with
                        | LiveExecution -> Seq.empty
                        | _ -> mapResult data err path d.Kind
                    let live = mapLiveResult tree path d fieldCtx
                    return Seq.append deferred live
                } |> Array.singleton |> AsyncVal.collectParallel
            let innerResult =
                d.DeferredFields
                |> List.map (fun d ->
                    let fieldCtx = { fieldCtx with ExecutionInfo = d.Info }
                    let path = d.Path |> List.rev |> List.map (fun x -> x :> obj)
                    traversePath d fieldCtx path (AsyncVal.wrap tree) [ List.head path ]
                    |> AsyncVal.bind(Array.map(fun (tree, path) ->
                        asyncVal {
                            let data, err =
                                if d.DeferredFields.Length > 0
                                then errorDict tree "Maximum degree of nested deferred executions reached." path
                                else treeToDict tree
                            let deferred = 
                                match d.Kind with
                                | LiveExecution -> Seq.empty
                                | _ -> mapResult data err path d.Kind
                            let live = mapLiveResult tree path d fieldCtx
                            return Seq.append deferred live
                        }) >> AsyncVal.collectParallel))
                |> Array.ofList
                |> AsyncVal.appendParallel
            [| outerResult; innerResult |]
            |> AsyncVal.appendParallel
        ) >> AsyncVal.appendParallel)
    let deferredResults =
        if ctx.ExecutionPlan.DeferredFields.Length = 0
        then None
        else
            resultTrees
            |> Array.map (AsyncVal.bind (fun tree ->
                ctx.ExecutionPlan.DeferredFields
                |> List.filter (fun d -> (List.head d.Path) = tree.Name)
                |> List.toArray
                |> Array.map (deferredResult tree)
                |> AsyncVal.collectParallel
                |> AsyncVal.map (Array.fold Array.append Array.empty)))
            |> AsyncVal.appendParallel
            |> AsyncVal.toAsync
            |> Observable.ofAsync
            |> Observable.bind Observable.ofSeq
            |> Observable.bind Observable.ofSeq
            |> Some
    dict, deferredResults

let private executeSubscription (resultSet: (string * ExecutionInfo) []) (ctx: ExecutionContext) (objdef: SubscriptionObjectDef) (fieldExecuteMap: FieldExecuteMap) (subscriptionProvider: ISubscriptionProvider) value =
    // Subscription queries can only have one root field
    let name, info = Array.head resultSet
    let subdef = info.Definition :?> SubscriptionFieldDef
    let args = getArgumentValues subdef.Args info.Ast.Arguments ctx.Variables
    let returnType = subdef.OutputTypeDef
    let fieldCtx =
        { ExecutionInfo = info
          Context = ctx
          ReturnType = returnType
          ParentType = objdef
          Schema = ctx.Schema
          Args = args
          Variables = ctx.Variables }
    subscriptionProvider.Add fieldCtx value subdef
    |> Observable.bind(fun v ->
        buildResolverTree returnType fieldCtx fieldExecuteMap (Some v)
        |> AsyncVal.map(treeToDict)
        |> AsyncVal.map(fun (data, err) -> 
            match err with
            | [] -> NameValueLookup.ofList["data", data.Value] :> Output
            | _ -> NameValueLookup.ofList["data", data.Value; "errors", upcast (formatErrors err)] :> Output)
        |> AsyncVal.toAsync
        |> Observable.ofAsync)

let private compileInputObject (indef: InputObjectDef) =
    indef.Fields
    |> Array.iter(fun input ->
        let errMsg = sprintf "Input object '%s': in field '%s': " indef.Name input.Name
        input.ExecuteInput <- compileByType errMsg input.TypeDef)

let private compileObject (objdef: ObjectDef) (executeFields: FieldDef -> unit) =
    objdef.Fields
    |> Map.iter (fun _ fieldDef ->
        executeFields fieldDef
        fieldDef.Args
        |> Array.iter (fun arg ->
            let errMsg = sprintf "Object '%s': field '%s': argument '%s': " objdef.Name fieldDef.Name arg.Name
            arg.ExecuteInput <- compileByType errMsg arg.TypeDef))

let internal compileSchema (ctx : SchemaCompileContext) =
    ctx.Schema.TypeMap.ToSeq()
    |> Seq.iter (fun (tName, x) ->
        match x with
        | SubscriptionObject subdef ->
            compileObject subdef (fun sub ->
                // Subscription Objects only contain subscription fields, so this cast is safe
                let subField = (sub :?> SubscriptionFieldDef)
                let filter = compileSubscriptionField subField
                let subscription = { Name = subField.Name ; Filter = filter }
                ctx.Schema.SubscriptionProvider.Register subscription)
        | Object objdef ->
            compileObject objdef (fun fieldDef -> ctx.FieldExecuteMap.SetExecute(tName, fieldDef))
        | InputObject indef -> compileInputObject indef
        | _ -> ())

let internal coerceVariables (variables: VarDef list) (vars: Map<string, obj>) =
    variables
    |> List.fold (fun acc vardef -> Map.add vardef.Name (coerceVariable vardef vars) acc) Map.empty

let internal executeOperation (ctx : ExecutionContext) : AsyncVal<GQLResponse> =
    let resultSet =
        ctx.ExecutionPlan.Fields
        |> List.filter (fun info -> info.Include ctx.Variables)
        |> List.map (fun info -> (info.Identifier, info))
        |> List.toArray
    let parseQuery o =
        let dict, deferred = executeQueryOrMutation resultSet ctx o ctx.FieldExecuteMap ctx.RootValue
        match deferred with
        | Some d -> dict |> AsyncVal.map(fun (dict', errors') -> GQLResponse.Deferred(dict', errors', d |> Observable.map(fun x -> upcast x), ctx.Metadata))
        | None -> dict |> AsyncVal.map(fun (dict', errors') -> GQLResponse.Direct(dict', errors', ctx.Metadata))
    match ctx.ExecutionPlan.Operation.OperationType with
    | Query -> parseQuery ctx.Schema.Query
    | Mutation ->
        match ctx.Schema.Mutation with
        | Some m ->
            parseQuery m
        | None -> raise(InvalidOperationException("Attempted to make a mutation but no mutation schema was present!"))
    | Subscription ->
        match ctx.Schema.Subscription with
        | Some s ->
            AsyncVal.wrap(GQLResponse.Stream(executeSubscription resultSet ctx s ctx.FieldExecuteMap ctx.Schema.SubscriptionProvider ctx.RootValue, ctx.Metadata))
        | None -> raise(InvalidOperationException("Attempted to make a subscription but no subscription schema was present!"))