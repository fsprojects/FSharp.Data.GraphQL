/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Execution

open System
open System.Reflection
open System.Reactive
open System.Reactive.Linq
open System.Runtime.InteropServices;
open System.Collections.Generic
open System.Collections.Concurrent
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Planning
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Introspection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Reflection.FSharpReflectionExtensions
open System.Collections.Generic

type Error = string * string list
type Output = IDictionary<string, obj>
type GQLResponse =
    | Direct of data:Output * errors: Error list
    | Deferred of data:Output * errors:Error list * defer:IObservable<Output>
    | Stream of stream:IObservable<Output>
    
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
        elif x.Count <> y.Count then false
        else
            x.Buffer
            |> Array.forall2 (fun (a: KeyValuePair<string, obj>) (b: KeyValuePair<string, obj>) ->
                if a.Key <> b.Key then false
                else 
                    match a.Value, b.Value with
                    | :? NameValueLookup, :? NameValueLookup as o -> structEq (downcast fst o) (downcast snd o)
                    | :? seq<obj>, :? seq<obj> -> Seq.forall2 (=) (a.Value :?> seq<obj>) (b.Value :?> seq<obj>)
                    | a1, b1 -> a1 = b1) y.Buffer
    let pad (sb: System.Text.StringBuilder) times = for i in 0..times do sb.Append("\t") |> ignore
    let rec stringify (sb: System.Text.StringBuilder) deep (o:obj) =
        match o with
        | :? NameValueLookup as lookup ->
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
                stringify sb (deep+1) i
                sb.Append(", ") |> ignore
            sb.Append("]") |> ignore
        | other -> 
            if other <> null 
            then sb.Append(other.ToString()) |> ignore
            else sb.Append("null") |> ignore
        ()
    /// Create new NameValueLookup from given list of key-value tuples.
    static member ofList (l: (string * obj) list) = NameValueLookup(l)
    /// Returns raw content of the current lookup.
    member private x.Buffer : KeyValuePair<string, obj> [] = kvals
    /// Return a number of entries stored in current lookup. It's fixed size.
    member x.Count = kvals.Length
    /// Updates an entry's value under given key. It will throw an exception
    /// if provided key cannot be found in provided lookup.
    member x.Update key value = setValue key value
    override x.Equals(other) = 
        match other with
        | :? NameValueLookup as lookup -> structEq x lookup
        | _ -> false
    override x.GetHashCode() =
        let mutable hash = 0
        for kv in kvals do
            hash <- (hash*397) ^^^ (kv.Key.GetHashCode()) ^^^ (if kv.Value = null then 0 else kv.Value.GetHashCode())
        hash
    override x.ToString() = 
        let sb =Text.StringBuilder()
        stringify sb 1 x
        sb.ToString()
    interface IEquatable<NameValueLookup> with
        member x.Equals(other) = structEq x other
    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (kvals :> System.Collections.IEnumerable).GetEnumerator()
    interface IEnumerable<KeyValuePair<string, obj>> with
        member x.GetEnumerator() = (kvals :> IEnumerable<KeyValuePair<string, obj>>).GetEnumerator()
    interface IDictionary<string, obj> with
        member x.Add(_, _) = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member x.Add(_) = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member x.Clear() = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member x.Contains(item) = kvals |> Array.exists ((=) item)
        member x.ContainsKey(key) = kvals |> Array.exists (fun kv -> kv.Key = key)
        member x.CopyTo(array, arrayIndex) = kvals.CopyTo(array, arrayIndex)
        member x.Count = x.Count
        member x.IsReadOnly = true
        member x.Item
            with get (key) = getValue key
            and set (key) v = setValue key v
        member x.Keys = upcast (kvals |> Array.map (fun kv -> kv.Key))
        member x.Values = upcast (kvals |> Array.map (fun kv -> kv.Value))
        member x.Remove(_:string) = 
            raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
            false
        member x.Remove(_:KeyValuePair<string,obj>) = 
            raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
            false
        member x.TryGetValue(key, value) = 
            match kvals |> Array.tryFind (fun kv -> kv.Key = key) with
            | Some kv -> value <- kv.Value; true
            | None -> value <- null; false
    new(t: (string * obj) list) = 
        NameValueLookup(t |> List.map (fun (k, v) -> KeyValuePair<string,obj>(k, v)) |> List.toArray)
    new(t: string []) = 
        NameValueLookup(t |> Array.map (fun k -> KeyValuePair<string,obj>(k, null)))
                
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

let private createFieldContext objdef ctx (info: ExecutionInfo) =
    let fdef = info.Definition
    let args = getArgumentValues fdef.Args info.Ast.Arguments ctx.Variables
    { ExecutionInfo = info
      Context = ctx.Context
      ReturnType = fdef.TypeDef
      ParentType = objdef
      Schema = ctx.Schema
      Args = args
      Variables = ctx.Variables }         
                

let private optionCast (value: obj) =
            let optionDef = typedefof<option<_>>
            if isNull value then None
            else
                let t = value.GetType()
                let v' = t.GetProperty("Value")
#if NETSTANDARD2_0           
                if t.GetTypeInfo().IsGenericType && t.GetTypeInfo().GetGenericTypeDefinition() = optionDef then
#else       
                if t.IsGenericType && t.GetGenericTypeDefinition() = optionDef then
#endif                  
                    Some(v'.GetValue(value, [| |]))
                else None

let private (|SomeObj|_|) = optionCast

let private resolveField (execute: ExecuteField) (ctx: ResolveFieldContext) (parentValue: obj) =
    if ctx.ExecutionInfo.IsNullable
    then
        execute ctx parentValue 
        |> AsyncVal.map(optionCast)
    else 
        execute ctx parentValue
        |> AsyncVal.map(fun v -> if isNull v then None else Some v)

/// Lifts an object to an option unless it is already an option
let private toOption x = 
    match x with
    | SomeObj(v)
    | v -> Some(v)

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
        | ResolverError err -> None
        | ResolverObjectNode node -> node.Value
        | ResolverListNode l -> l.Value
and ResolverLeaf = { Name: string; Value: obj option; }
and ResolverError = { Name: string; Message: string; PathToOrigin: string list; }
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
                leafOp (leaf.Name::path) leaf
            | ResolverError err ->
                let origin = (err.PathToOrigin |> List.rev)
                let path' = if err.Name <> "__index" then origin@(err.Name::path) else origin@path
                errorOp path' err
            | ResolverObjectNode node ->
                let path' = if node.Name <> "__index" then node.Name::path else path
                let ts = node.Children |> Array.map(helper path')
                nodeOp path' node.Name node.Value ts
            | ResolverListNode node ->
                let path' = node.Name::path
                let ts = node.Children |> Array.mapi(fun i c -> helper (i.ToString()::path') c)
                listOp path' node.Name node.Value ts
        helper []

let private treeToDict =
    ResolverTree.pathFold 
        (fun path leaf -> KeyValuePair<_,_>(leaf.Name, match leaf.Value with | Some v -> v | None -> null), [])
        (fun path error -> 
            let (e:Error) = (error.Message, path |> List.rev)
            KeyValuePair<_,_>(error.Name, null :> obj), [e])
        (fun path name value children -> 
            let dicts, errors = children |> Array.fold(fun (kvps, errs) (c, e) -> c::kvps, e@errs) ([], [])
            match value with
            | Some v -> KeyValuePair<_,_>(name, NameValueLookup(dicts |> List.rev |> List.toArray) :> obj), errors
            | None -> KeyValuePair<_,_>(name, null), errors)
        (fun path name value children -> 
            let dicts, errors = children |> Array.fold(fun (kvps, errs) (c, e) -> c::kvps, e@errs) ([], [])
            match value with
            | Some v -> KeyValuePair<_,_>(name, dicts |> List.map(fun d -> d.Value) |> List.rev |> List.toArray :> obj), errors
            | None -> KeyValuePair<_,_>(name, null), errors)

let private nullResolverError name = asyncVal { return ResolverError{ Name = name; Message = sprintf "Non-Null field %s resolved as a null!" name; PathToOrigin = []} }
let private propagateError name err = asyncVal { return ResolverError{ Name = name; Message = err.Message; PathToOrigin = err.Name::err.PathToOrigin} }

/// Builds the result tree for a given query
let rec private buildResolverTree (returnDef: OutputDef) (ctx: ResolveFieldContext) (fieldExecuteMap: FieldExecuteMap) (value: obj option) : AsyncVal<ResolverTree> =
    let name = ctx.ExecutionInfo.Identifier
    match returnDef with
    | Object objdef ->
        match ctx.ExecutionInfo.Kind with
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
    | Enum enumdef ->
        let name = ctx.ExecutionInfo.Identifier
        asyncVal {
            let value' = value |> Option.bind(fun v ->  coerceStringValue v |> Option.map(fun v' -> v' :> obj))
            return ResolverLeaf { Name = name; Value = value' }
        }
    | List (Output innerdef) ->
        let innerCtx = 
            match ctx.ExecutionInfo.Kind with
            | ResolveCollection innerPlan -> { ctx with ExecutionInfo = innerPlan}
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
            | [] -> asyncVal{ return ResolverListNode{ Name = name; Value = value; Children = acc |> List.rev |> List.toArray }}
        match value with
        | None when not ctx.ExecutionInfo.IsNullable -> nullResolverError name
        | None -> asyncVal{ return ResolverListNode{ Name = name; Value = None; Children = [| |]; } }
        | SomeObj(:? System.Collections.IEnumerable as enumerable) ->
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
            match ctx.ExecutionInfo.Kind with
            | ResolveAbstraction typeMap -> typeMap
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
            match ctx.ExecutionInfo.Kind with
            | ResolveAbstraction typeMap -> typeMap
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


and buildObjectFields (fields: ExecutionInfo list) (objdef: ObjectDef) (ctx: ResolveFieldContext) (fieldExecuteMap: FieldExecuteMap) (name: string) (value: obj): AsyncVal<ResolverTree> =
    let rec build (acc: ResolverTree list) = function
        | info::xs ->
            let fieldCtx = createFieldContext objdef ctx info
            let execute = fieldExecuteMap.GetExecute(objdef.Name, info.Definition.Name)
            resolveField execute fieldCtx value
            |> AsyncVal.bind(buildResolverTree info.ReturnDef fieldCtx fieldExecuteMap)
            |> AsyncVal.rescue(fun e -> ResolverError{ Name = info.Identifier; Message = ctx.Schema.ParseError e; PathToOrigin = []}) 
            |> AsyncVal.bind(fun tree ->
                match tree with
                | ResolverError e when not info.IsNullable -> propagateError name e
                | t when not info.IsNullable && tree.Value.IsNone -> asyncVal { return ResolverError { Name = name; Message = ctx.Schema.ParseError(GraphQLException (sprintf "Non-Null field %s resolved as a null!" info.Identifier)); PathToOrigin = [info.Identifier]}} 
                | t -> build (t::acc) xs)
        | [] -> asyncVal { return ResolverObjectNode{ Name = name; Value = Some value; Children = acc |> List.rev |> List.toArray } }
    build [] fields

let internal compileSubscriptionField (subfield: SubscriptionFieldDef) = 
    match subfield.Resolve with
    | Resolve.BoxedFilterExpr(_, _, filter) -> filter
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

let private getFieldDefinition (ctx: ExecutionContext) (objectType: ObjectDef) (field: Field) : FieldDef option =
        match field.Name with
        | "__schema" when Object.ReferenceEquals(ctx.Schema.Query, objectType) -> Some (upcast SchemaMetaFieldDef)
        | "__type" when Object.ReferenceEquals(ctx.Schema.Query, objectType) -> Some (upcast TypeMetaFieldDef)
        | "__typename" -> Some (upcast TypeNameMetaFieldDef)
        | fieldName -> objectType.Fields |> Map.tryFind fieldName
        
let private executeQueryOrMutation (resultSet: (string * ExecutionInfo) []) (ctx: ExecutionContext) (objdef: ObjectDef) (fieldExecuteMap: FieldExecuteMap) value =
    let resultTrees =
        resultSet
        |> Array.map (fun (name, info) ->
            let fdef = info.Definition
            let args = getArgumentValues fdef.Args info.Ast.Arguments ctx.Variables
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
            |> AsyncVal.rescue(fun e -> ResolverError{ Name = name; Message = ctx.Schema.ParseError e; PathToOrigin = []}))
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
    let rec traversePath (d : DeferredExecutionInfo) (fieldCtx : ResolveFieldContext) (path: string list) (tree: AsyncVal<ResolverTree>) (pathAcc: string list): AsyncVal<(ResolverTree * string list) []> =
        asyncVal {
            let! tree' = tree
            let! res = 
                match List.tail path, tree' with
                | [], t -> 
                    asyncVal { 
                        let! res = buildResolverTree d.Info.ReturnDef fieldCtx fieldExecuteMap t.Value
                        return! async { return [|res, List.rev ((List.head path)::pathAcc)|] }
                    }
                | [p], t -> 
                    asyncVal { 
                        let! res = buildResolverTree d.Info.ReturnDef fieldCtx fieldExecuteMap t.Value
                        return! async { return [|res, List.rev(p::pathAcc)|] }
                    }
                | [p;"__index"], t -> 
                    asyncVal { 
                        let! res = buildResolverTree d.Info.ReturnDef fieldCtx fieldExecuteMap t.Value
                        return! async { return [|res, List.rev(p::pathAcc)|] }
                    }
                | p, ResolverObjectNode n -> 
                    asyncVal {
                        let next = n.Children |> Array.tryFind(fun c' -> c'.Name = (List.head p))
                        let! res =
                            match next with
                            | Some next' -> traversePath d fieldCtx p (AsyncVal.wrap next') ((List.head p)::pathAcc)
                            | None -> AsyncVal.empty
                        return res
                    }
                | p, ResolverListNode l ->
                    asyncVal {
                        let! res =
                            l.Children
                            |> Array.mapi(fun i c -> 
                                traversePath d fieldCtx p (AsyncVal.wrap c) (i.ToString()::pathAcc))
                            |> AsyncVal.collectParallel
                        return res |> Array.fold (Array.append) [||]
                    }
                | _ ,_ -> raise <| GraphQLException("Path terminated unexpectedly!")
            return res
        }
    let deferredResults =
        if ctx.ExecutionPlan.DeferredFields.Length = 0
        then None
        else
            resultTrees
            |> Array.map(AsyncVal.bind(fun tree ->
                ctx.ExecutionPlan.DeferredFields
                |> List.filter (fun d -> (List.head d.Path) = tree.Name)
                |> List.toArray
                |> Array.map(fun d ->
                    let fdef = d.Info.Definition
                    let args = getArgumentValues fdef.Args d.Info.Ast.Arguments ctx.Variables
                    let fieldCtx = { 
                        ExecutionInfo = d.Info
                        Context = ctx
                        ReturnType = fdef.TypeDef
                        ParentType = objdef
                        Schema = ctx.Schema
                        Args = args
                        Variables = ctx.Variables
                    }
                    traversePath d fieldCtx d.Path (AsyncVal.wrap tree) [(List.head d.Path)]
                    |> AsyncVal.bind(Array.map(fun (tree, path) ->
                        asyncVal {
                            // TODO: what to do with deferred errors?
                            let d, _ = treeToDict tree
                            return NameValueLookup.ofList["data", d.Value; "path", upcast path]
                        }) >> AsyncVal.collectParallel))
                |> AsyncVal.collectParallel
                |> AsyncVal.map (Array.fold Array.append Array.empty)))
            |> AsyncVal.collectParallel
            |> AsyncVal.map (Array.fold Array.append Array.empty)
            |> AsyncVal.toAsync
            |> Observable.ofAsync
            |> Observable.bind Observable.ofSeq
            |> Some
    let streamedResults =
        if ctx.ExecutionPlan.StreamedFields.Length = 0
        then None
        else
            resultTrees
            |> Array.map(AsyncVal.bind(fun tree ->
                ctx.ExecutionPlan.StreamedFields
                |> List.filter (fun d -> (List.head d.Path) = tree.Name)
                |> List.toArray
                |> Array.map(fun d ->
                    let fdef = d.Info.Definition
                    let args = getArgumentValues fdef.Args d.Info.Ast.Arguments ctx.Variables
                    let fieldCtx = { 
                        ExecutionInfo = d.Info
                        Context = ctx
                        ReturnType = fdef.TypeDef
                        ParentType = objdef
                        Schema = ctx.Schema
                        Args = args
                        Variables = ctx.Variables
                    }
                    let nvli (path : string list) (index : int) data =
                        let path' = path |> List.map (fun p -> p :> obj)
                        NameValueLookup.ofList ["data", data; "path", upcast (path' @ [index])]
                    let nvl (path : string list) data =
                        NameValueLookup.ofList ["data", data; "path", upcast path]
                    let mapResult (d : KeyValuePair<string, obj>) path =
                        match d.Value with
                        | :? NameValueLookup as x -> 
                            x
                            |> Seq.map (fun x -> 
                                match x.Value with
                                | :? IEnumerable<obj> as x -> x |> Seq.mapi (fun i d -> nvli path i d)
                                | _ -> nvl path x |> Seq.singleton)
                            |> Seq.collect id
                        | x -> nvl path x |> Seq.singleton
                    traversePath d fieldCtx d.Path (AsyncVal.wrap tree) [(List.head d.Path)]
                    |> AsyncVal.bind(Array.map(fun (tree, path) ->
                        asyncVal {
                            let d, _ = treeToDict tree
                            return mapResult d path
                        }) >> AsyncVal.collectParallel))
                |> AsyncVal.collectParallel
                |> AsyncVal.map (Array.fold Array.append Array.empty)))
            |> AsyncVal.collectParallel
            |> AsyncVal.map (Array.fold Array.append Array.empty)
            |> AsyncVal.toAsync
            |> Observable.ofAsync
            |> Observable.bind Observable.ofSeq
            |> Observable.bind Observable.ofSeq
            |> Some
    dict, deferredResults, streamedResults

let private executeSubscription (resultSet: (string * ExecutionInfo) []) (ctx: ExecutionContext) (objdef: SubscriptionObjectDef) (fieldExecuteMap: FieldExecuteMap) (subscriptionProvider: ISubscriptionProvider) value = 
    // Subscription queries can only have one root field
    let name, info = Array.head resultSet
    let subdef = info.Definition :?> SubscriptionFieldDef
    let args = getArgumentValues subdef.Args info.Ast.Arguments ctx.Variables
    let returnType = subdef.InputTypeDef
    let fieldCtx = 
        { ExecutionInfo = info
          Context = ctx
          ReturnType = returnType
          ParentType = objdef
          Schema = ctx.Schema
          Args = args
          Variables = ctx.Variables } 
    subscriptionProvider.Add fieldCtx value name 
    |> Observable.bind(fun v -> 
        buildResolverTree returnType fieldCtx fieldExecuteMap (Some v) 
        |> AsyncVal.map(treeToDict)
        |> AsyncVal.map(fun data -> NameValueLookup.ofList["data", upcast data] :> Output)
        |> AsyncVal.map(fun d -> printfn "Async dict %A" d;d)
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

let internal compileSchema types (fieldExecuteMap: FieldExecuteMap) (subscriptionProvider: ISubscriptionProvider) =
    types
    |> Map.toSeq 
    |> Seq.iter (fun (tName, x) ->
        match x with
        | SubscriptionObject subdef ->
            compileObject subdef (fun sub ->
                // Subscription Objects only contain subscription fields, so this cast is safe
                let subField = (sub :?> SubscriptionFieldDef)
                let filter = compileSubscriptionField subField
                let subscription = { Name = subField.Name ; Filter = filter }
                subscriptionProvider.Register subscription) 
        | Object objdef ->
            compileObject objdef (fun fieldDef -> fieldExecuteMap.SetExecute(tName, fieldDef.Name, compileField fieldDef))
        | InputObject indef -> compileInputObject indef
        | _ -> ())

let private coerceVariables (variables: VarDef list) (vars: Map<string, obj>) =
    variables
    |> List.fold (fun acc vardef -> Map.add vardef.Name (coerceVariable vardef vars) acc) Map.empty

let internal evaluate (schema: #ISchema) (executionPlan: ExecutionPlan) (variables: Map<string, obj>) (root: obj) errors (fieldExecuteMap: FieldExecuteMap) : AsyncVal<GQLResponse> =
    let variables = coerceVariables executionPlan.Variables variables
    let ctx = {
        Schema = schema
        ExecutionPlan = executionPlan
        RootValue = root
        Variables = variables
        Errors = errors }
    let resultSet =
        executionPlan.Fields
        |> List.filter (fun info -> info.Include ctx.Variables)
        |> List.map (fun info -> (info.Identifier, info))
        |> List.toArray

    let parseQuery o =
        let dict, deferred, streamed = executeQueryOrMutation resultSet ctx o fieldExecuteMap root 
        match deferred, streamed with
        | Some d, None -> dict |> AsyncVal.map(fun (dict', errors') -> Deferred(dict', errors', d |> Observable.map(fun x -> upcast x)))
        | None, Some s -> dict |> AsyncVal.map(fun (dict', errors') -> Deferred(dict', errors', s |> Observable.map(fun x -> upcast x)))
        | Some d, Some s -> dict |> AsyncVal.map(fun (dict', errors') -> Direct(dict', errors')) // TODO: this case should not exist. Deferred should hold streamed, deferred and live results
        | None, None -> dict |> AsyncVal.map(fun (dict', errors') -> Direct(dict', errors'))

    match executionPlan.Operation.OperationType with
    | Subscription ->
        match schema.Subscription with
        | Some s -> 
            AsyncVal.wrap(Stream(executeSubscription resultSet ctx s fieldExecuteMap schema.SubscriptionProvider root))
        | None -> raise(InvalidOperationException("Attempted to make a subscription but no subscription schema was present!"))
    | Mutation -> 
        match schema.Mutation with
        | Some m ->
            parseQuery m
        | None -> raise(InvalidOperationException("Attempted to make a mutation but no mutation schema was present!"))
    | Query -> parseQuery schema.Query
