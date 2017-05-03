/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
namespace FSharp.Data.GraphQL.Execution


/// Name value lookup used as output to be serialized into JSON.
/// It has a form of a dictionary with fixed set of keys. Values under keys
/// can be set, but no new entry can be added or removed, once lookup
/// has been initialized.
/// This dicitionay implements structural equality.
module SchemaCompiler =                
    open System
    open System.Reflection
    open System.Runtime.InteropServices;
    open System.Collections.Generic
    open System.Collections.Concurrent
    open FSharp.Data.GraphQL
    open FSharp.Data.GraphQL.Ast
    open FSharp.Data.GraphQL.Types
    open FSharp.Data.GraphQL.Types.Resolve
    open FSharp.Data.GraphQL.Types.Patterns
    open FSharp.Data.GraphQL.Planning
    open FSharp.Data.GraphQL.Types.Introspection
    open FSharp.Data.GraphQL.Introspection
    open FSharp.Data.GraphQL.Values
    open FSharp.Quotations
    open FSharp.Quotations.Patterns
    open FSharp.Reflection.FSharpReflectionExtensions
    
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
                    
    /// Creates a function that returns only the selected set of fields
    let rec private createCompletion (possibleTypesFn: TypeDef -> ObjectDef []) (returnDef: OutputDef) (fieldExecuteMap: FieldExecuteMap): ResolveFieldContext -> obj -> AsyncVal<obj> =
        match returnDef with
        | Object objdef -> 
            fun (ctx: ResolveFieldContext) value -> 
                match ctx.ExecutionInfo.Kind with
                | SelectFields fields -> executeFields objdef ctx value fields fieldExecuteMap
                | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind 
        
        | Scalar scalardef ->
            let (coerce: obj -> obj option) = scalardef.CoerceValue
            fun _ value -> 
                coerce value
                |> Option.toObj
                |> AsyncVal.wrap
        
        | List (Output innerdef) ->
            let (innerfn: ResolveFieldContext -> obj -> AsyncVal<obj>) = createCompletion possibleTypesFn innerdef fieldExecuteMap
            fun ctx (value: obj) ->
                let innerCtx =
                    match ctx.ExecutionInfo.Kind with
                    | ResolveCollection innerPlan -> { ctx with ExecutionInfo = innerPlan }
                    | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind 
                match value with
                | :? string as s -> 
                    innerfn innerCtx (s)
                    |> AsyncVal.map (fun x -> upcast [| x |])
                | :? System.Collections.IEnumerable as enumerable ->
                    let completed =
                        enumerable
                        |> Seq.cast<obj>
                        |> Seq.map (fun x -> innerfn innerCtx x)
                        |> Seq.toArray
                        |> AsyncVal.collectParallel
                        |> AsyncVal.map box
                    completed
                | _ -> raise <| GraphQLException (sprintf "Expected to have enumerable value in field '%s' but got '%O'" ctx.ExecutionInfo.Identifier (value.GetType()))
        
        | Nullable (Output innerdef) ->
            let innerfn = createCompletion possibleTypesFn innerdef fieldExecuteMap
            let optionDef = typedefof<option<_>>
            fun ctx value ->
                let t = value.GetType()
                if value = null then AsyncVal.empty
    #if NETSTANDARD1_6           
                elif t.GetTypeInfo().IsGenericType && t.GetTypeInfo().GetGenericTypeDefinition() = optionDef then
    #else       
                elif t.IsGenericType && t.GetGenericTypeDefinition() = optionDef then
    #endif                  
                    let _, fields = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(value, t)
                    innerfn ctx fields.[0]
                else innerfn ctx value
        
        | Interface idef ->
            let resolver = resolveInterfaceType possibleTypesFn idef
            fun ctx value -> 
                let resolvedDef = resolver value
                let typeMap =
                    match ctx.ExecutionInfo.Kind with
                    | ResolveAbstraction typeMap -> typeMap
                    | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind 
                match Map.tryFind resolvedDef.Name typeMap with
                | Some fields -> executeFields resolvedDef ctx value fields fieldExecuteMap
                | None -> raise(GraphQLException (sprintf "GraphQL interface '%s' is not implemented by the type '%s'" idef.Name resolvedDef.Name))   
            
        | Union udef ->
            let resolver = resolveUnionType possibleTypesFn udef
            fun ctx value ->
                let resolvedDef = resolver value
                let typeMap =
                    match ctx.ExecutionInfo.Kind with
                    | ResolveAbstraction typeMap -> typeMap
                    | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind 
                match Map.tryFind resolvedDef.Name typeMap with
                | Some fields -> executeFields resolvedDef ctx (udef.ResolveValue value) fields fieldExecuteMap
                | None -> raise(GraphQLException (sprintf "GraphQL union '%s' doesn't have a case of type '%s'" udef.Name resolvedDef.Name))   
            
        | Enum _ ->
            fun _ value -> 
                let result = coerceStringValue value
                AsyncVal.wrap (result |> Option.map box |> Option.toObj)
        
        | _ -> failwithf "Unexpected value of returnDef: %O" returnDef
    
    
    /// Takes an object type and a field, and returns that field’s type on the object type, or null if the field is not valid on the object type
    and private getFieldDefinition (ctx: ExecutionContext) (objectType: ObjectDef) (field: Field) : FieldDef option =
            match field.Name with
            | "__schema" when Object.ReferenceEquals(ctx.Schema.Query, objectType) -> Some (upcast SchemaMetaFieldDef)
            | "__type" when Object.ReferenceEquals(ctx.Schema.Query, objectType) -> Some (upcast TypeMetaFieldDef)
            | "__typename" -> Some (upcast TypeNameMetaFieldDef)
            | fieldName -> objectType.Fields |> Map.tryFind fieldName
            
    and private createFieldContext objdef ctx (info: ExecutionInfo) =
        let fdef = info.Definition
        let args = getArgumentValues fdef.Args info.Ast.Arguments ctx.Variables
        { ExecutionInfo = info
          Context = ctx.Context
          ReturnType = fdef.TypeDef
          ParentType = objdef
          Schema = ctx.Schema
          Args = args
          Variables = ctx.Variables }         
    
    and private executeFields (objdef: ObjectDef) (ctx: ResolveFieldContext) (value: obj) fieldInfos (fieldExecuteMap: FieldExecuteMap) : AsyncVal<obj> = 
        let resultSet =
            fieldInfos
            |> List.filter (fun info -> info.Include ctx.Variables)
            |> List.map (fun info -> (info.Identifier, info))
            |> List.toArray
        resultSet
        |> Array.map (fun (name, info) ->  
            let innerCtx = createFieldContext objdef ctx info
            let execute = fieldExecuteMap.GetExecute(objdef.Name, info.Definition.Name)
            let res = execute innerCtx value
            res 
            |> AsyncVal.map (fun x -> KeyValuePair<_,_>(name, x))
            |> AsyncVal.rescue (fun e -> ctx.AddError e; KeyValuePair<_,_>(name, null)))
        |> AsyncVal.collectParallel
        |> AsyncVal.map (fun x -> upcast NameValueLookup x)

    /// Builds the resolution function for the value of the object that needs to undergo projection
    let internal resolveInitialObject (fieldDef: FieldDef): ExecuteField =
        match fieldDef.Resolve with
        | Resolve.BoxedSync(inType, outType, resolve) ->
            fun resolveFieldCtx value ->
                resolve resolveFieldCtx value
                |> AsyncVal.wrap
        | Resolve.BoxedAsync(intType, outType, resolve) ->
            fun resolveFieldCtx value ->
                resolve resolveFieldCtx value
                |> AsyncVal.ofAsync
        | Resolve.BoxedExpr (resolve) ->
            fun resolveFieldCtx value ->
                downcast resolve resolveFieldCtx value
        | Undefined -> 
            fun _ _ -> raise (InvalidOperationException(sprintf "Field '%s' has been accessed, but no resolve function for that field definition was provided. Make sure, you've specified resolve function or declared field with Define.AutoField method" fieldDef.Name))

    let internal compileField possibleTypesFn (fieldDef: FieldDef) (fieldExecuteMap: FieldExecuteMap) : ExecuteField =
        // SubscriptionFieldDef's are a special case in that the type returned from the completion should be the input type
        // We do this so that when we call the resolver function when the event is fired, we can actually return the proper fields
        let returnType, resolve = 
            match box fieldDef with
            | :? SubscriptionFieldDef as s -> s.InputTypeDef, (fun resolveFieldCtx value -> AsyncVal.wrap(value))
            | :? FieldDef as f -> f.TypeDef, resolveInitialObject f
            | _ -> raise(GraphQLException (sprintf "Invalid field type %A" fieldDef.GetType))   
        let completed = createCompletion possibleTypesFn returnType fieldExecuteMap
        fun resolveFieldCtx value ->
            try
                resolve resolveFieldCtx value
                |> AsyncVal.bind(completed resolveFieldCtx)
            with
                | :? AggregateException as e ->
                    e.InnerExceptions |> Seq.iter (resolveFieldCtx.AddError)
                    AsyncVal.empty
                | ex -> 
                    resolveFieldCtx.AddError ex
                    AsyncVal.empty
    let private compileInputObject (indef: InputObjectDef) =
        indef.Fields
        |> Array.iter(fun input -> 
            let errMsg = sprintf "Input object '%s': in field '%s': " indef.Name input.Name
            input.ExecuteInput <- compileByType errMsg input.TypeDef)
    
    let compileSubscriptionField (subfield: SubscriptionFieldDef) = 
        // This bears some explaining...
        // We use the Resolve module to define our subscription callbacks,
        // But unboxing a Resolve casts the untyped expr into a Expr<ResolveFieldContext -> obj -> obj>
        // Subscription callbacks should be of type Expr<ResolveFieldContext -> obj -> unit>
        // First we define a function that composes a function of two arguments with one that consumes one
        let inline (>>+) f g x y = g (f x y)
        match subfield.Resolve with
        // Now we compose our resolve function with one that simply ignores the output
        | Resolve.BoxedSync(inType, unit, resolve) -> resolve >>+ ignore
        // TODO: Implement Async subscription callbacks
        | Resolve.BoxedAsync(inType, unit, resolve) -> resolve >>+ ignore
        | Resolve.BoxedExpr(resolve) -> resolve >>+ ignore
    
    let private compileObject (objdef: ObjectDef) (executeFields: FieldDef -> unit) =
        objdef.Fields
        |> Map.iter (fun _ fieldDef ->
            executeFields fieldDef
            fieldDef.Args
            |> Array.iter (fun arg -> 
                let errMsg = sprintf "Object '%s': field '%s': argument '%s': " objdef.Name fieldDef.Name arg.Name
                arg.ExecuteInput <- compileByType errMsg arg.TypeDef))
    
    
    let internal compileSchema possibleTypesFn types (fieldExecuteMap: FieldExecuteMap) (subscriptionHandler: SubscriptionHandler) =
        types
        |> Map.toSeq 
        |> Seq.iter (fun (tName, x) ->
            match x with
            | SubscriptionObject subdef -> 
                compileObject subdef (fun sub -> 
                    // Subscription Objects only contain subscription fields, so this cast is safe
                    let subField = (sub :?> SubscriptionFieldDef)
                    subscriptionHandler.RegisterSubscription sub.Name (compileSubscriptionField subField)
                    // Make sure that we register a call in the executeMap so that we know how to resolve the fields
                    fieldExecuteMap.SetExecute(tName, subField.Name, compileField possibleTypesFn subField fieldExecuteMap))
            | Object objdef -> 
                compileObject objdef (fun fieldDef -> fieldExecuteMap.SetExecute(tName, fieldDef.Name, compileField possibleTypesFn fieldDef fieldExecuteMap))
            | InputObject indef -> compileInputObject indef
            | _ -> ())

            
module QueryExecution =
    open System
    open System.Reflection
    open System.Collections.Generic
    open FSharp.Data.GraphQL
    open FSharp.Data.GraphQL.Types
    open FSharp.Data.GraphQL.Ast
    open FSharp.Data.GraphQL.Types.Resolve
    open FSharp.Data.GraphQL.Types.Patterns
    open FSharp.Data.GraphQL.Planning
    open FSharp.Data.GraphQL.Types.Introspection
    open FSharp.Data.GraphQL.Values
    
    // Activates subscriptions by provinding them with context
    let internal executeSubscription (resultSet: (string * ExecutionInfo) []) (ctx: ExecutionContext)  (objdef: SubscriptionObjectDef) (subscriptionHandler: SubscriptionHandler) value :unit =
         // Activate subscriptions for all of the given Fields
         resultSet
         |> Array.iter (fun (name, info) ->
            let subdef = info.Definition :?> SubscriptionFieldDef
            let args = getArgumentValues subdef.Args info.Ast.Arguments ctx.Variables
            let fieldCtx = 
                { ExecutionInfo = info
                  Context = ctx
                  ReturnType = subdef.InputTypeDef
                  ParentType = objdef
                  Schema = ctx.Schema
                  Args = args
                  Variables = ctx.Variables } 
            subscriptionHandler.ActivateSubscription subdef.Name fieldCtx)
    
    let internal executeQuery (resultSet: (string * ExecutionInfo) []) (ctx: ExecutionContext)  (objdef: ObjectDef) (fieldExecuteMap: FieldExecuteMap) value =
        let results =
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
                let res = execute fieldCtx value
                //let res = //info.Definition.Execute fieldCtx value
                res
                |> AsyncVal.map (fun r -> KeyValuePair<_,_>(name, r))
                |> AsyncVal.rescue (fun e -> fieldCtx.AddError e; KeyValuePair<_,_>(name, null)))
        match ctx.ExecutionPlan.Strategy with
        | ExecutionStrategy.Parallel -> AsyncVal.collectParallel results
        | ExecutionStrategy.Sequential -> AsyncVal.collectSequential results
    
    
    let private coerceVariables (variables: VarDef list) (vars: Map<string, obj>) =
        variables
        |> List.fold (fun acc vardef -> Map.add vardef.Name (coerceVariable vardef vars) acc) Map.empty
        
    let internal evaluate (schema: #ISchema) (executionPlan: ExecutionPlan) (variables: Map<string, obj>) (root: obj) errors (fieldExecuteMap: FieldExecuteMap) (subscriptionHandler: SubscriptionHandler) : AsyncVal<NameValueLookup> = 
        let variables = coerceVariables executionPlan.Variables variables
        let ctx = {
            Schema = schema
            ExecutionPlan = executionPlan
            RootValue = root
            Variables = variables
            Errors = errors }
        
        let resultSet = resolveFieldValues executionPlan.Fields ctx.Variables
        let result = 
            match executionPlan.Operation.OperationType with
            | Subscription ->
                match schema.Subscription with
                | Some s -> 
                    executeSubscription resultSet ctx s subscriptionHandler root
                    // Return an object detailing the subscription
                    let a = async {
                        return [|KeyValuePair("result", box "Subscription Created")|]
                    }
                    AsyncVal.ofAsync(a)
                | None -> raise(InvalidOperationException("Attempted to make a subscription but no subscription schema was present!"))
            | Mutation -> 
                match schema.Mutation with
                | Some m ->
                    executeQuery resultSet ctx m fieldExecuteMap root 
                | None -> raise(InvalidOperationException("Attempted to make a mutation but no mutation schema was present!"))
            | Query -> executeQuery resultSet ctx schema.Query fieldExecuteMap root 
        result |> AsyncVal.map (NameValueLookup)
    