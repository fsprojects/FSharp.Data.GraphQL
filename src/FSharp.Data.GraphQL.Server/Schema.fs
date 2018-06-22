/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System.Collections.Generic
open System.Reactive.Subjects
open System.Reactive.Linq
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Introspection

/// A configuration object fot the GraphQL server schema.
type SchemaConfig =
    { /// List of types that couldn't be resolved from schema query root 
      /// tree traversal, but should be included anyway.
      Types : NamedDef list
      /// List of custom directives that should be included as known to the schema.
      Directives : DirectiveDef list
      /// Function called when errors occurred during query execution.
      /// It's used to retrieve messages shown as output to the client.
      /// May be also used to log messages before returning them.
      ParseError : exn -> string
      /// Provider for the back-end of the subscription system.
      SubscriptionProvider : ISubscriptionProvider
      /// Provider for the back-end of the live query subscription system.
      LiveFieldSubscriptionProvider : ILiveFieldSubscriptionProvider
    }
    /// Returns the default Subscription Provider, backed by Observable streams.
    static member DefaultSubscriptionProvider() = 
        let registeredSubscriptions = new Dictionary<string, Subscription * Subject<obj>>()
        { new ISubscriptionProvider with
            member __.Register (subscription : Subscription) =
                registeredSubscriptions.Add(subscription.Name, (subscription, new Subject<obj>()))

            member __.Add (ctx: ResolveFieldContext) (root: obj) (subdef: SubscriptionFieldDef)  =
                match registeredSubscriptions.TryGetValue(subdef.Name) with
                | true, (sub, channel) -> 
                    channel
                    |> Observable.map(fun o -> sub.Filter ctx root o)
                    |> Observable.choose(id)
                | false, _ -> Observable.Empty()

            member __.Publish (def: SubscriptionFieldDef<'Root, 'Input, 'Output>) (value: 'Input) =
                match registeredSubscriptions.TryGetValue(def.Name) with
                | true, (_, channel) -> channel.OnNext(box value)
                | false, _ -> () }

    /// Returns the default live field Subscription Provider, backed by Observable streams.
    static member DefaultLiveFieldSubscriptionProvider() =
        let registeredSubscriptions = new Dictionary<string * string, ILiveFieldSubscription * Subject<obj>>()
        { new ILiveFieldSubscriptionProvider with 
            member __.IsRegistered (typeName : string) (fieldName : string) =
                let key = typeName, fieldName
                registeredSubscriptions.ContainsKey(key)
            member __.Register (subscription : ILiveFieldSubscription) =
                let key = subscription.TypeName, subscription.FieldName
                let value = subscription, new Subject<obj>()
                registeredSubscriptions.Add(key, value)
            member __.TryFindSubscription (typeName : string) (fieldName : string) =
                let key = typeName, fieldName
                match registeredSubscriptions.TryGetValue(key) with
                | (true, (sub, _)) -> Some sub
                | _ -> None
            member __.Add (identity) (typeName : string) (fieldName : string) =
                let key = typeName, fieldName
                match registeredSubscriptions.TryGetValue(key) with
                | true, (sub, channel) -> channel |> Observable.filter (fun o -> sub.Identity o = identity)
                | false, _ -> Observable.Empty()
            member __.Publish<'T> (typeName : string) (fieldName : string) (value : 'T) =
                let key = typeName, fieldName
                match registeredSubscriptions.TryGetValue(key) with
                | true, (_, channel) -> channel.OnNext(box value)
                | false, _ -> () }
    /// Default SchemaConfig used by Schema when no config is provided.
    static member Default = 
        { Types = []
          Directives = [ IncludeDirective; SkipDirective; DeferDirective; StreamDirective; LiveDirective ]
          ParseError = fun e -> e.Message
          SubscriptionProvider = SchemaConfig.DefaultSubscriptionProvider()
          LiveFieldSubscriptionProvider = SchemaConfig.DefaultLiveFieldSubscriptionProvider() }


/// GraphQL server schema. Defines the complete type system to be used by GraphQL queries.
type Schema<'Root> (query: ObjectDef<'Root>, ?mutation: ObjectDef<'Root>, ?subscription: SubscriptionObjectDef<'Root>, ?config: SchemaConfig) =
    let initialTypes: NamedDef list = 
        [ Int
          String
          Boolean
          Float
          ID
          Date
          Uri
          __Schema
          query ]

    let schemaConfig = 
        match config with
        | None -> SchemaConfig.Default
        | Some c -> c

    let typeMap : TypeMap =
        let m = mutation |> function Some(Named n) -> [n] | _ -> []
        let s = subscription |> function Some(Named n) -> [n] | _ -> []
        initialTypes @ s @ m @ schemaConfig.Types |> TypeMap.FromSeq

    let getImplementations (typeMap : TypeMap) =
        typeMap.ToSeq()
        |> Seq.choose (fun (_, v) ->
            match v with
            | Object odef -> Some odef
            | _ -> None)
        |> Seq.fold (fun acc objdef -> 
            objdef.Implements
            |> Array.fold (fun acc' iface ->
                match Map.tryFind iface.Name acc' with
                | Some list -> Map.add iface.Name (objdef::list) acc'
                | None -> Map.add iface.Name [objdef] acc') acc) Map.empty

    let implementations = lazy (getImplementations typeMap)
    
    let getPossibleTypes abstractDef =
        match abstractDef with
        | Union u -> u.Options
        | Interface i -> Map.find i.Name (implementations.Force()) |> Array.ofList
        | _ -> [||]

    let rec introspectTypeRef isNullable (namedTypes: Map<string, IntrospectionTypeRef>) typedef =
        match typedef with
        | Nullable inner -> introspectTypeRef true namedTypes inner
        | List inner -> 
            if isNullable 
            then IntrospectionTypeRef.List(introspectTypeRef false namedTypes inner)
            else IntrospectionTypeRef.NonNull(introspectTypeRef true namedTypes typedef)
        | Named named -> 
            if isNullable
            then Map.find named.Name namedTypes
            else IntrospectionTypeRef.NonNull(introspectTypeRef true namedTypes typedef)
        | _ -> failwithf "Unexpected value of typedef: %O" typedef

    let introspectInput (namedTypes: Map<string, IntrospectionTypeRef>) (inputDef: InputFieldDef) : IntrospectionInputVal =
        { Name = inputDef.Name
          Description = inputDef.Description
          Type = introspectTypeRef false namedTypes inputDef.TypeDef
          DefaultValue = inputDef.DefaultValue |> Option.map string }

    let introspectField (namedTypes: Map<string, IntrospectionTypeRef>) (fdef: FieldDef) =
        { Name = fdef.Name
          Description = fdef.Description
          Args = fdef.Args |> Array.map (introspectInput namedTypes) 
          Type = introspectTypeRef false namedTypes fdef.TypeDef
          IsDeprecated = Option.isSome fdef.DeprecationReason
          DeprecationReason = fdef.DeprecationReason }

    let instrospectSubscriptionField (namedTypes: Map<string, IntrospectionTypeRef>) (subdef: SubscriptionFieldDef) =
        { Name = subdef.Name
          Description = subdef.Description
          Args = subdef.Args |> Array.map (introspectInput namedTypes) 
          Type = introspectTypeRef false namedTypes subdef.OutputTypeDef
          IsDeprecated = Option.isSome subdef.DeprecationReason
          DeprecationReason = subdef.DeprecationReason }

    let introspectEnumVal (enumVal: EnumVal) : IntrospectionEnumVal =
        { Name = enumVal.Name
          Description = enumVal.Description
          IsDeprecated = Option.isSome enumVal.DeprecationReason
          DeprecationReason = enumVal.DeprecationReason }

    let locationToList location =
        System.Enum.GetValues(typeof<DirectiveLocation>)
        |> Seq.cast<DirectiveLocation>
        |> Seq.filter (fun v -> int(location) &&& int(v) <> 0)
        |> Seq.toArray

    let introspectDirective (namedTypes: Map<string, IntrospectionTypeRef>) (directive: DirectiveDef) : IntrospectionDirective =
        { Name = directive.Name
          Description = directive.Description
          Locations = locationToList directive.Locations
          Args = directive.Args |> Array.map (introspectInput namedTypes) }

    let introspectType (namedTypes: Map<string, IntrospectionTypeRef>) typedef =
        match typedef with
        | Scalar scalardef -> 
            IntrospectionType.Scalar(scalardef.Name, scalardef.Description)
        | SubscriptionObject subdef ->
            let fields =
                subdef.Fields
                |> Map.toArray
                |> Array.map (snd >> instrospectSubscriptionField namedTypes)
            let interfaces = 
                subdef.Implements 
                |> Array.map (fun idef -> Map.find idef.Name namedTypes)
            IntrospectionType.Object(subdef.Name, subdef.Description, fields, interfaces)
        | Object objdef -> 
            let fields = 
                objdef.Fields 
                |> Map.toArray
                |> Array.map (snd >> introspectField namedTypes)
            let interfaces = 
                objdef.Implements 
                |> Array.map (fun idef -> Map.find idef.Name namedTypes)
            IntrospectionType.Object(objdef.Name, objdef.Description, fields, interfaces)
        | InputObject inObjDef -> 
            let inputs = 
                inObjDef.Fields 
                |> Array.map (introspectInput namedTypes)
            IntrospectionType.InputObject(inObjDef.Name, inObjDef.Description, inputs)
        | Union uniondef -> 
            let possibleTypes = 
                getPossibleTypes uniondef
                |> Array.map (fun tdef -> Map.find tdef.Name namedTypes)
            IntrospectionType.Union(uniondef.Name, uniondef.Description, possibleTypes)
        | Enum enumdef -> 
            let enumVals = 
                enumdef.Options
                |> Array.map introspectEnumVal
            IntrospectionType.Enum(enumdef.Name, enumdef.Description, enumVals)
        | Interface idef ->
            let fields = 
                idef.Fields 
                |> Array.map (introspectField namedTypes)
            let possibleTypes = 
                getPossibleTypes idef
                |> Array.map (fun tdef -> Map.find tdef.Name namedTypes)
            IntrospectionType.Interface(idef.Name, idef.Description, fields, possibleTypes)
        | _ -> failwithf "Unexpected value of typedef: %O" typedef  

    let introspectSchema (types : TypeMap) : IntrospectionSchema =
        let inamed = 
            types.ToSeq()
            |> Seq.map (fun (typeName, typedef) -> 
                match typedef with
                | Scalar x -> typeName, { Kind = TypeKind.SCALAR; Name = Some typeName; Description = x.Description; OfType = None }
                | Object x -> typeName, { Kind = TypeKind.OBJECT; Name = Some typeName; Description = x.Description; OfType = None }
                | InputObject x -> typeName, { Kind = TypeKind.INPUT_OBJECT; Name = Some typeName; Description = x.Description; OfType = None }
                | Union x -> typeName, { Kind = TypeKind.UNION; Name = Some typeName; Description = x.Description; OfType = None }
                | Enum x -> typeName, { Kind = TypeKind.ENUM; Name = Some typeName; Description = x.Description; OfType = None }
                | Interface x -> typeName, { Kind = TypeKind.INTERFACE; Name = Some typeName; Description = x.Description; OfType = None }
                | _ -> failwithf "Unexpected value of typedef: %O" typedef)
            |> Map.ofSeq
        let itypes =
            types.ToSeq()
            |> Seq.toArray
            |> Array.map (snd >> (introspectType inamed))
        let idirectives = 
            schemaConfig.Directives 
            |> List.map (introspectDirective inamed)
            |> List.toArray
        { QueryType = Map.find query.Name inamed
          MutationType = mutation |> Option.map (fun m -> Map.find m.Name inamed)
          SubscriptionType = None
          Types = itypes
          Directives = idirectives }
          
    let introspected = lazy (introspectSchema typeMap)

    interface ISchema with        
        member __.TypeMap = typeMap
        member __.Directives = schemaConfig.Directives |> List.toArray
        member __.Introspected = introspected.Force()
        member __.Query = upcast query
        member __.Mutation = mutation |> Option.map (fun x -> upcast x)
        member __.Subscription = subscription |> Option.map (fun x -> upcast x)
        member __.TryFindType typeName = typeMap.TryFind(typeName, includeDefaultTypes = true)
        member __.GetPossibleTypes typedef = getPossibleTypes typedef
        member __.ParseError exn = schemaConfig.ParseError exn
        member x.IsPossibleType abstractdef (possibledef: ObjectDef) =
            match (x :> ISchema).GetPossibleTypes abstractdef with
            | [||] -> false
            | possibleTypes -> possibleTypes |> Array.exists (fun t -> t.Name = possibledef.Name)
        member __.SubscriptionProvider = schemaConfig.SubscriptionProvider
        member __.LiveFieldSubscriptionProvider = schemaConfig.LiveFieldSubscriptionProvider        

    interface ISchema<'Root> with
        member __.Query = query
        member __.Mutation = mutation
        member __.Subscription = subscription

    interface System.Collections.Generic.IEnumerable<NamedDef> with
        member __.GetEnumerator() = (typeMap.ToSeq() |> Seq.map snd).GetEnumerator()

    interface System.Collections.IEnumerable with
        member __.GetEnumerator() = (typeMap.ToSeq() |> Seq.map snd :> System.Collections.IEnumerable).GetEnumerator()
        