/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Reactive
open System.Reactive.Subjects
open System.Reactive.Disposables
open System.Reactive.Linq
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Introspection
open FSharp.Data.GraphQL.Planning
open FSharp.Data.GraphQL.Execution



/// A configuration object fot the GraphQL server schema.
type SchemaConfig =
    { /// List of types that couldn't be resolved from schema query root 
      /// tree traversal, but should be included anyway.
      Types : NamedDef list
      /// List of custom directives that should be included as known to the schema.
      Directives : DirectiveDef list
      /// Function called, when errors occurred during query execution.
      /// It's used to retrieve messages shown as output to the client.
      /// May be also used to log messages before returning them.
      ParseError: exn -> string
      /// Provider for the back-end of the subscription system
      SubscriptionProvider: ISubscriptionProvider
    }
    /// Returns the default Subscription Provider, backed by Observable streams
    static member DefaultSubscriptionProvider() = 
        let registeredSubscriptions = new Dictionary<string, Subscription * Subject<obj>>()
        { new ISubscriptionProvider with
            member this.Register (subscription: Subscription) =
                registeredSubscriptions.Add(subscription.Name, (subscription, new Subject<obj>()))
            member this.Add (ctx: ResolveFieldContext) (root: obj) (name: string)  =
                match registeredSubscriptions.TryGetValue(name) with
                | true, (sub, channel) -> 
                    channel
                    |> Observable.filter(fun o -> sub.Filter ctx root o)
                | false, _ -> Observable.Empty()
            member this.Publish<'T> (subIdent: string) (value: 'T) =
                match registeredSubscriptions.TryGetValue(subIdent) with
                | true, (sub, channel) ->
                    channel.OnNext(box value)
                | false, _ -> printfn "Error: Tried to publish on non-existent channel `%s`" subIdent
        }

    static member Default = 
        { Types = []
          Directives = [ IncludeDirective; SkipDirective; DeferDirective; StreamDirective ]
          ParseError = fun e -> e.Message
          SubscriptionProvider = SchemaConfig.DefaultSubscriptionProvider() }


/// GraphQL server schema. Defines the complete type system to be used by GraphQL queries.
type Schema<'Root> (query: ObjectDef<'Root>, ?mutation: ObjectDef<'Root>, ?subscription: SubscriptionObjectDef<'Root>, ?config: SchemaConfig) =
    let rec insert ns typedef =
        let inline addOrReturn tname (tdef: NamedDef) acc =
            if Map.containsKey tname acc 
            then acc 
            else Map.add tname tdef acc

        match typedef with
        | Scalar scalardef -> addOrReturn scalardef.Name typedef ns
        | Enum enumdef -> addOrReturn enumdef.Name typedef ns
        | Object objdef -> 
            let ns' = addOrReturn objdef.Name typedef ns
            let withFields' =
                objdef.Fields
                |> Map.toArray
                |> Array.map snd
                |> Array.collect (fun x -> Array.append [| x.TypeDef :> TypeDef |] (x.Args |> Array.map (fun a -> upcast a.TypeDef)))
                |> Array.map(function | Named n -> n | _ -> failwith "Expected a Named type!")
                |> Array.filter (fun x -> not (Map.containsKey x.Name ns'))
                |> Array.fold (insert) ns'
            objdef.Implements
            |> Array.fold insert withFields'
        | Interface interfacedef ->
            let ns' = addOrReturn typedef.Name typedef ns
            interfacedef.Fields
            |> Array.map (fun x -> match x.TypeDef with | Named n -> n | _ -> failwith "Expected a Named type!")
            |> Array.filter (fun x -> not (Map.containsKey x.Name ns'))
            |> Array.fold (insert) ns'    
        | Union uniondef ->
            let ns' = addOrReturn typedef.Name typedef ns
            uniondef.Options
            |> Array.fold insert ns'
        | List (Named innerdef) -> insert ns innerdef 
        | Nullable (Named innerdef) -> insert ns innerdef
        | InputObject objdef -> 
            let ns' = addOrReturn objdef.Name typedef ns
            objdef.Fields
            |> Array.collect (fun x -> [| x.TypeDef :> TypeDef |])
            |> Array.map(function | Named n -> n | _ -> failwith "Expected a Named type!")
            |> Array.filter (fun x -> not (Map.containsKey x.Name ns'))
            |> Array.fold (insert) ns'
        | _ -> failwith "Unexpected type!"
        
    let initialTypes: NamedDef list = [ 
        Int
        String
        Boolean
        Float
        ID
        Date
        Uri
        __Schema
        query]

    let schemaConfig = match config with None -> SchemaConfig.Default | Some c -> c
    let mutable typeMap: Map<string, NamedDef> = 
        let m = 
            mutation 
            |> function Some(Named n) -> [n] | _ -> []
        let s =
            subscription
            |> function Some(Named n) -> [n] | _ -> []
        initialTypes @ s @ m @ schemaConfig.Types
        |> List.fold insert Map.empty

    let implementations =
        typeMap
        |> Map.toSeq
        |> Seq.choose (fun (_, v) ->
            match v with
            | Object odef -> Some odef
            | _ -> None)
        |> Seq.fold (fun acc objdef -> 
            objdef.Implements
            |> Array.fold (fun acc' iface ->
                match Map.tryFind iface.Name acc' with
                | Some list -> Map.add iface.Name (objdef::list) acc'
                | None -> Map.add iface.Name [objdef] acc') acc
            ) Map.empty
    
    let getPossibleTypes abstractDef =
        match abstractDef with
        | Union u -> u.Options
        | Interface i -> Map.find i.Name implementations |> Array.ofList
        | _ -> [||]

    //-- INTROSPECTION SCHEMA GENERATION

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
          Type = introspectTypeRef false namedTypes subdef.InputTypeDef
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

    let introspectSchema types : IntrospectionSchema =
        let inamed = 
            types 
            |> Map.map (fun typeName typedef -> 
                match typedef with
                | Scalar x -> { Kind = TypeKind.SCALAR; Name = Some typeName; Description = x.Description; OfType = None }
                | Object x -> { Kind = TypeKind.OBJECT; Name = Some typeName; Description = x.Description; OfType = None }
                | InputObject x -> { Kind = TypeKind.INPUT_OBJECT; Name = Some typeName; Description = x.Description; OfType = None }
                | Union x -> { Kind = TypeKind.UNION; Name = Some typeName; Description = x.Description; OfType = None }
                | Enum x -> { Kind = TypeKind.ENUM; Name = Some typeName; Description = x.Description; OfType = None }
                | Interface x -> { Kind = TypeKind.INTERFACE; Name = Some typeName; Description = x.Description; OfType = None }
                | _ -> failwithf "Unexpected value of typedef: %O" typedef)

        let itypes =
            types
            |> Map.toArray
            |> Array.map (snd >> (introspectType inamed))

        let idirectives = 
            schemaConfig.Directives 
            |> List.map (introspectDirective inamed)
            |> List.toArray
            
        let ischema =
            { QueryType = Map.find query.Name inamed
              MutationType = mutation |> Option.map (fun m -> Map.find m.Name inamed)
              SubscriptionType = None // not supported yet
              Types = itypes
              Directives = idirectives }
        ischema
        
    let introspected = introspectSchema typeMap      

    interface ISchema with        
        member val TypeMap = typeMap
        member val Directives = schemaConfig.Directives |> List.toArray
        member val Introspected = introspected
        member __.Query = upcast query
        member __.Mutation = mutation |> Option.map (fun x -> upcast x)
        member __.Subscription = subscription |> Option.map (fun x -> upcast x)
        member __.TryFindType typeName = Map.tryFind typeName typeMap
        member __.GetPossibleTypes typedef = getPossibleTypes typedef
        member __.ParseError exn = schemaConfig.ParseError exn
        member x.IsPossibleType abstractdef (possibledef: ObjectDef) =
            match (x :> ISchema).GetPossibleTypes abstractdef with
            | [||] -> false
            | possibleTypes -> possibleTypes |> Array.exists (fun t -> t.Name = possibledef.Name)
        member __.SubscriptionProvider = schemaConfig.SubscriptionProvider

    interface ISchema<'Root> with
        member x.Query = query
        member x.Mutation = mutation
        member x.Subscription = subscription

    interface System.Collections.Generic.IEnumerable<NamedDef> with
        member x.GetEnumerator() = (typeMap |> Map.toSeq |> Seq.map snd).GetEnumerator()

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (typeMap |> Map.toSeq |> Seq.map snd :> System.Collections.IEnumerable).GetEnumerator()
        