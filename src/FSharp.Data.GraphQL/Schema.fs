/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Validation

type Schema(query: GraphQLType, ?mutation: GraphQLType) =
    let rec insert ns typedef =
        let inline addOrReturn name typedef' ns' =
            if Map.containsKey name ns' 
            then ns' 
            else Map.add name typedef' ns'

        match typedef with
        | Scalar scalardef -> addOrReturn scalardef.Name typedef ns
        | Enum enumdef -> addOrReturn enumdef.Name typedef ns
        | Object objdef -> 
            let ns' =
                objdef.Fields
                |> List.map (fun x -> x.Schema)
                |> List.fold (fun n t -> insert n t) ns
            objdef.Implements
            |> List.fold (fun n t -> insert n t) ns'
        | Interface interfacedef ->
            interfacedef.Fields
            |> List.map (fun x -> x.Schema)
            |> List.fold (fun n t -> insert n t) ns
        | Union uniondef ->
            uniondef.Options
            |> List.fold (fun n t -> insert n t) ns
        | ListOf innerdef -> insert ns innerdef 
        | NonNull innerdef -> insert ns innerdef
        | InputObject innerdef -> insert ns (Object innerdef)
        
    let nativeTypes = Map.ofList [
        Int.Name, Int
        String.Name, String
        Bool.Name, Bool
        Float.Name, Float
    ]
    let mutable types: Map<string, GraphQLType> = insert nativeTypes query
    member x.TryFindType typeName = Map.tryFind typeName types
    member x.Query with get() = query
    member x.Mutation with get() = mutation

    interface System.Collections.Generic.IEnumerable<GraphQLType> with
        member x.GetEnumerator() = (types |> Map.toSeq |> Seq.map snd).GetEnumerator()

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (types |> Map.toSeq |> Seq.map snd :> System.Collections.IEnumerable).GetEnumerator()

    static member Scalar (name: string, coerceInput: Value -> 'T option, coerceOutput: 'T -> Value option, coerceValue: obj -> 'T, ?description: string) = 
        {
            Name = name
            Description = description
            CoerceInput = coerceInput >> box
            CoerceOutput = (fun x ->
                match x with
                | :? 'T as t -> coerceOutput t
                | _ -> None)
            CoerceValue = coerceValue >> box
        }
        
    /// GraphQL type for user defined enums
    static member Enum (name: string, options: EnumValue list, ?description: string): GraphQLType = Enum { Name = name; Description = description; Options = options }

    /// Single enum option to be used as argument in <see cref="Schema.Enum"/>
    static member EnumValue (name: string, value: 'Val, ?description: string): EnumValue = { Name = name; Description = description; Value = value :> obj }

    /// GraphQL custom object type
    static member ObjectType (name: string, fields: FieldDefinition list, ?description: string, ?interfaces: InterfaceType list): GraphQLType = 
        let o = Object { 
            Name = name
            Description = description
            Fields = fields
            Implements = []
        }
        match interfaces with
        | None -> o
        | Some i -> implements o i

    /// Single field defined inside either object types or interfaces
    static member Field (name: string, schema: GraphQLType, resolve: 'Object -> Args -> ResolveInfo -> 'Value, ?description: string, ?arguments: ArgumentDefinition list): FieldDefinition = {
        Name = name
        Description = description
        Schema = schema
        Resolve = 
            let modified (x: obj) (args: Args) (info:ResolveInfo) : obj = 
                upcast (resolve (x :?> 'Object) args info)
            Some modified
        Arguments = if arguments.IsNone then [] else arguments.Value
    }
    
    /// Single field defined inside either object types or interfaces
    static member Field (name: string, schema: GraphQLType, resolve: 'Object -> Args -> 'Value, ?description: string, ?arguments: ArgumentDefinition list): FieldDefinition =
        {
            Name = name
            Description = description
            Schema = schema
            Resolve = 
                let modified (x: obj) (args: Args) (_:ResolveInfo) : obj = 
                    upcast (resolve (x :?> 'Object) args)
                Some modified
            Arguments = if arguments.IsNone then [] else arguments.Value
        }
        
    /// Single field defined inside either object types or interfaces
    static member Field (name: string, schema: GraphQLType, resolve: 'Object  -> 'Value, ?description: string, ?arguments: ArgumentDefinition list): FieldDefinition =
        {
            Name = name
            Description = description
            Schema = schema
            Resolve = 
                let modified (x: obj) _ (_:ResolveInfo) : obj = 
                    upcast (resolve (x :?> 'Object))
                Some modified
            Arguments = if arguments.IsNone then [] else arguments.Value
        }
        
    /// Single field defined inside either object types or interfaces
    static member Field (name: string, schema: GraphQLType, ?description: string, ?arguments: ArgumentDefinition list): FieldDefinition =
        {
            Name = name
            Description = description
            Schema = schema
            Resolve = None
            Arguments = if arguments.IsNone then [] else arguments.Value
        }

    static member Argument (name: string, schema: GraphQLType, ?defaultValue: 'T, ?description: string): ArgumentDefinition = {
        Name = name
        Description = description
        Type = schema
        DefaultValue = 
            match defaultValue with
            | Some value -> Some (upcast value)
            | None -> None
    }

    /// GraphQL custom interface type. It's needs to be implemented object types and should not be used alone.
    static member Interface (name: string, fields: FieldDefinition list, ?description: string): InterfaceType = {
        Name = name
        Description = description
        Fields = fields 
    }

    /// GraphQL custom union type, materialized as one of the types defined. It can be used as interface/object type field.
    static member Union (name: string, options: GraphQLType list, ?description: string): GraphQLType = 
        let graphQlOptions = 
            options
            |> List.map (fun x ->
                match x with
                | Object o -> o.Name
                | _ -> failwith "Cannot use types other that object types in Union definitions")
        Union {
            Name = name
            Description = description
            Options = options
        }
