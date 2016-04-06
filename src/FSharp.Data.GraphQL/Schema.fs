/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Validation

type Schema(query: ObjectType, ?mutation: ObjectType) =
    let mutable types: Map<string, GraphQLType> = Map.empty
    member x.Validate (): ValidationResult = Success
    member x.Add (schema: GraphQLType) : unit = types <- Map.add schema.Name schema types
    member x.Add (schemas: GraphQLType list) = 
        types <- schemas |> List.fold (fun acc s -> Map.add s.Name s acc) types
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
    static member ObjectType (name: string, fields: Field list, ?description: string, ?interfaces: InterfaceType list): GraphQLType = 
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
    static member Field (name: string, schema: GraphQLType, ?description: string, ?arguments: Argument list, ?resolve: Resolver<obj>): Field = {
        Name = name
        Description = description
        Schema = schema
        Resolver = resolve
        Arguments = if arguments.IsNone then [] else arguments.Value
    }

    /// GraphQL custom interface type. It's needs to be implemented object types and should not be used alone.
    static member Interface (name: string, fields: Field list, ?description: string): InterfaceType = {
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
            Options = graphQlOptions
        }
