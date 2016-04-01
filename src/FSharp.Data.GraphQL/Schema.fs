/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Validation

type Schema() =
    let mutable types: GraphQLType list = []
    member x.Validate (): ValidationResult = Success
    member x.Add (schema: GraphQLType) : unit = types <- types @ [ schema ]     // we must append to the end
    member x.Add (schemas: GraphQLType list) = types <- schemas @ types
    
    interface System.Collections.Generic.IEnumerable<GraphQLType> with
        member x.GetEnumerator() = (types :> System.Collections.Generic.IEnumerable<GraphQLType>).GetEnumerator()

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (types :> System.Collections.IEnumerable).GetEnumerator()

    static member Scalar (name: string, coerceInput, ?description: string) = 
        {
            Name = name
            Description = description
            CoerceInput = coerceInput
        }
        
    /// GraphQL type for user defined enums
    static member Enum (name: string, options: EnumValue list, ?description: string): GraphQLType = Enum { Name = name; Description = description; Options = options }

    /// Single enum option to be used as argument in <see cref="Schema.Enum"/>
    static member EnumValue (name: string, value: 'Val, ?description: string): EnumValue = { Name = name; Description = description; Value = value :> obj }

    /// GraphQL custom object type
    static member ObjectType (name: string, fields: Field list, ?description: string, ?interfaces: Interface list): GraphQLType = 
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
    static member Interface (name: string, fields: Field list, ?description: string): Interface = {
        Name = name
        Description = description
        Fields = fields 
    }

    /// GraphQL custom union type, materialized as one of the types defined. It can be used as interface/object type field.
    static member Union (name: string, options: GraphQLType list, ?description: string): GraphQLType = 
        Union {
            Name = name
            Description = description
            Options = options
        }
