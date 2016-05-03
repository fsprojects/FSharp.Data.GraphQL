/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
[<AutoOpen>]
module Helpers

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

let equals (expected : 'x) (actual : 'x) = 
    Assert.True((actual = expected), sprintf "expected %+A\nbut got %+A" expected actual)
let noErrors (result: ExecutionResult) =
    Assert.True((None = result.Errors), sprintf "expected ExecutionResult to have no errors but got %+A" result.Errors)
let throws<'e when 'e :> exn> (action : unit -> unit) = Assert.Throws<'e>(action)
let sync = Async.RunSynchronously
let field name typedef (resolve : 'a -> 'b) = Define.Field(name = name, typedef = typedef, resolve = (fun _ a -> resolve a))
let fieldA name typedef args (resolve : ResolveFieldContext -> 'a -> 'b) = 
    Define.Field(name = name, typedef = typedef, args = args, resolve = resolve)    
let asyncField name typedef (resolve : 'a -> Async<'b>) = Define.AsyncField(name = name, typedef = typedef, resolve = (fun _ a -> resolve a))
let asyncFieldA name typedef args (resolve : ResolveFieldContext -> 'a -> Async<'b>) = 
    Define.AsyncField(name = name, typedef = typedef, arguments = args, resolve = resolve)
let arg name typedef = Define.Arg(name, typedef)
let objdef name (fields: FieldDef list) = Define.Object(name, fields)
let is<'t> (o: obj) = o :? 't
let hasError errMsg errors =
    let containsMessage = 
        errors
        |> Array.exists (fun (GraphQLError e) -> e.Contains(errMsg))
    Assert.True (containsMessage, sprintf "expected to contain message '%s', but no such message was found" errMsg)

let (<??) opt other = 
    match opt with
    | None -> Some other
    | _ -> opt

let introspectionQuery = """query IntrospectionQuery {
    __schema {
      queryType { name }
      mutationType { name }
      subscriptionType { name }
      types {
        ...FullType
      }
      directives {
        name
        description
        locations
        args {
          ...InputValue
        }
      }
    }
  }

  fragment FullType on __Type {
    kind
    name
    description
    fields(includeDeprecated: true) {
      name
      description
      args {
        ...InputValue
      }
      type {
        ...TypeRef
      }
      isDeprecated
      deprecationReason
    }
    inputFields {
      ...InputValue
    }
    interfaces {
      ...TypeRef
    }
    enumValues(includeDeprecated: true) {
      name
      description
      isDeprecated
      deprecationReason
    }
    possibleTypes {
      ...TypeRef
    }
  }

  fragment InputValue on __InputValue {
    name
    description
    type { ...TypeRef }
    defaultValue
  }

  fragment TypeRef on __Type {
    kind
    name
    ofType {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
        }
      }
    }
  }"""

open FSharp.Data.GraphQL.Introspection

type IntrospectionQuery = 
    { __schema : IntrospectionSchema }

and IntrospectionSchema = 
    { queryType : IntrospectionNamedTypeRef
      mutationType : IntrospectionNamedTypeRef option
      subscriptionType : IntrospectionNamedTypeRef option
      types : IntrospectionType list
      directives : IntrospectionDirective list }

and IntrospectionType = 
    | IntrospectionScalarType of IntrospectionScalarType
    | IntrospectionObjectType of IntrospectionObjectType
    | IntrospectionInterfaceType of IntrospectionInterfaceType
    | IntrospectionUnionType of IntrospectionUnionType
    | IntrospectionEnumType of IntrospectionEnumType
    | IntrospectionInputObjectType of IntrospectionInputObjectType

and IntrospectionScalarType = 
    { kind : TypeKind
      name : string
      description : string option }

and IntrospectionObjectType = 
    { kind : TypeKind
      name : string
      description : string option
      fields : IntrospectionField list
      interfaces : IntrospectionNamedTypeRef list }

and IntrospectionInterfaceType = 
    { kind : TypeKind
      name : string
      description : string option
      fields : IntrospectionField list
      possibleTypes : IntrospectionNamedTypeRef list }

and IntrospectionUnionType = 
    { kind : TypeKind
      name : string
      description : string option
      possibleTypes : IntrospectionNamedTypeRef list }

and IntrospectionEnumType = 
    { kind : TypeKind
      name : string
      description : string option
      enumValues : IntrospectionEnumValue list }

and IntrospectionInputObjectType = 
    { kind : TypeKind
      name : string
      description : string option
      inputFields : IntrospectionInputValue list }

and IntrospectionTypeRef = 
    | IntrospectionNamedTypeRef of IntrospectionNamedTypeRef
    | IntrospectionListTypeRef of IntrospectionListTypeRef
    | IntrospectionNonNullTypeRef of IntrospectionNonNullTypeRef

and IntrospectionNamedTypeRef = 
    { kind : string
      name : string }

and IntrospectionListTypeRef = 
    { kind : TypeKind
      ofType : IntrospectionTypeRef option }

and IntrospectionNonNullTypeRef = 
    { kind : TypeKind
      ofType : IntrospectionTypeRef option }

and IntrospectionField = 
    { name : string
      description : string option
      args : IntrospectionInputValue list
      Type : IntrospectionTypeRef
      isDeprecated : bool
      deprecationReason : string option }

and IntrospectionInputValue = 
    { name : string
      description : string option
      Type : IntrospectionTypeRef
      defaultValue : string option }

and IntrospectionEnumValue = 
    { name : string
      description : string option
      isDeprecated : bool
      deprecationReason : string option }

and IntrospectionDirective = 
    { name : string
      description : string option
      locations : DirectiveLocation list
      args : IntrospectionInputValue list }
