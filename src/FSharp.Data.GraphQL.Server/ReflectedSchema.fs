/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Reflection

type ObjectAttribute(?name: string) =
    inherit Attribute()
    
type FieldAttribute(?name: string) =
    inherit Attribute()
    
type InterfaceAttribute(?name: string) =
    inherit Attribute()
    
type UnionAttribute(?name: string) =
    inherit Attribute()

type ReflectedSchema() = 
    static member FromType<'a>() = ()
    static member FromType(t: Type) = ()
    static member FromAssembly(assembly: Assembly) = ()
