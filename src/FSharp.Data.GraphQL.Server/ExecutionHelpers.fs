namespace FSharp.Data.GraphQL.Execution

open System
open System.Reflection
open System.Runtime.InteropServices;
open System.Collections.Generic
open System.Collections.Concurrent
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Resolve
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Planning
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Introspection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Reflection.FSharpReflectionExtensions

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
[<AutoOpen>]
module Helpers =
    let private getOperation = function
        | OperationDefinition odef -> Some odef
        | _ -> None
    
    let internal findOperation doc opName =
        match doc.Definitions |> List.choose getOperation, opName with
        | [def], _ -> Some def
        | defs, name -> 
            defs
            |> List.tryFind (fun def -> def.Name = name)

    let internal resolveFieldValues (fields: ExecutionInfo list) (variables: Map<string, obj>) =
        fields
        |> List.filter (fun info -> info.Include variables)
        |> List.map (fun info -> (info.Identifier, info))
        |> List.toArray

    let private collectDefaultArgValue acc (argdef: InputFieldDef) =
        match argdef.DefaultValue with
        | Some defVal -> Map.add argdef.Name defVal acc
        | None -> acc
    
    let internal argumentValue variables (argdef: InputFieldDef) (argument: Argument) =
        match argdef.ExecuteInput argument.Value variables with
        | null -> argdef.DefaultValue
        | value -> Some value    
    
    let internal getArgumentValues (argDefs: InputFieldDef []) (args: Argument list) (variables: Map<string, obj>) : Map<string, obj> = 
        argDefs
        |> Array.fold (fun acc argdef -> 
            match List.tryFind (fun (a: Argument) -> a.Name = argdef.Name) args with
            | Some argument ->
                match argumentValue variables argdef argument with
                | Some v -> Map.add argdef.Name v acc
                | None -> acc
            | None -> collectDefaultArgValue acc argdef
        ) Map.empty