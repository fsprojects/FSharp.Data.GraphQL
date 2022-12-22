namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

type Output = IDictionary<string, obj>

type GQLResponse =
    { DocumentId: int
      Data : Output
      Errors : GQLProblemDetails list }

type GQLExecutionResult =
    { DocumentId: int
      Content : GQLResponseContent
      Metadata : Metadata }
    static member Direct(documentId, data, errors, meta) =
        { DocumentId = documentId
          Content = Direct (data, errors)
          Metadata = meta }
    static member Deferred(documentId, data, errors, deferred, meta) =
        { DocumentId = documentId
          Content = Deferred (data, errors, deferred)
          Metadata = meta }
    static member Stream(documentId, data, meta) =
        { DocumentId = documentId
          Content = Stream data
          Metadata = meta }
    static member Empty(documentId, meta) =
        GQLExecutionResult.Direct(documentId, Map.empty, [], meta)
    static member Error(documentId, msg, meta) =
        GQLExecutionResult.Direct(documentId, Map.empty, [ GQLProblemDetails.Create msg ], meta)
    static member Invalid(documentId, errors, meta) =
        GQLExecutionResult.Direct(documentId, Map.empty, errors, meta)
    static member ErrorAsync(documentId, msg, meta) =
        asyncVal.Return (GQLExecutionResult.Error (documentId, msg, meta))

// TODO: Rename to PascalCase
and GQLResponseContent =
    | Direct of data : Output * errors: GQLProblemDetails list
    | Deferred of data : Output * errors : GQLProblemDetails list * defer : IObservable<GQLDeferredResponseContent>
    | Stream of stream : IObservable<GQLSubscriptionResponseContent>

and GQLDeferredResponseContent =
    | DeferredResult of data : obj * path : string list
    | DeferredErrors of data : obj * errors: GQLProblemDetails list * path : string list

and GQLSubscriptionResponseContent =
    | SubscriptionResult of data : Output
    | SubscriptionErrors of data : Output * errors: GQLProblemDetails list

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

    let (|BoxedSeq|_|) (xs : obj) =
        match xs with
        | (:? System.Collections.IEnumerable as enumerable) -> Some (Seq.cast<obj> enumerable)
        | _ -> None

    let rec structEq (x: NameValueLookup) (y: NameValueLookup) =
        if Object.ReferenceEquals(x, y) then true
        elif Object.ReferenceEquals(y, null) then false
        elif Object.ReferenceEquals(x, null) then false
        elif x.Count <> y.Count then false
        else
            x.Buffer
            |> Array.forall2 (fun (a: KeyValuePair<string, obj>) (b: KeyValuePair<string, obj>) ->
                if a.Key <> b.Key then false
                else
                    match a.Value, b.Value with
                    | (:? NameValueLookup as x), (:? NameValueLookup as y) -> structEq x y
                    | (BoxedSeq x), (BoxedSeq y) ->
                        if Seq.length x <> Seq.length y then false else Seq.forall2 (=) x y
                    | a1, b1 -> a1 = b1) y.Buffer
    let pad (sb: System.Text.StringBuilder) times =
        for _ in 0..times do sb.Append("\t") |> ignore
    let rec stringify (sb: System.Text.StringBuilder) deep (o:obj) =
        match o with
        | :? NameValueLookup as lookup ->
            if lookup.Count > 0 then
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
                stringify sb (deep + 1) i
                sb.Append(", ") |> ignore
            sb.Append("]") |> ignore
        | other ->
            if isNull other |> not
            then sb.Append(other.ToString()) |> ignore
            else sb.Append("null") |> ignore
        ()
    /// Returns raw content of the current lookup.
    member _.Buffer : KeyValuePair<string, obj> [] = kvals
    /// Return a number of entries stored in current lookup. It's fixed size.
    member _.Count = kvals.Length
    /// Updates an entry's value under given key. It will throw an exception
    /// if provided key cannot be found in provided lookup.
    member _.Update key value = setValue key value
    override x.Equals(other) =
        match other with
        | :? NameValueLookup as lookup -> structEq x lookup
        | _ -> false
    override _.GetHashCode() =
        let mutable hash = 0
        for kv in kvals do
            hash <- (hash*397) ^^^ (kv.Key.GetHashCode()) ^^^ (if isNull kv.Value then 0 else kv.Value.GetHashCode())
        hash
    override x.ToString() =
        let sb =Text.StringBuilder()
        stringify sb 1 x
        sb.ToString()
    interface IEquatable<NameValueLookup> with
        member x.Equals(other) = structEq x other
    interface System.Collections.IEnumerable with
        member _.GetEnumerator() = (kvals :> System.Collections.IEnumerable).GetEnumerator()
    interface IEnumerable<KeyValuePair<string, obj>> with
        member _.GetEnumerator() = (kvals :> IEnumerable<KeyValuePair<string, obj>>).GetEnumerator()
    interface IDictionary<string, obj> with
        member _.Add(_, _) = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member _.Add(_) = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member _.Clear() = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member _.Contains(item) = kvals |> Array.exists ((=) item)
        member _.ContainsKey(key) = kvals |> Array.exists (fun kv -> kv.Key = key)
        member _.CopyTo(array, arrayIndex) = kvals.CopyTo(array, arrayIndex)
        member x.Count = x.Count
        member _.IsReadOnly = true
        member _.Item
            with get (key) = getValue key
            and set (key) v = setValue key v
        member _.Keys = upcast (kvals |> Array.map (fun kv -> kv.Key))
        member _.Values = upcast (kvals |> Array.map (fun kv -> kv.Value))
        member _.Remove(_:string) =
            raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
            false
        member _.Remove(_:KeyValuePair<string,obj>) =
            raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
            false
        member _.TryGetValue(key, value) =
            match kvals |> Array.tryFind (fun kv -> kv.Key = key) with
            | Some kv -> value <- kv.Value; true
            | None -> value <- null; false
    new(t: (string * obj) list) =
        NameValueLookup(t |> List.map (fun (k, v) -> KeyValuePair<string,obj>(k, v)) |> List.toArray)
    new(t: string []) =
        NameValueLookup(t |> Array.map (fun k -> KeyValuePair<string,obj>(k, null)))

module NameValueLookup =
    /// Create new NameValueLookup from given list of key-value tuples.
    let ofList (l: (string * obj) list) = NameValueLookup(l)

