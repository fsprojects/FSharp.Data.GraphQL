namespace FSharp.Data.GraphQL

type DisplayNameAttribute(name: string) =
    inherit System.Attribute()
    member __.Name = name

/// Dummy type to wrap requested fields in a GraphQL query projection
type Fields([<System.ParamArray>] fields: obj[]) =
    class end

/// Dummy type to wrap a field with a selection in a GraphQL query projection
type Selection<'T> private () =
    new (fields: 'T[] option, selection: 'T->Fields) = Selection()
    new (fields: 'T option, selection: 'T->Fields) = Selection()
    new (fields: 'T[], selection: 'T->Fields) = Selection()
    new (fields: 'T, selection: 'T->Fields) = Selection()

/// Dummy type to wrap inline fragments, shouldn't be used directly.
/// Use the static method On of provided types instead
type InlineFragment(typeCondition: string, selection: obj->Fields) =
    class end

namespace FSharp.Data.GraphQL.Client

open System
open System.Collections.Generic
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

module QuotationHelpers =
    let makeExprArray (exprs: Expr list) =
        Expr.NewArray(typeof<obj>, exprs |> List.map (fun e -> Expr.Coerce(e, typeof<obj>)))

    #if FABLE_COMPILER
    open Fable
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.AST
    open Fable.AST.Fable.Util

    type GraphQLFableEmitter() =
        member __.BuildQuery (com: ICompiler) (info: Fable.ApplyInfo) =
            ImportCall (Naming.fableInjectFile, "graphqlBuildQuery", None, false, info.args)
            |> makeCall com info.range info.returnType

        member __.LaunchQuery (com: ICompiler) (info: Fable.ApplyInfo) =
            ImportCall (Naming.fableInjectFile, "graphqlLaunchQuery", None, false, info.args)
            |> makeCall com info.range info.returnType

    type GraphQLFablePlugin() =
        // Ignore cont (arg2) for now
        let getLaunchQueryBody com args =
            """fetch($0, {
                method: 'post',
                body: JSON.stringify({query: $3})
            })
            .then(resp => resp.json())
            .then(resp => new Promise((resolve, reject) => {
                if (resp.errors) {
                    reject(errors.join('\n'))
                }
                else {
                    resolve($1 ? resp.data[$1] : resp.data);
                }
            }))"""
            |> makeEmit args
            |> fun p -> CoreLibCall ("Async", Some "awaitPromise", false, [p])
            |> makeCall com None Fable.Any

        let getBuildQueryBody com args =
            """var i = 0, openBraces = 0, closeBraces = 0;
            while (closeBraces == 0 || closeBraces < 0) {
                switch($1[i]) {
                    case "{": openBraces++; break;
                    case "}": closeBraces++; break;
                }
                i++;
            }
            var queryFields = $1.substr(0, i), queryFragments = $1.substr(i);
            var args = $2.map((k, i) => k + ": " + JSON.stringify($3[i])).join(", ");
            "{ " + $0 + "(" + args + ") " + queryFields + " }" + queryFragments;"""
            |> makeEmit args

        interface IInjectPlugin with
            member __.Inject com =
              [ { new IInjection with
                    member __.Name = "graphqlLaunchQuery"
                    member __.ArgumentsLength = 4
                    member __.GetBody args = getLaunchQueryBody com args }
                { new IInjection with
                    member __.Name = "graphqlBuildQuery"
                    member __.ArgumentsLength = 4
                    member __.GetBody args = getBuildQueryBody com args } ]

    let getDynamicField (name: string) (expr: Expr) =
        <@@ (%%expr: obj)?(name) @@>

    let makeOption (optType: Type) (expr: Expr) =
        expr

    [<Emit(typeof<GraphQLFableEmitter>, "LaunchQuery")>]
    let launchQuery (serverUrl: string) (queryName: string) (cont: obj->'T) (query: string)
                    : Async<'T> =
        failwith "JS only"

    [<Emit(typeof<GraphQLFableEmitter>, "BuildQuery")>]
    let buildQuery (queryName: string) (queryFields: string)
                   (argNames: string[]) (argValues: obj[]): string =
        failwith "JS only"
    #else
    open System.Net
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq

    type private Selection =
        | Field of name: string * selectionSet: (Selection list) option
        | InlineFragment of typeCondition: string option * Selection list
        // TODO: Throw error if there's an empty selection set?
        override x.ToString() =
            match x with
            | Field(name, selectionSet) ->
                match selectionSet with
                | None -> name
                | Some selectionSet ->
                    selectionSet
                    |> Seq.map string
                    |> String.concat ","
                    |> sprintf "%s { %s }" name
            | InlineFragment(typeCondition, fields) ->
                let typeCondition =
                    match typeCondition with Some t -> "on " + t | None -> ""
                fields
                |> Seq.map string
                |> String.concat ","
                |> sprintf "...%s { %s }" typeCondition

    let extractFields (projection: Expr) =
        // Accessing nullable types injects a null equality check in the generated code
        let (|NullCheck|_|) = function
            | IfThenElse (Call (None, _, [_; Value(_, _)]),
                          Call (None, _, []),
                          Call (None, _, [Coerce(_, _)])) as e ->
                Some e
            | _ -> None
        let (|Fields|_|) = function
            | NewObject (cons, [ NewArray (_, args) ])
                when cons.DeclaringType.FullName = "FSharp.Data.GraphQL.Fields" ->
                Some args
            | _ -> None
        let (|Selection|_|) = function
            | NewObject (cons, [ fieldExpr; Lambda (_, Fields argExprs) ])
                when cons.DeclaringType.FullName.StartsWith("FSharp.Data.GraphQL.Selection`1") ->
                Some (fieldExpr, argExprs)
            | _ -> None
        let (|InlineFragment|_|) = function
            | Let(_, Lambda(_, Fields argExprs), NewObject(cons, [ Value (:? string as typeName, _); _ ]))
                when cons.DeclaringType.FullName.StartsWith("FSharp.Data.GraphQL.InlineFragment") ->
                Some (typeName, argExprs)
            | _ -> None
        let rec translatePropGet = function
            | Selection (fieldExpr, argExprs) as e ->
                match translatePropGet fieldExpr with
                | Field (name, _) -> Field (name, Some (List.map translatePropGet argExprs))
                | _ -> failwithf "Selection misses field name: %A" e
            | InlineFragment (typeName, argExprs) ->
                InlineFragment (Some typeName, List.map translatePropGet argExprs)
            // TODO HACK: It shouldn't be needed to access array elements
            | Let (_, Call(None, meth, _), body)
                when meth.Name = "GetArray" ->
                translatePropGet body
            | Let (_, Call(Some (Coerce (Var _, dicType)), meth, [ Value (propName, _) ]), NullCheck _)
            | Call (Some (Coerce (Var _, dicType)), meth, [ Value (propName, _) ])
                when dicType.Name = "IDictionary`2" && meth.Name = "get_Item" ->
                Field (unbox<string> propName, None)
            | PropertyGet (Some (Coerce (_, dicType)), prop, [ Value (propName, _) ])
                when dicType.Name = "IDictionary`2" && prop.Name = "Item" ->
                Field (unbox<string> propName, None)
            | PropertyGet (Some (Var _), prop, []) ->
                Field (prop.Name, None)
            | Coerce (e, _) -> translatePropGet e
            | e -> failwithf "Unsupported field: %A" e
        match projection with
        | Lambda (_, Fields args)
        | Lambda (_, Coerce (NewTuple args, _))
        | Lambda (_, NewTuple args) -> List.map translatePropGet args
        | Lambda (_, Coerce (arg, _))
        | Lambda (_, arg) -> [ translatePropGet arg ]
        | _ -> failwithf "Unsupported projection: %A" projection
        |> Seq.map string
        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
        |> String.concat ","
        |> sprintf "{%s}"

    let getDynamicField (name: string) (expr: Expr) =
        let dicType = typeof<IDictionary<string,obj>>
        let mi = dicType.GetMethod("get_Item")
        Expr.Call(Expr.Coerce(expr, dicType), mi, [Expr.Value(name)])

    let makeOption (optType: Type) (expr: Expr) =
        let optArg = optType.GetGenericArguments().[0]
        let cases =
            FSharpType.GetUnionCases(optType)
            |> Seq.map (fun case -> case.Name, case)
            |> Map
        let var = Var("instance", typeof<obj>)
        Expr.Let(var, expr,
            Expr.IfThenElse(
                <@@ %%Expr.Var(var) = null @@>,
                Expr.NewUnionCase(cases.["None"], []),
                Expr.NewUnionCase(cases.["Some"], [Expr.Coerce(Expr.Var var, optArg)])))

    let rec jsonToObject (token: JToken) =
        match token.Type with
        | JTokenType.Object ->
            token.Children<JProperty>()
            |> Seq.map (fun prop -> prop.Name, jsonToObject prop.Value)
            |> dict :> obj
        | JTokenType.Array ->
            token |> Seq.map jsonToObject |> Seq.toArray :> obj
        | _ ->
            (token :?> JValue).Value

    let launchRequest (serverUrl: string) (opName: string) (opField: string) (cont: obj->'T) (reqBody: string) =
        async {
            use client = new WebClient()
            client.Headers.Set("content-type", "application/json")
            let queryJson =
                // Options are problematic within quotations so we just use null here
                if opName <> null
                then opName + reqBody
                else reqBody
            let queryJson =
                dict [
                    "operationName", if opName <> null then opName else "query"
                    "query", queryJson
                    "variable", "null"
                ] |> JsonConvert.SerializeObject
            let! json = client.UploadStringTaskAsync(Uri(serverUrl), queryJson) |> Async.AwaitTask
            let res = JToken.Parse json |> jsonToObject :?> IDictionary<string,obj>
            if res.ContainsKey("errors") then
                res.["errors"] :?> obj[] |> Seq.map string |> String.concat "\n" |> failwith
            let data =
                if opField <> null
                then (res.["data"] :?> IDictionary<string,obj>).[opField]
                else res.["data"]
            return cont(data)
        }

    let buildQuery (queryName: string) (resFields: string)
                   (argNames: string[]) (argValues: obj[]) =
        let queryFields, queryFragments =
            if String.IsNullOrWhiteSpace resFields then "", "" else
            let mutable i = 0
            let mutable openBraces = 0
            let mutable closeBraces = 0
            while closeBraces = 0 || closeBraces < openBraces do
                match resFields.Chars(i) with
                | '{' -> openBraces <- openBraces + 1
                | '}' -> closeBraces <- closeBraces + 1
                | _ -> ()
                i <- i + 1
            resFields.Substring(0, i), resFields.Substring(i)
        Seq.zip argNames argValues
        |> Seq.map (fun (k,v) -> sprintf "%s: %s" k (JsonConvert.SerializeObject v))
        |> String.concat ", "
        |> fun args -> sprintf "{ %s(%s) %s }%s" queryName args queryFields queryFragments
    #endif
