namespace FSharp.Data.GraphQL.Samples.StarWarsApi.Middleware

open System
open System.Threading.Tasks
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Linq.RuntimeHelpers
open Microsoft.AspNetCore.Authorization
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Samples.StarWarsApi

type FieldPolicyMiddleware<'Val, 'Res> = ResolveFieldContext -> 'Val -> (ResolveFieldContext -> 'Val -> Async<'Res>) -> Async<'Res>

type internal CustomPolicyFieldDefinition<'Val, 'Res> (source : FieldDef<'Val, 'Res>, middleware : FieldPolicyMiddleware<'Val, 'Res>) =

    interface FieldDef<'Val, 'Res> with

        member _.Name = source.Name
        member _.Description = source.Description
        member _.DeprecationReason = source.DeprecationReason
        member _.TypeDef = source.TypeDef
        member _.Args = source.Args
        member _.Metadata = source.Metadata
        member _.Resolve =

            let changeAsyncResolver expr =
                let expr =
                    match expr with
                    | WithValue (_, _, e) -> e
                    | _ -> failwith "Unexpected resolver expression."
                let resolver =
                    <@ fun ctx input -> middleware ctx input (%%expr : ResolveFieldContext -> 'Val -> Async<'Res>) @>
                let compiledResolver = LeafExpressionConverter.EvaluateQuotation resolver
                Expr.WithValue (compiledResolver, resolver.Type, resolver)

            let changeSyncResolver expr =
                let expr =
                    match expr with
                    | WithValue (_, _, e) -> e
                    | _ -> failwith "Unexpected resolver expression."
                let resolver =
                    <@
                        fun ctx input ->
                            middleware ctx input (fun ctx input ->
                                ((%%expr : ResolveFieldContext -> 'Val -> 'Res) ctx input)
                                |> async.Return)
                    @>
                try
                    let compiledResolver = LeafExpressionConverter.EvaluateQuotation resolver
                    Expr.WithValue (compiledResolver, resolver.Type, resolver)
                with :? NotSupportedException as ex ->
                    let message =
                        $"F# compiler cannot convert '{source.Name}' field resolver expression to LINQ, use function instead"
                    raise (NotSupportedException (message, ex))

            match source.Resolve with
            | Sync (input, output, expr) -> Async (input, output, changeSyncResolver expr)
            | Async (input, output, expr) -> Async (input, output, changeAsyncResolver expr)
            | Undefined -> failwith "Field has no resolve function."
            | x -> failwith <| sprintf "Resolver '%A' is not supported." x

    interface IEquatable<FieldDef> with
        member _.Equals (other) = source.Equals (other)

    override _.Equals y = source.Equals y
    override _.GetHashCode () = source.GetHashCode ()
    override _.ToString () = source.ToString ()

[<AutoOpen>]
module TypeSystemExtensions =

    let handlePolicies (policies : string array) (ctx : ResolveFieldContext) value = async {

        let root : Root = downcast ctx.Context.RootValue
        let serviceProvider = root.ServiceProvider
        let authorizationService = serviceProvider.GetRequiredService<IAuthorizationService> ()
        let principal = serviceProvider.GetRequiredService<IHttpContextAccessor>().HttpContext.User

        let! authorizationResults =
            policies
            |> Seq.map (fun p -> authorizationService.AuthorizeAsync (principal, value, p))
            |> Seq.toArray
            |> Task.WhenAll
            |> Async.AwaitTask

        let failedRequirements =
            authorizationResults
            |> Seq.where (fun r -> not r.Succeeded)
            |> Seq.collect (fun r -> r.Failure.FailedRequirements)

        if Seq.isEmpty failedRequirements then
            return Ok ()
        else
            return Error "Forbidden"
    }

    [<Literal>]
    let AuthorizationPolicy = "AuthorizationPolicy"

    type FieldDef<'Val, 'Res> with

        member field.WithPolicyMiddleware<'Val, 'Res> (middleware : FieldPolicyMiddleware<'Val, 'Res>) : FieldDef<'Val, 'Res> =
            upcast CustomPolicyFieldDefinition (field, middleware)

        member field.WithAuthorizationPolicies<'Val, 'Res> ([<ParamArray>] policies : string array) : FieldDef<'Val, 'Res> =

            let middleware ctx value (resolver : ResolveFieldContext -> 'Val -> Async<'Res>) : Async<'Res> = async {
                let! result = handlePolicies policies ctx value
                match result with
                | Ok _ -> return! resolver ctx value
                | Error error -> return raise (GQLMessageException error)
            }

            field.WithPolicyMiddleware<'Val, 'Res> middleware
