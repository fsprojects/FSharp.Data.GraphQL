namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL

type ObjectFilterMiddleware<'Val, 'Res>() =
    let middleware = fun (ctx : SchemaCompileContext) (next : SchemaCompileContext -> unit) ->
        let addArgsAndResolver (field : FieldDef<'Val>) = 
            field.WithArgs([ Define.ObjectFilterInput ])
                 .WithResolveMiddleware<'Res seq>(fun ctx input next ->
                    printfn "%A" (ctx.Args |> Map.toList)
                    next ctx input)
        let modifyFields (object : ObjectDef<'Val>) (fields : FieldDef<'Val> seq) =
            object.WithFields(fields |> Seq.map addArgsAndResolver)
        let typesWithListFields =
            ctx.Schema.TypeMap.GetTypesWithListFields<'Val, 'Res>()
        let modifiedTypes =
            typesWithListFields 
            |> Seq.map (fun (o, fields) -> fields |> modifyFields o)
            |> Seq.cast<NamedDef>
        ctx.Schema.TypeMap.AddOrOverwriteTypes(modifiedTypes, overwrite = true)
        next ctx

    interface IExecutorMiddleware with
        member __.ExecuteOperationAsync = None
        member __.PlanOperation = None
        member __.CompileSchema = Some middleware