namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL

type ObjectFilterMiddleware<'ObjectType, 'ListType>() =
    let middleware = fun (ctx : SchemaCompileContext) (next : SchemaCompileContext -> unit) ->
        let modifyFields (object : ObjectDef<'ObjectType>) (fields : FieldDef<'ObjectType> seq) =
            let args = [ Define.Input("filter", ObjectListFilter) ]
            let fields = fields |> Seq.map (fun x -> x.WithArgs(args)) |> List.ofSeq
            object.WithFields(fields)
        let typesWithListFields =
            ctx.Schema.TypeMap.GetTypesWithListFields<'ObjectType, 'ListType>()
        let modifiedTypes =
            typesWithListFields 
            |> Seq.map (fun (object, fields) -> modifyFields object fields)
            |> Seq.cast<NamedDef>
        ctx.Schema.TypeMap.AddOrOverwriteTypes(modifiedTypes, overwrite = true)
        next ctx

    interface IExecutorMiddleware with
        member __.ExecuteOperationAsync = None
        member __.PlanOperation = None
        member __.CompileSchema = Some middleware