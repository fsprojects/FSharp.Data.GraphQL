namespace FSharp.Data.GraphQL.Server.Middleware

open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types

type QueryTreeField =
    { Alias : string option
      Name : string
      Directives : Directive list
      Arguments : Argument list
      Kind : QueryTreeFieldKind }

and QueryTreeFieldKind =
    | ValueField
    | SelectionField of QueryTreeField list
    | CollectionField of QueryTreeField
    | AbstractionField of Map<string, QueryTreeField list>
    | DeferredField of QueryTreeField
    | StreamedField of QueryTreeField * BufferedStreamOptions
    | LiveField of QueryTreeField

module QueryTreeField =
    let rec fromExecutionInfoField (info : ExecutionInfo) =
        let fromField (kind : QueryTreeFieldKind) (field : Field) =
            { Alias = field.Alias
              Name = field.Name
              Directives = field.Directives
              Arguments = field.Arguments
              Kind = kind }
        let kind =
            match info.Kind with
            | ResolveValue -> ValueField
            | SelectFields fields -> SelectionField (List.map fromExecutionInfoField fields)
            | ResolveCollection innerInfo -> CollectionField (fromExecutionInfoField innerInfo)
            | ResolveAbstraction typeFields ->
                let typeFields = typeFields |> Map.map (fun _ info -> info |> List.map fromExecutionInfoField)
                AbstractionField typeFields
            | ResolveDeferred innerInfo -> DeferredField (fromExecutionInfoField innerInfo)
            | ResolveStreamed (innerInfo, bufferOptions) -> StreamedField (fromExecutionInfoField innerInfo, bufferOptions)
            | ResolveLive innerInfo -> LiveField (fromExecutionInfoField innerInfo)
        fromField kind info.Ast

type QueryTreeOperation =
    { OperationType : OperationType
      Fields : QueryTreeField list }

type QueryTree =
    { Operation : QueryTreeOperation
      // TODO: how should we best encode variables for serialization?
      Variables : Map<string, obj> }

module QueryTree =
    let fromPlannedQuery (plan : ExecutionPlan) (variables : Map<string, obj>) =
        let operation =
            { OperationType = plan.Operation.OperationType
              Fields = plan.Fields |> List.map QueryTreeField.fromExecutionInfoField }
        { Operation = operation; Variables = variables }