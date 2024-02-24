namespace FSharp.Data.GraphQL.Server.AspNetCore

module GraphQLQueryDecoding =
  open FSharp.Data.GraphQL
  open System
  open System.Text.Json
  open System.Text.Json.Serialization
  open FsToolkit.ErrorHandling

  let genericErrorContentForDoc (docId: int) (message: string) =
    struct (docId, [GQLProblemDetails.Create ((sprintf "%s" message), Skip)])

  let genericFinalErrorForDoc (docId: int) (message: string) =
    Result.Error (genericErrorContentForDoc docId message)

  let genericFinalError message =
    message |> genericFinalErrorForDoc -1

  let private resolveVariables (serializerOptions : JsonSerializerOptions) (expectedVariables : Types.VarDef list) (variableValuesObj : JsonDocument) =
    result {
      try
        try
          if (not (variableValuesObj.RootElement.ValueKind.Equals(JsonValueKind.Object))) then
            let offendingValueKind = variableValuesObj.RootElement.ValueKind
            return! Result.Error (sprintf "\"variables\" must be an object, but here it is \"%A\" instead" offendingValueKind)
          else
            let providedVariableValues = variableValuesObj.RootElement.EnumerateObject() |> List.ofSeq
            return! Ok
              (expectedVariables
              |> List.choose
                  (fun expectedVariable ->
                    providedVariableValues
                    |> List.tryFind(fun x -> x.Name = expectedVariable.Name)
                    |> Option.map
                      (fun providedValue ->
                        let boxedValue =
                          if providedValue.Value.ValueKind.Equals(JsonValueKind.Null) then
                            null :> obj
                          elif providedValue.Value.ValueKind.Equals(JsonValueKind.String) then
                            providedValue.Value.GetString() :> obj
                          else
                            JsonSerializer.Deserialize(providedValue.Value, serializerOptions) :> obj
                        (expectedVariable.Name, boxedValue)
                      )
                  )
              |> Map.ofList)
        with
          | :? JsonException as ex ->
            return! Result.Error (sprintf "%s" (ex.Message))
          | :? GraphQLException as ex ->
            return! Result.Error (sprintf "%s" (ex.Message))
          | ex ->
            printfn "%s" (ex.ToString())
            return! Result.Error ("Something unexpected happened during the parsing of this request.")
      finally
        variableValuesObj.Dispose()
    }

  let decodeGraphQLQuery (serializerOptions : JsonSerializerOptions) (executor : Executor<'a>) (operationName : string option) (variables : JsonDocument option) (query : string) =
    let executionPlanResult =
      result {
        try
          match operationName with
          | Some operationName ->
              return! executor.CreateExecutionPlan(query, operationName = operationName)
          | None ->
              return! executor.CreateExecutionPlan(query)
        with
          | :? JsonException as ex ->
            return! genericFinalError (sprintf "%s" (ex.Message))
          | :? GraphQLException as ex ->
            return! genericFinalError (sprintf "%s" (ex.Message))
      }

    executionPlanResult
    |> Result.bind
      (fun executionPlan ->
          match variables with
          | None -> Ok <| (executionPlan, Map.empty) // it's none of our business here if some variables are expected. If that's the case, execution of the ExecutionPlan will take care of that later (and issue an error).
          | Some variableValuesObj ->
            variableValuesObj
            |> resolveVariables serializerOptions executionPlan.Variables
            |> Result.map (fun variableValsObj -> (executionPlan, variableValsObj))
            |> Result.mapError (fun x -> sprintf "%s" x |> genericErrorContentForDoc executionPlan.DocumentId)
      )
    |> Result.map (fun (executionPlan, variables) ->
              { ExecutionPlan = executionPlan
                Variables = variables })