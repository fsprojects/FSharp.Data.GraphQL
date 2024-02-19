namespace FSharp.Data.GraphQL.Server.AspNetCore

module GraphQLQueryDecoding =
  open FSharp.Data.GraphQL
  open Rop
  open System
  open System.Text.Json
  open System.Text.Json.Serialization

  let private resolveVariables (serializerOptions : JsonSerializerOptions) (expectedVariables : Types.VarDef list) (variableValuesObj : JsonDocument) =
    try
      try
        if (not (variableValuesObj.RootElement.ValueKind.Equals(JsonValueKind.Object))) then
          let offendingValueKind = variableValuesObj.RootElement.ValueKind
          fail (sprintf "\"variables\" must be an object, but here it is \"%A\" instead" offendingValueKind)
        else
          let providedVariableValues = variableValuesObj.RootElement.EnumerateObject() |> List.ofSeq
          expectedVariables
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
          |> Map.ofList
          |> succeed
      with
        | :? JsonException as ex ->
          fail (sprintf "%s" (ex.Message))
        | :? GraphQLException as ex ->
          fail (sprintf "%s" (ex.Message))
        | ex ->
          printfn "%s" (ex.ToString())
          fail "Something unexpected happened during the parsing of this request."
    finally
      variableValuesObj.Dispose()

  let decodeGraphQLQuery (serializerOptions : JsonSerializerOptions) (executor : Executor<'a>) (operationName : string option) (variables : JsonDocument option) (query : string) =
    let executionPlanResult =
      try
        match operationName with
        | Some operationName ->
          executor.CreateExecutionPlan(query, operationName = operationName)
          |> succeed
        | None ->
          executor.CreateExecutionPlan(query)
          |> succeed
      with
        | :? JsonException as ex ->
          fail (sprintf "%s" (ex.Message))
        | :? GraphQLException as ex ->
          fail (sprintf "%s" (ex.Message))

    executionPlanResult
    |> bindR
        (function
          | Ok x -> Success (x, [])
          | Result.Error (_, problemDetails) ->
              Failure <|
                (problemDetails
                 |> List.map (fun (x: GQLProblemDetails) -> x.Message))
        )
    |> bindR
      (fun executionPlan ->
          match variables with
          | None -> succeed <| (executionPlan, Map.empty) // it's none of our business here if some variables are expected. If that's the case, execution of the ExecutionPlan will take care of that later (and issue an error).
          | Some variableValuesObj ->
            variableValuesObj
            |> resolveVariables serializerOptions executionPlan.Variables
            |> mapR (fun variableValsObj -> (executionPlan, variableValsObj))
      )
    |> mapR (fun (executionPlan, variables) ->
              { ExecutionPlan = executionPlan
                Variables = variables })