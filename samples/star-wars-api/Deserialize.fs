module FSharp.Data.GraphQL.Samples.StarWarsApi.Deserialize

open System
open Newtonsoft.Json.Linq
open FSharp.Data.GraphQL.Ast

let rec jTokenToAstValue (j : JToken) =
  match j.Type with
  | JTokenType.Null -> NullValue
  | JTokenType.Boolean -> BooleanValue (j.ToObject<bool>())
  | JTokenType.Integer -> IntValue (j.ToObject<int>() |> int64)
  | JTokenType.Float -> FloatValue (j.ToObject<float>())
  | JTokenType.String -> StringValue (j.ToObject<string>())
  | JTokenType.Guid -> StringValue (j.ToObject<Guid>().ToString())
  | JTokenType.Uri -> StringValue (j.ToObject<Uri>().OriginalString)
  | JTokenType.Date -> StringValue (j.ToObject<DateTime>().ToString("o"))
  | JTokenType.Array ->
    j
    |> Seq.map jTokenToAstValue
    |> Seq.toList
    |> ListValue
  | JTokenType.Object ->
    (j :?> JObject).Properties()
    |> Seq.map (fun prop ->
        prop.Name, jTokenToAstValue prop.Value)
    |> Map.ofSeq
    |> ObjectValue
  | _ -> failwithf "Unsupported JToken.Type %A for %A" j.Type j

let deserializeQueryAndVariables (data : string) =
  if String.IsNullOrWhiteSpace(data)
  then
    None
  else
    let j = JObject.Parse data

    printfn "%A" data
    printfn "%A" (Seq.toList j)

    let query =
      try
        j.Property("query").ToObject<string>()
      with _ ->
        failwith "Failure deserializing response. Could not read query - it is not stringified in request."

    printfn "query = %A" query

    let variables =
      match j.TryGetValue("variables") with
      | (true, v) ->
        let variablesString = v.ToObject<string>()

        if String.IsNullOrWhiteSpace variablesString
        then
          None
        else
          let j = JToken.Parse(variablesString)

          match j.Type with
          | JTokenType.Null -> Some Map.empty
          | JTokenType.Object ->
            let j = j :?> JObject

            j.Properties()
            |> Seq.map (fun x -> x.Name, jTokenToAstValue x.Value)
            |> Map.ofSeq
            |> Some
          | _ -> failwithf "variables property must be a JSON object"
      | _ -> None

    Some (query, variables)
