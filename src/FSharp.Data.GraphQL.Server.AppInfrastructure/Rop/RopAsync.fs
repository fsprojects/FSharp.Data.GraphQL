module FSharp.Data.GraphQL.Server.AppInfrastructure.RopAsync

open Rop
open System.Threading.Tasks

let bindToAsyncTaskR<'a, 'b, 'c> (f: 'a -> Task<RopResult<'b, 'c>>) result =
    task {
        let secondResult =
            match result with
            | Success (x, msgs) ->
              task {
                  let! secRes = f x
                  return secRes |> mergeMessages msgs
              }
            | Failure errs ->
              task {
                  return Failure errs
              }
        return! secondResult
    }