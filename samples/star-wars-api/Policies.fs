namespace FSharp.Data.GraphQL.Samples.StarWarsApi.Authorization

open FSharp.Core
open Microsoft.AspNetCore.Authorization

module Policies =

    let [<Literal>] CanSetMoon = "CanSetMoon"

type IsCharacterRequierment (character : string Set) =
    member val Characters = character
    interface IAuthorizationRequirement

type IsCharacterHandler () =

    inherit AuthorizationHandler<IsCharacterRequierment> () // Inject services from DI

        override _.HandleRequirementAsync (context, requirement) =
            Async.StartImmediateAsTask(async {
                let allowedCharacters = requirement.Characters
                if context.User.Claims
                   |> Seq.where (fun c -> c.Type = "character")
                   |> Seq.exists (fun c -> allowedCharacters |> Set.contains c.Value)
                then context.Succeed requirement
                else () // Go to the next handler if registered
            }) :> _
