namespace StarWars

open Fabulous.Core
open Fabulous.DynamicViews
open FSharp.Data
open FSharp.Data.GraphQL
open Xamarin.Forms.PlatformConfiguration.AndroidSpecific
open Xamarin.Forms
open Style
open Commands
open Helpers

module MainPage =

    type Model =
      { Characters: Character[] option
        ErrorMessage : string option
        RuntimeContext : GraphQLProviderRuntimeContext }

    type Msg =
        | CharactersQueryResult of Character[]
        | CharacterSelected of Character
        | Error of message : string

    type ExternalMsg =
        | NoOp
        | NavigateToCharacterDetail of Character

    let initModel (runtimeCtx:GraphQLProviderRuntimeContext) =
        { Characters = None
          ErrorMessage = None
          RuntimeContext = runtimeCtx }

    let getCharactersData context =
        async {
            let! result = Commands.GetCharactersData.AsyncRun(runtimeContext=context)
            let msg =
                match result.Data with
                | Some q ->
                    CharactersQueryResult q.Characters
                | None ->
                    Error "Failed to load viewer name"
            return msg
        }
        |> Cmd.ofAsyncMsg

    let init runtimeCtx =
        let model = initModel runtimeCtx
        model, getCharactersData runtimeCtx, ExternalMsg.NoOp
        
    let update (msg:Msg) (model:Model) =
        match msg with
        | CharactersQueryResult c ->
            let m = { model with Characters = Some c; ErrorMessage = None }
            m, Cmd.none, ExternalMsg.NoOp
        | CharacterSelected c ->
            model, Cmd.none, ExternalMsg.NavigateToCharacterDetail c
        | Error message ->
            displayAlert("Request Failed", message, "OK") |> Async.Start
            let m = { model with ErrorMessage = Some message }
            m, Cmd.none, ExternalMsg.NoOp

    let getCharacterName (character:string option) =
        match character with
        | None -> "Loading..."
        | Some v -> v

    let buildListViewElements (queryResult:Character[] option) =
        match queryResult with
        | None -> 
            [ View.Label(text = "Loading communities...",
                         horizontalOptions = LayoutOptions.Center,
                         widthRequest = 200.0,
                         horizontalTextAlignment = TextAlignment.Center) ]
        | Some characters ->
            characters
            |> Array.map (fun c ->
                let cName =
                    match c.IsHuman() with
                    | true -> c.AsHuman().Name
                    | _ -> c.AsDroid().Name

                View.Label(text = sprintf "%s" (getCharacterName cName),
                     horizontalOptions = LayoutOptions.Center,
                     widthRequest = 200.0,
                     horizontalTextAlignment = TextAlignment.Center))
            |> Array.toList

    let findCharacter (characters:Character[]) idx = characters.[idx]

    let mkChildren model dispatch =
        let characters =
            Option.defaultValue [||] model.Characters
        let label =
            View.Label(text = sprintf "Select a character",
                         horizontalOptions = LayoutOptions.Center,
                         widthRequest = 200.0,
                         heightRequest = 30.0,
                         horizontalTextAlignment = TextAlignment.Center)
        let listView =
            View.ListView(items = (buildListViewElements model.Characters),
                          horizontalOptions = LayoutOptions.CenterAndExpand,
                          verticalOptions = LayoutOptions.FillAndExpand,
                          selectionMode = ListViewSelectionMode.None,
                          itemTapped = (findCharacter characters >> CharacterSelected >> dispatch))
        [ label; listView]

    let view (model: Model) dispatch =
        dependsOn model (fun model mModel ->
            View.ContentPage(
                title = "Star Wars",
                content = View.StackLayout(padding = 20.0,
                                           verticalOptions = LayoutOptions.Center,
                                            children = mkChildren mModel dispatch)
            )
        )