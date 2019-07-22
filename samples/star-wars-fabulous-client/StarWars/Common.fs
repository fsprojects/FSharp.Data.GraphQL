namespace StarWars

open FSharp.Data
open FSharp.Data.GraphQL

module Commands =

    type GraphQLApi = GraphQLProvider<"http://localhost:8084">
    let GetCharactersData = GraphQLApi.Operation<"queries/FetchCharacters.graphql">()

    type Character = GraphQLApi.Operations.FetchCharacters.Types.CharactersFields.Character
