namespace StarWars

open FSharp.Data
open FSharp.Data.GraphQL

module Commands =

    [<Literal>]
    let IntrospectionPath = "../../../tests/FSharp.Data.GraphQL.IntegrationTests/introspection.json"

    type GraphQLApi = GraphQLProvider<IntrospectionPath>
    let GetCharactersData = GraphQLApi.Operation<"queries/FetchCharacters.graphql"> ()

    type Character = GraphQLApi.Operations.FetchCharacters.Types.CharactersFields.Character
