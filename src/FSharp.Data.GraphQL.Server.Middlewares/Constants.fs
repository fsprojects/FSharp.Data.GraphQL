namespace FSharp.Data.GraphQL.Server.Middlewares

module internal Constants =
    module internal MetadataKeys =
        let [<Literal>] queryWeight = "queryWeight"
        let [<Literal>] queryWeightThreshold = "queryWeightThreshold"