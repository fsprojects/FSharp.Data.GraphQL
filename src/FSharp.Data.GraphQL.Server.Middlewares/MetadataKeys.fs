namespace FSharp.Data.GraphQL.Server.Middlewares

module internal MetadataKeys =
    module QueryWeightMiddleware =
        let [<Literal>] QueryWeight = "queryWeight"
        let [<Literal>] QueryWeightThreshold = "queryWeightThreshold"

    module QueryExtensionsMiddleware =
        let [<Literal>] SortableBy = "sortableBy"