namespace FSharp.Data.GraphQL.Server.Middlewares

module internal Literals =
    module MetadataKeys =
        module QueryWeightMiddleware =
            let [<Literal>] QueryWeight = "queryWeight"
            let [<Literal>] QueryWeightThreshold = "queryWeightThreshold"
    module ArgumentKeys =        

        module ObjectListFilterMiddleware =
            let [<Literal>] Filter = "filter"