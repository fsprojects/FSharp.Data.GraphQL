namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting

module Program =
    let exitCode = 0

    let [<Literal>] BaseAddress = "localhost:8084"

    let buildWebHost args =
        WebHost
            .CreateDefaultBuilder(args)
            .UseStartup<Startup>()
            .UseUrls(sprintf "http://%s" BaseAddress)

    [<EntryPoint>]
    let main args =
        buildWebHost(args).Build().Run()
        exitCode
