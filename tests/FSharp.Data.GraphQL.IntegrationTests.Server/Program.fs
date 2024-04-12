namespace FSharp.Data.GraphQL.IntegrationTests.Server

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting

module Program =
    let exitCode = 0

    let buildWebHost args =
        WebHost
            .CreateDefaultBuilder(args)
            .UseStartup<Startup>()

    [<EntryPoint>]
    let main args =
        buildWebHost(args).Build().Run()
        exitCode
