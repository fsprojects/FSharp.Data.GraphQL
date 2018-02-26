namespace FSharp.Data.GraphQL.Samples.GiraffeServer

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting

module Program =
    let exitCode = 0

    let BuildWebHost args =
        WebHost
            .CreateDefaultBuilder(args)
            .UseStartup<Startup>()
            .UseUrls("http://localhost:8084")
            .Build()

    [<EntryPoint>]
    let main args =
        BuildWebHost(args).Run()
        exitCode
