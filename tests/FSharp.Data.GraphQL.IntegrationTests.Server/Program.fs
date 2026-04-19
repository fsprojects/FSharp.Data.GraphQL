namespace FSharp.Data.GraphQL.IntegrationTests.Server

open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Hosting

module Program =
    let exitCode = 0

    let buildHost args =
        Host
            .CreateDefaultBuilder(args)
            .ConfigureWebHostDefaults(fun webBuilder ->
                webBuilder.UseStartup<Startup>() |> ignore)

    [<EntryPoint>]
    let main args =
        buildHost(args).Build().Run()
        exitCode
