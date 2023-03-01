namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration


module Program =
    let exitCode = 0

    let buildWebHost (args: string array) =

        // Build an initial configuration that takes in both environment and command line settings.
        let config = ConfigurationBuilder()
                            .AddEnvironmentVariables()
                            .AddCommandLine(args)
                            .Build()

        // This is done so that an environment specified on the command line with "--environment" is respected,
        // when we look for appsettings.*.json files.
        let configureAppConfiguration (context: WebHostBuilderContext) (config: IConfigurationBuilder) =

            // The default IFileProvider has the working directory set to the same as the DLL.
            // We'll use this to re-set the FileProvider in the configuration builder below.
            let fileProvider = ConfigurationBuilder().GetFileProvider()

            // Extract the environment name from the configuration that was already built above
            let envName = context.HostingEnvironment.EnvironmentName
            // Use the name to find the additional appsettings file, if present.
            config.SetFileProvider(fileProvider)
                    .AddJsonFile("appsettings.json", false, true)
                    .AddJsonFile($"appsettings.{envName}.json", true) |> ignore

        WebHost
            .CreateDefaultBuilder(args)
            .UseConfiguration(config)
            .ConfigureAppConfiguration(configureAppConfiguration)
            .UseStartup<Startup>()

    [<EntryPoint>]
    let main args =
        buildWebHost(args).Build().Run()
        exitCode
