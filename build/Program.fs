module Program

open System
open System.IO
open System.Net.Http
open System.Text.Json

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Tools

Path.Combine (__SOURCE_DIRECTORY__, "..")
|> Path.GetFullPath
|> Directory.SetCurrentDirectory

let execContext =
    let args = Environment.GetCommandLineArgs() |> Seq.skip 1 |> Seq.toList
    Fake.Core.Context.FakeExecutionContext.Create false "build.fsx" args
execContext
|> Fake.Core.Context.RuntimeContext.Fake
|> Fake.Core.Context.setExecutionContext

module DotNetCli =
    let setVersion (o : DotNet.Options) = { o with Version = Some "8.0.202" }
    let setRestoreOptions (o : DotNet.RestoreOptions) = o.WithCommon setVersion

let configurationString = Environment.environVarOrDefault "CONFIGURATION" "Release"
let configuration =
    match configurationString with
    | "Debug" -> DotNet.BuildConfiguration.Debug
    | "Release" -> DotNet.BuildConfiguration.Release
    | config -> DotNet.BuildConfiguration.Custom config

// --------------------------------------------------------------------------------------
// Information about the project are used
// --------------------------------------------------------------------------------------
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

[<Literal>]
let DotNetMoniker = "net8.0"

let project = "FSharp.Data.GraphQL"
let release = ReleaseNotes.load "RELEASE_NOTES.md"
let projectRepo = "https://github.com/fsprojects/FSharp.Data.GraphQL.git"

// --------------------------------------------------------------------------------------
// Clean build results

Target.create "Clean" <| fun _ -> Shell.cleanDirs [ "bin"; "temp" ]

Target.create "CleanDocs" <| fun _ -> Shell.cleanDirs [ "docs/output" ]

// --------------------------------------------------------------------------------------
// Build library & test project

let [<Literal>] RestoreTarget = "Restore"
Target.create RestoreTarget <| fun _ ->
    !! "src/**/*.??proj" -- "src/**/*.shproj"
    |> Seq.iter (fun pattern -> DotNet.restore DotNetCli.setRestoreOptions pattern)


let [<Literal>] BuildTarget = "Build"
Target.create BuildTarget <| fun _ ->
    "FSharp.Data.GraphQL.sln"
    |> DotNet.build (fun options -> {
        options with
            Configuration = configuration
            MSBuildParams = { options.MSBuildParams with DisableInternalBinLog = true }
    })

let startGraphQLServer (project : string) port (streamRef : DataRef<Stream>) =
    DotNet.build
        (fun options -> {
            options with
                Configuration = configuration
                MSBuildParams = { options.MSBuildParams with DisableInternalBinLog = true }
        })
        project

    let projectName = Path.GetFileNameWithoutExtension (project)
    let projectPath = Path.GetDirectoryName (project)

    let serverExe =
        projectPath
        </> "bin"
        </> configurationString
        </> DotNetMoniker
        </> (projectName + ".dll")

    CreateProcess.fromRawCommandLine "dotnet" $"{serverExe} --configuration {configurationString} --urls=http://localhost:%i{port}/"
    |> CreateProcess.withStandardInput (CreatePipe streamRef)
    |> Proc.start
    |> ignore

    System.Threading.Thread.Sleep (2000)

let runTests (project : string) (args : string) =
    DotNet.build
        (fun options -> {
            options with
                Framework = Some DotNetMoniker
                Configuration = configuration
                MSBuildParams = { options.MSBuildParams with DisableInternalBinLog = true }
        })
        project

    let customParams = String.Join (' ', "--no-build -v=normal", args)

    DotNet.test
        (fun options ->
            {
                options with
                    Framework = Some DotNetMoniker
                    Configuration = configuration
                    MSBuildParams = {
                        options.MSBuildParams with
                            DisableInternalBinLog = true
                            Verbosity = Some Normal
                    }
                    Common = { options.Common with CustomParams = Some customParams }
            }
                .WithCommon
                DotNetCli.setVersion)
        project

let starWarsServerStream = StreamRef.Empty

let [<Literal>] StartStarWarsServerTarget = "StartStarWarsServer"
Target.create StartStarWarsServerTarget <| fun _ ->
    Target.activateFinal "StopStarWarsServer"

    let project =
        "samples"
        </> "star-wars-api"
        </> "FSharp.Data.GraphQL.Samples.StarWarsApi.fsproj"

    startGraphQLServer project 8086 starWarsServerStream

let [<Literal>] StopStarWarsServerTarget = "StopStarWarsServer"
Target.createFinal StopStarWarsServerTarget <| fun _ ->
    try
        starWarsServerStream.Value.Write ([| 0uy |], 0, 1)
    with e ->
        printfn "%s" e.Message

let integrationServerStream = StreamRef.Empty

let [<Literal>] StopIntegrationServerTarget = "StopIntegrationServer"
let [<Literal>] StartIntegrationServerTarget = "StartIntegrationServer"
Target.create StartIntegrationServerTarget <| fun _ ->
    Target.activateFinal StopIntegrationServerTarget

    let project =
        "tests"
        </> "FSharp.Data.GraphQL.IntegrationTests.Server"
        </> "FSharp.Data.GraphQL.IntegrationTests.Server.fsproj"

    startGraphQLServer project 8085 integrationServerStream

Target.createFinal StopIntegrationServerTarget <| fun _ ->
    try
        integrationServerStream.Value.Write ([| 0uy |], 0, 1)
    with e ->
        printfn "%s" e.Message

let [<Literal>] UpdateIntrospectionFileTarget = "UpdateIntrospectionFile"
Target.create UpdateIntrospectionFileTarget <| fun _ ->
    let client = new HttpClient ()
    (task {
        let! result = client.GetAsync ("http://localhost:8086")
        let! contentStream = result.Content.ReadAsStreamAsync ()
        let! jsonDocument = JsonDocument.ParseAsync contentStream
        let file =
            new FileStream ("tests/FSharp.Data.GraphQL.IntegrationTests/introspection.json", FileMode.Create, FileAccess.Write, FileShare.None)
        let encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
        let jsonWriterOptions = JsonWriterOptions (Indented = true, Encoder = encoder)
        let writer = new Utf8JsonWriter (file, jsonWriterOptions)
        jsonDocument.WriteTo writer
        do! writer.FlushAsync ()
        do! writer.DisposeAsync ()
        do! file.DisposeAsync ()
        result.Dispose ()
    })
        .Wait ()
    client.Dispose ()

let [<Literal>] RunUnitTestsTarget = "RunUnitTests"
Target.create RunUnitTestsTarget <| fun _ ->
    runTests "tests/FSharp.Data.GraphQL.Tests/FSharp.Data.GraphQL.Tests.fsproj" ""

let [<Literal>] RunIntegrationTestsTarget = "RunIntegrationTests"
Target.create RunIntegrationTestsTarget <| fun _ ->
    runTests "tests/FSharp.Data.GraphQL.IntegrationTests/FSharp.Data.GraphQL.IntegrationTests.fsproj" "" //"--filter Execution=Sync"

let prepareDocGen () =
    Shell.rm "docs/release-notes.md"
    Shell.cp "RELEASE_NOTES.md" "docs/RELEASE_NOTES.md"
    Shell.rename "docs/release-notes.md" "docs/RELEASE_NOTES.md"

    Shell.rm "docs/license.md"
    Shell.cp "LICENSE.txt" "docs/LICENSE.txt"
    Shell.rename "docs/license.md" "docs/LICENSE.txt"

    Shell.cleanDir ".fsdocs"

let [<Literal>] GenerateDocsTarget = "GenerateDocs"
Target.create GenerateDocsTarget <| fun _ ->
    prepareDocGen ()
    DotNet.exec DotNetCli.setVersion "fsdocs" "build --clean" |> ignore

let [<Literal>] GenerateDocsWatchTarget = "GenerateDocsWatch"
Target.create GenerateDocsWatchTarget <| fun _ ->
    prepareDocGen ()
    DotNet.exec DotNetCli.setVersion "fsdocs" "watch --clean" |> ignore
    System.Console.ReadKey () |> ignore

let [<Literal>] ReleaseDocsTarget = "ReleaseDocs"
Target.create ReleaseDocsTarget <| fun _ ->
    Git.Repository.clone "" projectRepo "temp/gh-pages"
    Git.Branches.checkoutBranch "temp/gh-pages" "gh-pages"

    Shell.copyRecursive "output" "temp/gh-pages" true
    |> printfn "%A"

    Git.CommandHelper.runSimpleGitCommand "temp/gh-pages" "add ."
    |> printfn "%s"

    let cmd = sprintf """commit -a -m "Update generated documentation for version %s""" release.NugetVersion

    Git.CommandHelper.runSimpleGitCommand "temp/gh-pages" cmd
    |> printfn "%s"

    Git.Branches.push "temp/gh-pages"

let getPackageName id = sprintf "%s.%s" project id

let getPackageDir packageName = sprintf "nuget/%s" packageName

let getProjectPath packageName = sprintf "src/%s/%s.fsproj" packageName packageName

let pack id =
    let packageName = getPackageName id
    let packageDir = getPackageDir packageName
    let projectPath = getProjectPath packageName

    Shell.cleanDir packageDir

    projectPath
    |> DotNet.pack (fun p ->
        {
            p with
                Common = { p.Common with Version = Some release.NugetVersion }
                NoLogo = true
                OutputPath = Some packageDir
                MSBuildParams = { p.MSBuildParams with Properties = [("IsNuget", "true")] }
        }
            .WithCommon
            DotNetCli.setVersion)

type PushSource =
    | NuGet
    | GitHub

let push id =
    let packageName = getPackageName id
    let packageDir = getPackageDir packageName
    let packageFile = packageName + ".nupkg"

    let source =
#if NuGet
        NuGet
#else
        GitHub
#endif

    let apiKeyVariableName, source =
        match source with
        | GitHub -> "GITHUB_TOKEN", Some "github"
        | NuGet -> "NUGET_SECRET", None

    packageFile
    |> DotNet.nugetPush (fun p ->
        {
            p with
                Common = { p.Common with WorkingDirectory = packageDir }
                PushParams = {
                    p.PushParams
                        with
                            ApiKey = Some (Environment.GetEnvironmentVariable apiKeyVariableName)
                            Source = source
                            SkipDuplicate = true
                }
        }
            .WithCommon DotNetCli.setVersion)

let [<Literal>] PackSharedTarget = "PackShared"
Target.create PackSharedTarget <| fun _ -> pack "Shared"

let [<Literal>] PackServerTarget = "PackServer"
Target.create PackServerTarget <| fun _ -> pack "Server"

let [<Literal>] PackServerAspNetCore = "PackServerAspNetCore"
Target.create "PackServerAspNetCore" <| fun _ -> pack "Server.AspNetCore"

let [<Literal>] PackServerGiraffe = "PackServerGiraffe"
Target.create "PackServerGiraffe" <| fun _ -> pack "Server.Giraffe"

let [<Literal>] PackServerOxpecker = "PackServerOxpecker"
Target.create "PackServerOxpecker" <| fun _ -> pack "Server.Oxpecker"

let [<Literal>] PackClientTarget = "PackClient"
Target.create PackClientTarget <| fun _ -> pack "Client"

let [<Literal>] PackMiddlewareTarget = "PackMiddleware"
Target.create PackMiddlewareTarget <| fun _ -> pack "Server.Middleware"

let [<Literal>] PackRelayTarget = "PackRelay"
Target.create PackRelayTarget <| fun _ -> pack "Server.Relay"

let [<Literal>] PushSharedTarget = "PushShared"
Target.create PushSharedTarget <| fun _ -> push "Shared"

let [<Literal>] PushServerTarget = "PushServer"
Target.create PushServerTarget <| fun _ -> push "Server"

let [<Literal>] PushServerAspNetCore = "PushServerAspNetCore"
Target.create "PushServerAspNetCore" <| fun _ -> push "Server.AspNetCore"

let [<Literal>] PushServerGiraffe = "PushServerGiraffe"
Target.create "PushServerGiraffe" <| fun _ -> push "Server.Giraffe"

let [<Literal>] PushServerOxpecker = "PushServerOxpecker"
Target.create "PushServerOxpecker" <| fun _ -> push "Server.Oxpecker"

let [<Literal>] PushClientTarget = "PushClient"
Target.create PushClientTarget <| fun _ -> push "Client"

let [<Literal>] PushMiddlewareTarget = "PushMiddleware"
Target.create PushMiddlewareTarget <| fun _ -> push "Server.Middleware"

let [<Literal>] PushRelayTarget = "PushRelay"
Target.create PushRelayTarget <| fun _ -> push "Server.Relay"


// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build --target <Target>' to override

Target.create "All" ignore
Target.create "PackAndPush" ignore

"Clean"
    ==> RestoreTarget
    ==> BuildTarget
    ==> RunUnitTestsTarget
    ==> StartStarWarsServerTarget
    ==> StartIntegrationServerTarget
    ==> UpdateIntrospectionFileTarget
    ==> RunIntegrationTestsTarget
    ==> "All"
    =?> (GenerateDocsTarget, Environment.environVar "GITHUB_ACTIONS" = "True")
    |> ignore

"CleanDocs"
    ==> GenerateDocsTarget
    |> ignore

PackSharedTarget
    ==> PushSharedTarget
    ==> PackClientTarget
    ==> PushClientTarget
    ==> PackServerTarget
    ==> PushServerTarget
    ==> PackServerAspNetCore
    ==> PushServerAspNetCore
    ==> PackServerGiraffe
    ==> PushServerGiraffe
    ==> PackServerOxpecker
    ==> PushServerOxpecker
    ==> PackMiddlewareTarget
    ==> PushMiddlewareTarget
    ==> PackRelayTarget
    ==> PushRelayTarget
    ==> "PackAndPush"
    |> ignore

Target.runOrDefaultWithArguments "All"

execContext.Context.Clear ()
