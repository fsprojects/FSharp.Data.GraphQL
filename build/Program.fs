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

Path.Combine(__SOURCE_DIRECTORY__, "..")
|> Path.GetFullPath
|> Directory.SetCurrentDirectory

let execContext =
    let args = Environment.GetCommandLineArgs() |> Seq.skip 1 |> Seq.toList
    Fake.Core.Context.FakeExecutionContext.Create false "build.fsx" args
execContext
|> Fake.Core.Context.RuntimeContext.Fake
|> Fake.Core.Context.setExecutionContext

module DotNetCli =
    let setVersion (o: DotNet.Options) = { o with Version = Some "7.0.401" }
    let setRestoreOptions (o: DotNet.RestoreOptions)= o.WithCommon  setVersion

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
let DotNetMoniker = "net6.0"

let project = "FSharp.Data.GraphQL"
let release = ReleaseNotes.load "RELEASE_NOTES.md"
let projectRepo = "https://github.com/fsprojects/FSharp.Data.GraphQL.git"

// --------------------------------------------------------------------------------------
// Clean build results

Target.create "Clean" <| fun _ -> Shell.cleanDirs [ "bin"; "temp" ]

Target.create "CleanDocs" <| fun _ -> Shell.cleanDirs [ "docs/output" ]

// --------------------------------------------------------------------------------------
// Build library & test project

Target.create "Restore" <| fun _ ->
    !! "src/**/*.??proj" -- "src/**/*.shproj"
    |> Seq.iter (fun pattern -> DotNet.restore DotNetCli.setRestoreOptions pattern)


Target.create "Build" <| fun _ ->
    "FSharp.Data.GraphQL.sln"
    |> DotNet.build (fun o ->
        { o with
            Configuration = configuration
            MSBuildParams = { o.MSBuildParams with DisableInternalBinLog = true } })

let startGraphQLServer (project : string) port (streamRef : DataRef<Stream>) =
    DotNet.build
        (fun options ->
            { options with
                Configuration = configuration
                MSBuildParams = { options.MSBuildParams with DisableInternalBinLog = true } })
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

let runTests (project : string) =
    DotNet.build
        (fun options ->
            { options with
                Configuration = configuration
                MSBuildParams = { options.MSBuildParams with DisableInternalBinLog = true } })
        project

    DotNet.test
        (fun options ->
            { options with
                Configuration = configuration
                MSBuildParams = { options.MSBuildParams with DisableInternalBinLog = true }
                Common = { options.Common with CustomParams = Some "--no-build -v=normal" } }.WithCommon DotNetCli.setVersion)
        project

let starWarsServerStream = StreamRef.Empty

Target.create "StartStarWarsServer" <| fun _ ->
    Target.activateFinal "StopStarWarsServer"

    let project =
        "samples"
        </> "star-wars-api"
        </> "FSharp.Data.GraphQL.Samples.StarWarsApi.fsproj"

    startGraphQLServer project 8086 starWarsServerStream

Target.createFinal "StopStarWarsServer" <| fun _ ->
    try
        starWarsServerStream.Value.Write ([| 0uy |], 0, 1)
    with e ->
        printfn "%s" e.Message

let integrationServerStream = StreamRef.Empty

Target.create "StartIntegrationServer" <| fun _ ->
    Target.activateFinal "StopIntegrationServer"

    let project =
        "tests"
        </> "FSharp.Data.GraphQL.IntegrationTests.Server"
        </> "FSharp.Data.GraphQL.IntegrationTests.Server.fsproj"

    startGraphQLServer project 8085 integrationServerStream

Target.createFinal "StopIntegrationServer" <| fun _ ->
    try
        integrationServerStream.Value.Write ([| 0uy |], 0, 1)
    with e ->
        printfn "%s" e.Message

Target.create "UpdateIntrospectionFile" <| fun _ ->
    let client = new HttpClient ()
    (task{
        let! result = client.GetAsync("http://localhost:8086")
        let! contentStream = result.Content.ReadAsStreamAsync()
        let! jsonDocument = JsonDocument.ParseAsync contentStream
        let file = new FileStream("tests/FSharp.Data.GraphQL.IntegrationTests/introspection.json", FileMode.Create, FileAccess.Write, FileShare.None)
        let encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
        let jsonWriterOptions = JsonWriterOptions(Indented = true, Encoder = encoder)
        let writer = new Utf8JsonWriter(file, jsonWriterOptions)
        jsonDocument.WriteTo writer
        do! writer.FlushAsync()
        do! writer.DisposeAsync()
        do! file.DisposeAsync()
        result.Dispose()
    }).Wait()
    client.Dispose()

Target.create "RunUnitTests" <| fun _ ->
    runTests "tests/FSharp.Data.GraphQL.Tests/FSharp.Data.GraphQL.Tests.fsproj"

Target.create "RunIntegrationTests" <| fun _ ->
    runTests "tests/FSharp.Data.GraphQL.IntegrationTests/FSharp.Data.GraphQL.IntegrationTests.fsproj"

let prepareDocGen () =
    Shell.rm "docs/release-notes.md"
    Shell.cp "RELEASE_NOTES.md" "docs/RELEASE_NOTES.md"
    Shell.rename "docs/release-notes.md" "docs/RELEASE_NOTES.md"

    Shell.rm "docs/license.md"
    Shell.cp "LICENSE.txt" "docs/LICENSE.txt"
    Shell.rename "docs/license.md" "docs/LICENSE.txt"

    Shell.cleanDir ".fsdocs"

Target.create "GenerateDocs" <| fun _ ->
    prepareDocGen ()
    DotNet.exec DotNetCli.setVersion "fsdocs" "build --clean" |> ignore

Target.create "GenerateDocsWatch" <| fun _ ->
    prepareDocGen ()
    DotNet.exec DotNetCli.setVersion "fsdocs" "watch --clean" |> ignore
    System.Console.ReadKey () |> ignore

Target.create "ReleaseDocs" <| fun _ ->
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
        { p with
            Common = { p.Common with Version = Some release.NugetVersion }
            OutputPath = Some packageDir }.WithCommon DotNetCli.setVersion)

let publishPackage id =
    let packageName = getPackageName id
    let packageDir = getPackageDir packageName
    let projectPath = getProjectPath packageName

    projectPath
    |> DotNet.publish (fun p ->
        { p with Common = { p.Common with WorkingDirectory = packageDir } }.WithCommon DotNetCli.setVersion)

Target.create "PublishShared" <| fun _ -> publishPackage "Shared"

Target.create "PublishServer" <| fun _ -> publishPackage "Server"

Target.create "PublishClient" <| fun _ -> publishPackage "Client"

Target.create "PublishMiddleware" <| fun _ -> publishPackage "Server.Middleware"

Target.create "PublishRelay" <| fun _ -> publishPackage "Server.Relay"

Target.create "PackShared" <| fun _ -> pack "Shared"

Target.create "PackServer" <| fun _ -> pack "Server"

Target.create "PackClient" <| fun _ -> pack "Client"

Target.create "PackMiddleware" <| fun _ -> pack "Server.Middleware"

Target.create "PackRelay" <| fun _ -> pack "Server.Relay"


// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build --target <Target>' to override

Target.create "All" ignore
Target.create "PackAll" ignore

"Clean"
    ==> "Restore"
    ==> "Build"
    ==> "RunUnitTests"
    ==> "StartStarWarsServer"
    ==> "StartIntegrationServer"
    ==> "UpdateIntrospectionFile"
    ==> "RunIntegrationTests"
    ==> "All"
    =?> ("GenerateDocs", Environment.environVar "GITHUB_ACTIONS" = "True")
    |> ignore

"CleanDocs"
    ==> "GenerateDocs"
    |> ignore

"PackShared"
    ==> "PackServer"
    ==> "PackClient"
    ==> "PackMiddleware"
    ==> "PackRelay"
    ==> "PackAll"
    |> ignore

Target.runOrDefaultWithArguments "All"

execContext.Context.Clear ()
