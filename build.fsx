#r "nuget: Fake.Api.GitHub"
#r "nuget: Fake.Core.ReleaseNotes"
#r "nuget: Fake.Core.Target"
#r "nuget: Fake.Core.UserInput"
#r "nuget: Fake.DotNet.AssemblyInfoFile"
#r "nuget: Fake.DotNet.Cli"
#r "nuget: Fake.DotNet.Fsc"
#r "nuget: Fake.DotNet.MSBuild"
#r "nuget: Fake.IO.FileSystem"
#r "nuget: Fake.Tools.Git"
#r "nuget: System.Reactive"
#r "nuget: Octokit"

open System.IO
open Fake
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Tools
open Fake.Core

// https://github.com/fsprojects/FAKE/issues/2517
// Regular header and `#load ".fake/build.fsx/intellisense.fsx"`

#if !FAKE
let execContext =
    System.Environment.GetCommandLineArgs ()
    |> Array.skip 2 // skip fsi.exe; build.fsx
    |> Array.toList
    |> Fake.Core.Context.FakeExecutionContext.Create false __SOURCE_FILE__

execContext
|> Fake.Core.Context.RuntimeContext.Fake
|> Fake.Core.Context.setExecutionContext
#endif

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
    |> Seq.iter (fun pattern -> DotNet.restore id pattern)


Target.create "Build" <| fun _ ->
    "FSharp.Data.GraphQL.sln"
    |> DotNet.build (fun o ->
        { o with
            Configuration = DotNet.BuildConfiguration.Release
            MSBuildParams = { o.MSBuildParams with DisableInternalBinLog = true } })

let startGraphQLServer (project : string) (streamRef : DataRef<Stream>) =
    DotNet.build
        (fun options ->
            { options with
                Configuration = DotNet.BuildConfiguration.Release
                MSBuildParams = { options.MSBuildParams with DisableInternalBinLog = true } })
        project

    let projectName = Path.GetFileNameWithoutExtension (project)
    let projectPath = Path.GetDirectoryName (project)

    let serverExe =
        projectPath
        </> "bin"
        </> "Release"
        </> DotNetMoniker
        </> (projectName + ".dll")

    CreateProcess.fromRawCommandLine "dotnet" $"{serverExe} --urls=http://localhost:8086/"
    |> CreateProcess.withStandardInput (CreatePipe streamRef)
    |> Proc.start
    |> ignore

    System.Threading.Thread.Sleep (2000)

let runTests (project : string) =
    DotNet.build
        (fun options ->
            { options with
                Configuration = DotNet.BuildConfiguration.Release
                MSBuildParams = { options.MSBuildParams with DisableInternalBinLog = true } })
        project

    DotNet.test
        (fun options ->
            { options with
                Configuration = DotNet.BuildConfiguration.Release
                MSBuildParams = { options.MSBuildParams with DisableInternalBinLog = true }
                Common = { options.Common with CustomParams = Some "--no-build -v=normal" } })
        project

let starWarsServerStream = StreamRef.Empty

Target.create "StartStarWarsServer" <| fun _ ->
    Target.activateFinal "StopStarWarsServer"

    let project =
        "samples"
        </> "star-wars-api"
        </> "FSharp.Data.GraphQL.Samples.StarWarsApi.fsproj"

    startGraphQLServer project starWarsServerStream

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

    startGraphQLServer project integrationServerStream

Target.createFinal "StopIntegrationServer" <| fun _ ->
    try
        integrationServerStream.Value.Write ([| 0uy |], 0, 1)
    with e ->
        printfn "%s" e.Message

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
    DotNet.exec id "fsdocs" "build --clean" |> ignore

Target.create "GenerateDocsWatch" <| fun _ ->
    prepareDocGen ()
    DotNet.exec id "fsdocs" "watch --clean" |> ignore
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
            OutputPath = Some packageDir })

let publishPackage id =
    let packageName = getPackageName id
    let packageDir = getPackageDir packageName
    let projectPath = getProjectPath packageName

    projectPath
    |> DotNet.publish (fun p ->
        { p with Common = { p.Common with WorkingDirectory = packageDir } })

Target.create "PublishServer" <| fun _ -> publishPackage "Server"

Target.create "PublishClient" <| fun _ -> publishPackage "Client"

Target.create "PublishMiddleware" <| fun _ -> publishPackage "Server.Middleware"

Target.create "PublishShared" <| fun _ -> publishPackage "Shared"

Target.create "PackServer" <| fun _ -> pack "Server"

Target.create "PackClient" <| fun _ -> pack "Client"

Target.create "PackMiddleware" <| fun _ -> pack "Server.Middleware"

Target.create "PackShared" <| fun _ -> pack "Shared"


// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build -t <Target>' to override

Target.create "All" ignore
Target.create "PackAll" ignore

"Clean"
    ==> "Restore"
    ==> "Build"
    ==> "RunUnitTests"
    ==> "StartStarWarsServer"
    ==> "StartIntegrationServer"
    ==> "RunIntegrationTests"
    ==> "All"
    =?> ("GenerateDocs", Environment.environVar "APPVEYOR" = "True")

"CleanDocs"
    ==> "GenerateDocs"

"PackShared"
    ==> "PackServer"
    ==> "PackClient"
    ==> "PackMiddleware"
    ==> "PackAll"

Target.runOrDefaultWithArguments "All"

#if !FAKE
execContext.Context.Clear ()
#endif
