#r "paket:
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.Tools.Git
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.Core.ReleaseNotes
nuget Fake.Core.UserInput
nuget Fake.DotNet.MSBuild
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Fsc
nuget Fake.Api.GitHub
nuget Fake.DotNet.Paket
nuget Octokit
nuget FSharp.Core //"

#if !FAKE
#load ".fake/build.fsx/intellisense.fsx"
#endif

open System
open System.IO
open System.Collections.Generic
open System.Threading
open Fake
open Fake.Tools.Git
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Tools
open Fake.Core
open Fake.Api
open Octokit

// --------------------------------------------------------------------------------------
// Information about the project are used
// --------------------------------------------------------------------------------------
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"


let project = "FSharp.Data.GraphQL"
let summary = "FSharp implementation of Facebook GraphQL query language"
let gitOwner = "fsprojects"
let gitHome = "https://github.com/" + gitOwner
let gitName = "FSharp.Data.GraphQL"
let release = ReleaseNotes.load "RELEASE_NOTES.md"

// Generate assembly info files with the right version & up-to-date information
Target.create "AssemblyInfo" (fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ AssemblyInfo.Title projectName
          AssemblyInfo.Product project
          AssemblyInfo.Description summary
          AssemblyInfo.Version release.AssemblyVersion
          AssemblyInfo.FileVersion release.AssemblyVersion ]
    let internalsVisibility (fsproj: string) =
        match fsproj with
        | f when f.EndsWith "FSharp.Data.GraphQL.Shared.fsproj" ->
            [ AssemblyInfo.InternalsVisibleTo "FSharp.Data.GraphQL.Server"
              AssemblyInfo.InternalsVisibleTo "FSharp.Data.GraphQL.Client"
              AssemblyInfo.InternalsVisibleTo "FSharp.Data.GraphQL.Client.DesignTime"
              AssemblyInfo.InternalsVisibleTo "FSharp.Data.GraphQL.Tests" ]
        | f when f.EndsWith "FSharp.Data.GraphQL.Server.fsproj" -> 
            [ AssemblyInfo.InternalsVisibleTo "FSharp.Data.GraphQL.Benchmarks"
              AssemblyInfo.InternalsVisibleTo "FSharp.Data.GraphQL.Tests" ]
        | _ -> []

    let getProjectDetails (projectPath:string) =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath,
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! "src/**/*.fsproj"
    -- "src/FSharp.Data.GraphQL.Client.DesignTime/FSharp.Data.GraphQL.Client.DesignTime.fsproj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, _, folderName, attributes) ->
         AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") (attributes @ internalsVisibility projFileName)
    )
)

// --------------------------------------------------------------------------------------
// Clean build results

Target.create "Clean" (fun _ ->
    Shell.cleanDirs ["bin"; "temp"]
)

Target.create "CleanDocs" (fun _ ->
    Shell.cleanDirs ["docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

// We need to disable parallel restoring of projects to because running paket in parallel from Mono
// is giving errors in Unix based operating systems.
Target.create "Restore" (fun _ ->
    !! "src/**/*.??proj"
    -- "src/**/*.shproj"
    |> Seq.iter (DotNet.restore id))


Target.create "Build" <| fun _ ->
    "FSharp.Data.GraphQL.sln"
    |>  DotNet.build (fun o ->
            { o with Configuration = DotNet.BuildConfiguration.Release })

/// The path to the .NET CLI executable.
let dotNetCliExe = DotNet.Options.Create().DotNetCliPath

Target.create "RunTests" (fun _ ->
    let restore =
        DotNet.restore id
    let build =
        DotNet.build (fun options ->
        { options with 
            Configuration = DotNet.BuildConfiguration.Release
            Common = { options.Common with 
                        CustomParams = Some "--no-restore" } })
    let runTests (project : string) =
        restore project
        build project
        DotNet.test (fun options ->
            { options with
                Configuration = DotNet.BuildConfiguration.Release
                Common = { options.Common with
                            CustomParams = Some "--no-build -v=normal" } }) project
    let startServer (serverProject:string) =
        use waiter = new ManualResetEvent(false)
        let serverProjectDir = Path.GetDirectoryName(serverProject)
        let projectName = Path.GetFileNameWithoutExtension(serverProject)
        let serverExe = "bin" </> "Release" </> "net5.0" </> (projectName + ".dll")
        let stdHandler (msg : string) =
            let expectedMessage = "Application started. Press Ctrl+C to shut down.".ToLowerInvariant()
            if msg.ToLowerInvariant().Contains(expectedMessage)
            then waiter.Set() |> ignore
        let errHandler (msg : string) =
            failwithf "Error while starting %s server. %s" msg projectName
        restore serverProject
        build serverProject
        // The server executable must be run instead of the project.
        // "dotnet run" command (used to run projects instead of the dll) spawns additional dotnet processes
        // that are not handled as child processes, so FAKE will not be able to track and kill them after the tests are done.
        CreateProcess.fromRawCommand dotNetCliExe [| serverExe |]
        |> CreateProcess.withWorkingDirectory serverProjectDir
        |> CreateProcess.redirectOutput
        |> CreateProcess.withOutputEventsNotNull stdHandler errHandler
        |> Proc.start
        |> ignore // FAKE automatically kills all started processes at the end of the script, so we don't need to worry about finishing them
        if not (waiter.WaitOne(TimeSpan.FromMinutes(float 2)))
        then failwithf "Timeout while waiting for %s server to run. Can not run integration tests." projectName
    runTests "tests/FSharp.Data.GraphQL.Tests/FSharp.Data.GraphQL.Tests.fsproj"
    startServer ("samples" </> "star-wars-api" </> "FSharp.Data.GraphQL.Samples.StarWarsApi.fsproj")
    startServer ("tests" </> "FSharp.Data.GraphQL.IntegrationTests.Server" </> "FSharp.Data.GraphQL.IntegrationTests.Server.fsproj")
    runTests "tests/FSharp.Data.GraphQL.IntegrationTests/FSharp.Data.GraphQL.IntegrationTests.fsproj")

// --------------------------------------------------------------------------------------
// Generate the documentation

// Documentation
let buildDocumentationTarget fsiargs target =
    // TODO:
    ()

Target.create "GenerateReferenceDocs" (fun _ ->
    buildDocumentationTarget "-d:RELEASE -d:REFERENCE" "Default"
)

let generateHelp' fail debug =
    let args =
        if debug then "--define:HELP"
        else "--define:RELEASE --define:HELP"
    try
        buildDocumentationTarget args "Default"
        Trace.traceImportant "Help generated"
    with
    | _ when not fail ->
        Trace.traceImportant "generating help documentation failed"

let generateHelp fail =
    generateHelp' fail false

Target.create "GenerateHelp" (fun _ ->
    Shell.rm "docs/content/release-notes.md"
    Shell.cp "docs/content/" "RELEASE_NOTES.md"
    Shell.rename "docs/content/release-notes.md" "docs/content/RELEASE_NOTES.md"

    Shell.rm "docs/content/license.md"
    Shell.cp "docs/content/" "LICENSE.txt"
    Shell.rename "docs/content/license.md" "docs/content/LICENSE.txt"

    generateHelp true
)

Target.create "GenerateHelpDebug" (fun _ ->
    Shell.rm "docs/content/release-notes.md"
    Shell.cp "docs/content/" "RELEASE_NOTES.md"
    Shell.rename "docs/content/release-notes.md" "docs/content/RELEASE_NOTES.md"

    Shell.rm "docs/content/license.md"
    Shell.cp "docs/content/" "LICENSE.txt"
    Shell.rename "docs/content/license.md" "docs/content/LICENSE.txt"

    generateHelp' true true
)

Target.create "KeepRunning" (fun _ ->
    use watcher = !! "docs/content/**/*.*" |> ChangeWatcher.run (fun _ ->
         generateHelp' true true
    )

    Trace.traceImportant "Waiting for help edits. Press any key to stop."

    System.Console.ReadKey() |> ignore

    watcher.Dispose()
)

Target.create "GenerateDocs" ignore

Target.create "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    Shell.cleanDir tempDocsDir
    Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir

    Shell.copyRecursive "docs/output" tempDocsDir true |> Trace.tracefn "%A"
    Git.Staging.stageAll tempDocsDir
    Git.Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)

let captureAndReraise ex =
    System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(ex).Throw()
    Unchecked.defaultof<_>

let rec retry count asyncF =
    // This retry logic causes an exception on Mono:
    // https://github.com/fsharp/fsharp/issues/440
    if not (isNull (System.Type.GetType("Mono.Runtime"))) then
        asyncF
    else
        async {
            try
                return! asyncF
            with ex ->
                return!
                    match (ex, ex.InnerException) with
                    | (:? AggregateException, (:? AuthorizationException as ex)) -> captureAndReraise ex
                    | _ when count > 0 -> retry (count - 1) asyncF
                    | (ex, _) -> captureAndReraise ex
        }

let retryWithArg count input asycnF =
    async {
        let! choice = input |> Async.Catch
        match choice with
        | Choice1Of2 input' ->
            return! (asycnF input') |> retry count
        | Choice2Of2 ex ->
            return captureAndReraise ex
    }

[<NoComparison>]
type Draft =
    { Client : GitHubClient
      Owner : string
      Project : string
      DraftRelease : Release }

let makeRelease draft owner project version prerelease (notes:seq<string>) (client : Async<GitHubClient>) =
    retryWithArg 5 client <| fun client' -> async {
        let data = NewRelease(version)
        data.Name <- version
        data.Body <- String.Join(Environment.NewLine, notes)
        data.Draft <- draft
        data.Prerelease <- prerelease
        let! draft = Async.AwaitTask <| client'.Repository.Release.Create(owner, project, data)
        let draftWord = if data.Draft then " draft" else ""
        printfn "Created%s release id %d" draftWord draft.Id
        return {
            Client = client'
            Owner = owner
            Project = project
            DraftRelease = draft }
    }

let createDraft owner project version prerelease notes client = 
    makeRelease true owner project version prerelease notes client
    
let releaseDraft (draft : Async<Draft>) =
    retryWithArg 5 draft <| fun draft' -> async {
        let update = draft'.DraftRelease.ToUpdate()
        update.Draft <- Nullable<bool>(false)
        let! released = Async.AwaitTask <| draft'.Client.Repository.Release.Edit(draft'.Owner, draft'.Project, draft'.DraftRelease.Id, update)
        printfn "Released %d on github" released.Id
    }

Target.create "Release" (fun _ ->
    let user = 
        match Environment.environVarOrDefault "github-user" System.String.Empty with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> UserInput.getUserInput "Username: "
    let pw =
        match Environment.environVarOrDefault "github-pw" System.String.Empty with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> UserInput.getUserPassword "Password: "
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    Git.Staging.stageAll ""
    Git.Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
    Git.Branches.pushBranch "" remote (Information.getBranchName "")

    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" remote release.NugetVersion

    GitHub.createClient user pw
    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    |> releaseDraft
    |> Async.RunSynchronously
)

Target.create "AdHocBuild" (fun _ ->
    !! "src/FSharp.Data.GraphQL.Client/FSharp.Data.GraphQL.Client.fsproj"
    |> MSBuild.runRelease id "bin/FSharp.Data.GraphQL.Client" "Build"
    |> Trace.logItems "Output: "
)

let pack id =
    Shell.cleanDir <| sprintf "nuget/%s.%s" project id
    Paket.pack(fun p ->
        { p with
            ToolType = ToolType.CreateLocalTool()
            Version = release.NugetVersion
            OutputPath = sprintf "nuget/%s.%s" project id
            TemplateFile = sprintf "src/%s.%s/%s.%s.fsproj.paket.template" project id project id
            MinimumFromLockFile = true
            IncludeReferencedProjects = false })

let publishPackage id =
    pack id
    Paket.push(fun p ->
        { p with
            ToolType = ToolType.CreateLocalTool()
            WorkingDir = sprintf "nuget/%s.%s" project id
            PublishUrl = "https://www.nuget.org/api/v2/package" })

Target.create "PublishServer" (fun _ ->
    publishPackage "Server"
)

Target.create "PublishClient" (fun _ ->
    publishPackage "Client"
)

Target.create "PublishMiddleware" (fun _ ->
    publishPackage "Server.Middleware"
)

Target.create "PackServer" (fun _ ->
    pack "Server"
)

Target.create "PackClient" (fun _ ->
    pack "Client"
)

Target.create "PackMiddleware" (fun _ ->
    pack "Server.Middleware"
)


// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build -t <Target>' to override

Target.create "All" ignore

"Clean"
  ==> "Restore"
  =?> ("AssemblyInfo", BuildServer.isLocalBuild)
  ==> "Build"
  ==> "RunTests"
  ==> "All"
  =?> ("GenerateReferenceDocs", Environment.environVar "APPVEYOR" = "True")
  =?> ("GenerateDocs", Environment.environVar "APPVEYOR" = "True")
  =?> ("ReleaseDocs",BuildServer.isLocalBuild)

"CleanDocs"
  ==> "GenerateHelp"
  ==> "GenerateReferenceDocs"
  ==> "GenerateDocs"

"CleanDocs"
  ==> "GenerateHelpDebug"

"GenerateHelpDebug"
  ==> "KeepRunning"

"ReleaseDocs"
  ==> "Release"

Target.runOrDefault "All"