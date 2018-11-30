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
nuget Octokit //"

#load ".fake/build.fsx/intellisense.fsx"

open System
open System.IO
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
open System.Diagnostics

// --------------------------------------------------------------------------------------
// Information about the project are used
// --------------------------------------------------------------------------------------
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

let project = "FSharp.Data.GraphQL"
let summary = "FSharp implementation of Facebook GraphQL query language"
let description = "FSharp implementation of Facebook GraphQL query language"
let authors = [ "Bazinga Technologies Inc" ]
let copyright = "Copyright (c) 2016 Bazinga Technologies Inc"
let tags = "FSharp GraphQL Relay React"
let solutionFile  = "FSharp.Data.GraphQL.sln"
let testAssemblies = "tests/**/bin/Release/**/*Tests*.dll"
let gitOwner = "bazingatechnologies"
let gitHome = "https://github.com/" + gitOwner
let gitName = "FSharp.Data.GraphQL"
let gitRaw = Environment.environVarOrDefault "gitRaw" "https://raw.github.com/bazingatechnologies"
let release = ReleaseNotes.load "RELEASE_NOTES.md"

module Util =
    open System.Net
    
    let join pathParts =
        Path.Combine(Array.ofSeq pathParts)

    let run workingDir fileName args =
        let fileName, args =
            if Environment.isUnix
            then fileName, args else "cmd", ("/C " + fileName + " " + args)
        let ok =
            Process.execSimple (fun info -> 
                { info with
                      FileName = fileName
                      WorkingDirectory = workingDir
                      Arguments = args }) TimeSpan.MaxValue = 0
        if not ok then failwith (sprintf "'%s> %s %s' task failed" workingDir fileName args)

    let runAndReturn workingDir fileName args =
        let fileName, args =
            if Environment.isUnix
            then fileName, args else "cmd", ("/C " + args)
        Process.execWithResult (fun info ->
                { info with
                      FileName = fileName
                      WorkingDirectory = workingDir
                      Arguments = args }) TimeSpan.MaxValue
        |> fun p -> p.Messages |> String.concat "\n"

    let rmdir dir =
        if Environment.isUnix
        then Shell.rm_rf dir
        // Use this in Windows to prevent conflicts with paths too long
        else run "." "cmd" ("/C rmdir /s /q " + Path.GetFullPath dir)

    let compileScript symbols outDir (fsxPath : string) =
        let dllFile = Path.ChangeExtension(Path.GetFileName fsxPath, ".dll")
        let opts = [
            yield Fsc.Out (Path.Combine(outDir, dllFile))
            yield Fsc.Target Fsc.TargetType.Library
            yield! symbols |> List.map Fsc.Define
        ]
        Fsc.compile opts [fsxPath]

    let normalizeVersion (version: string) =
        let i = version.IndexOf("-")
        if i > 0 then version.Substring(0, i) else version

    let assemblyInfo projectDir version extra =
        let version = normalizeVersion version
        let asmInfoPath = projectDir </> "AssemblyInfo.fs"
        (AssemblyInfo.Version version) :: extra
        |> AssemblyInfoFile.createFSharp asmInfoPath

module Npm =
    let script workingDir script args =
        sprintf "run %s -- %s" script (String.concat " " args)
        |> Util.run workingDir "npm"

    let install workingDir modules =
        sprintf "install %s" (String.concat " " modules)
        |> Util.run workingDir "npm"

    let command workingDir command args =
        sprintf "%s %s" command (String.concat " " args)
        |> Util.run workingDir "npm"

    let commandAndReturn workingDir command args =
        sprintf "%s %s" command (String.concat " " args)
        |> Util.runAndReturn workingDir "npm"

    let getLatestVersion package tag =
        let package =
            match tag with
            | Some tag -> package + "@" + tag
            | None -> package
        commandAndReturn "." "show" [package; "version"]

module Node =
    let run workingDir script args =
        let args = sprintf "%s %s" script (String.concat " " args)
        Util.run workingDir "node" args

// --------------------------------------------------------------------------------------
// Helpers for generating AssemblyInfo
// --------------------------------------------------------------------------------------
let (|Fsproj|Csproj|Vbproj|Shproj|) (projFileName:string) =
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | f when f.EndsWith("shproj") -> Shproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)

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
              AssemblyInfo.InternalsVisibleTo "FSharp.Data.GraphQL.Tests" ]
        | f when f.EndsWith "FSharp.Data.GraphQL.Server.fsproj" -> 
            [ AssemblyInfo.InternalsVisibleTo "FSharp.Data.GraphQL.Benchmarks"
              AssemblyInfo.InternalsVisibleTo "FSharp.Data.GraphQL.Tests" ]
        | _ -> []

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath,
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! "src/**/*.??proj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, _, folderName, attributes) ->
            match projFileName with
            | Fsproj -> AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") (attributes @ internalsVisibility projFileName)
            | Csproj -> AssemblyInfoFile.createCSharp ((folderName </> "Properties") </> "AssemblyInfo.cs") attributes
            | Vbproj -> AssemblyInfoFile.createVisualBasic ((folderName </> "My Project") </> "AssemblyInfo.vb") attributes
            | Shproj -> ()
        )
)

// Copies binaries from default VS location to expected bin folder
// But keeps a subdirectory structure for each project in the
// src folder to support multiple project outputs
Target.create "CopyBinaries" (fun _ ->
    !! "src/**/*.??proj"
    -- "src/**/*.shproj"
    -- "src/netcore/**/*.??proj"
    |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) </> "bin/Release", "bin" </> (System.IO.Path.GetFileNameWithoutExtension f)))
    |>  Seq.iter (fun (fromDir, toDir) -> Shell.copyDir toDir fromDir (fun _ -> true))
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

// Paket restore at solution level gives concurrency erros on packages folder when running in Unix.
// For that reason, we restore each project individually
Target.create "Restore" (fun _ ->
    !! "src/**/*.??proj"
    ++ "tests/**/*.??proj"
    -- "src/**/*.shproj"
    -- "tests/FSharp.Data.GraphQL.Tests.Sql/FSharp.Data.GraphQL.Tests.Sql.fsproj"
    |> Seq.iter (DotNet.restore id))

Target.create "Build" (fun _ ->
    "FSharp.Data.GraphQL.sln"
    |> DotNet.build (fun options ->
        { options with 
            Configuration = DotNet.BuildConfiguration.Release
            Common = { options.Common with 
                        CustomParams = Some "--no-restore" } }))

Target.create "RunTests" (fun _ ->
    "tests/FSharp.Data.GraphQL.Tests/FSharp.Data.GraphQL.Tests.fsproj"
    |> DotNet.test (fun p ->
            { p with
                NoBuild = true
                Configuration = DotNet.BuildConfiguration.Release }))

// --------------------------------------------------------------------------------------
// Generate the documentation


let fakePath = "packages" </> "build" </> "FAKE" </> "tools" </> "FAKE.exe"

/// The path to the F# Interactive tool.
let pathToFsiExe =
    let fsiPath = @".\tools\FSharp\;.\lib\FSharp\;[ProgramFilesX86]\Microsoft SDKs\F#\10.1\Framework\v4.0;[ProgramFilesX86]\Microsoft SDKs\F#\4.1\Framework\v4.0;[ProgramFilesX86]\Microsoft SDKs\F#\4.0\Framework\v4.0;[ProgramFilesX86]\Microsoft SDKs\F#\3.1\Framework\v4.0;[ProgramFilesX86]\Microsoft SDKs\F#\3.0\Framework\v4.0;[ProgramFiles]\Microsoft F#\v4.0\;[ProgramFilesX86]\Microsoft F#\v4.0\;[ProgramFiles]\FSharp-2.0.0.0\bin\;[ProgramFilesX86]\FSharp-2.0.0.0\bin\;[ProgramFiles]\FSharp-1.9.9.9\bin\;[ProgramFilesX86]\FSharp-1.9.9.9\bin\"
    let ev = Environment.environVar "FSI"
    if not (String.isNullOrEmpty ev) then ev else
    if Environment.isUnix then
        // The standard name on *nix is "fsharpi"
        match Process.tryFindFileOnPath "fsharpi" with
        | Some file -> file
        | None ->
        // The early F# 2.0 name on *nix was "fsi"
        match Process.tryFindFileOnPath "fsi" with
        | Some file -> file
        | None -> "fsharpi"
    else
        // let dir = Path.GetDirectoryName fullAssemblyPath
        // let fi = FileInfo.ofPath (Path.Combine(dir, "fsi.exe"))
        // if fi.Exists then fi.FullName else
        Process.findPath "FSIPath" fsiPath "fsi.exe"

let fakeStartInfo script workingDirectory args fsiargs environmentVars =
    (fun (info: ProcStartInfo) ->
        { info with 
            FileName = System.IO.Path.GetFullPath fakePath
            Arguments = sprintf "%s --fsiargs -d:FAKE %s \"%s\"" args fsiargs script
            WorkingDirectory = workingDirectory
            Environment = 
                Map.ofSeq environmentVars
                |> Map.add "MSBuild" MSBuild.msBuildExe
                |> Map.add "GIT" Git.CommandHelper.gitPath
                |> Map.add "FSI" pathToFsiExe })

/// Run the given buildscript with FAKE.exe
let executeFAKEWithOutput workingDirectory script fsiargs envArgs =
    let exitCode =
        Process.execSimple (fakeStartInfo script workingDirectory "" fsiargs envArgs) TimeSpan.MaxValue
    System.Threading.Thread.Sleep 1000
    exitCode

// Documentation
let buildDocumentationTarget fsiargs target =
    Trace.trace (sprintf "Building documentation (%s), this could take some time, please wait..." target)
    let exit = executeFAKEWithOutput "docs/tools" "generate.fsx" fsiargs ["target", target]
    if exit <> 0 then
        failwith "generating reference documentation failed"
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

let createIndexFsx lang =
    let content = """(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../../bin"

(**
F# Project Scaffold ({0})
=========================
*)
"""
    let targetDir = "docs/content" </> lang
    let targetFile = targetDir </> "index.fsx"
    Directory.ensure targetDir
    System.IO.File.WriteAllText(targetFile, System.String.Format(content, lang))

Target.create "AddLangDocs" (fun _ ->
    let args = System.Environment.GetCommandLineArgs()
    if args.Length < 4 then
        failwith "Language not specified."

    args.[3..]
    |> Seq.iter (fun lang ->
        if lang.Length <> 2 && lang.Length <> 3 then
            failwithf "Language must be 2 or 3 characters (ex. 'de', 'fr', 'ja', 'gsw', etc.): %s" lang

        let templateFileName = "template.cshtml"
        let templateDir = "docs/tools/templates"
        let langTemplateDir = templateDir </> lang
        let langTemplateFileName = langTemplateDir </> templateFileName

        if System.IO.File.Exists(langTemplateFileName) then
            failwithf "Documents for specified language '%s' have already been added." lang

        Directory.ensure langTemplateDir
        Shell.copy langTemplateDir [ templateDir </> templateFileName ]

        createIndexFsx lang)
)

// --------------------------------------------------------------------------------------
// Release Scripts

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
        let data = new NewRelease(version)
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

let createDraft owner project version prerelease notes client = makeRelease true owner project version prerelease notes client
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
    |> MSBuild.runDebug id "bin/FSharp.Data.GraphQL.Client" "Build"
    |> Trace.logItems "Output: "
)

let pack id =
    Shell.cleanDir <| sprintf "nuget/%s.%s" project id
    Paket.pack(fun p ->
        { p with
            Version = release.NugetVersion
            OutputPath = sprintf "nuget/%s.%s" project id
            TemplateFile = sprintf "src/%s.%s/%s.%s.fsproj.paket.template" project id project id
            IncludeReferencedProjects = true
            MinimumFromLockFile = true
        })
let publishPackage id =
    pack id
    Paket.push(fun p ->
        { p with 
            WorkingDir = sprintf "nuget/%s.%s" project id
            PublishUrl = "https://www.nuget.org/api/v2/package" })
    
Target.create "PublishServer" (fun _ ->
    publishPackage "Server"
)

Target.create "PublishClient" (fun _ ->
    publishPackage "Client"
)

Target.create "PublishMiddlewares" (fun _ ->
    publishPackage "Server.Middlewares"
)

Target.create "PackServer" (fun _ ->
    pack "Server"
)

Target.create "PackClient" (fun _ ->
    pack "Client"
)

Target.create "PackMiddlewares" (fun _ ->
    pack "Server.Middlewares"
)

Target.create "PublishNpm" (fun _ ->
    let binDir, prjDir = "bin/npm", "src/FSharp.Data.GraphQL.Client"
    Shell.cleanDir binDir

    !! ("src/FSharp.Data.GraphQL.Client" </> "*.fsproj")
    |> MSBuild.run id "bin/FSharp.Data.GraphQL.Client" "Build" [ "Configuration", "Build"; "DefineConstants", "FABLE" ]
    |> ignore

    Shell.copyDir binDir (prjDir </> "bin" </> "Release") (fun _ -> true)
    !! (prjDir </> "npm" </> "*.*")
    |> Seq.iter (fun path -> Shell.cp path binDir)

    Npm.command binDir "version" ["0.0.3"] //[string release.SemVer]
    Npm.command binDir "publish" []
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target.create "All" ignore

"Clean"
  ==> "Restore"
  =?> ("AssemblyInfo", BuildServer.isLocalBuild)
  ==> "Build"
  ==> "CopyBinaries"
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