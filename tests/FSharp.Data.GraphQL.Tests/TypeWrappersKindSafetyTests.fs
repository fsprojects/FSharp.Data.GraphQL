module FSharp.Data.GraphQL.Tests.TypeWrappersKindSafetyTests

open System
open System.Diagnostics
open System.Threading.Tasks
open FSharp.Data.GraphQL.Types
open Xunit

type private InputOnly = { Value : int }
type private OutputOnly = { Value : int }

let private InputOnlyType =
    Define.InputObject<InputOnly> (name = "InputOnlyType", fields = [ Define.Input ("value", IntType) ])

let private OutputOnlyType =
    Define.Object<OutputOnly> (name = "OutputOnlyType", fields = [ Define.Field ("value", IntType, fun _ x -> x.Value) ])

type TypeWrappersKindSafetyFixture () =

    let scriptsDir = IO.Path.Combine (AppContext.BaseDirectory, "TypeWrappersKindSafety")
    let referencesPath = IO.Path.Combine (scriptsDir, "References.fsx")
    let sourceProjectDir =
        IO.Path.GetFullPath (IO.Path.Combine (AppContext.BaseDirectory, "..", "..", ".."))
    let sourceScriptsDir = IO.Path.Combine (sourceProjectDir, "TypeWrappersKindSafety")
    let sourceReferencesPath = IO.Path.Combine (sourceScriptsDir, "References.fsx")

    let ensureFileContentAsync (path : string) (content : string) : Task = task {
        if IO.File.Exists (path) then
            let! existing = IO.File.ReadAllTextAsync (path)

            if not (String.Equals (existing, content, StringComparison.Ordinal)) then
                do! IO.File.WriteAllTextAsync (path, content)
        else
            do! IO.File.WriteAllTextAsync (path, content)
    }

    member _.ScriptPath (name : string) = IO.Path.Combine (scriptsDir, name)

    member _.RunFsiCheckAsync (scriptPath : string) : Task<int> = task {
        let psi =
            ProcessStartInfo (
                "dotnet",
                sprintf "fsi --noninteractive \"%s\"" scriptPath,
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                WorkingDirectory = AppContext.BaseDirectory
            )

        use proc = Process.Start (psi)
        do! proc.WaitForExitAsync ()
        return proc.ExitCode
    }

    member private _.ReferencesContent =
        let sharedAssembly = IO.Path.Combine (AppContext.BaseDirectory, "FSharp.Data.GraphQL.Shared.dll")
        let serverAssembly = IO.Path.Combine (AppContext.BaseDirectory, "FSharp.Data.GraphQL.Server.dll")

        [| sprintf "#r @\"%s\"" sharedAssembly; sprintf "#r @\"%s\"" serverAssembly |]
        |> String.concat "\n"

    interface IAsyncLifetime with

        member this.InitializeAsync () : Task =
            task {
                IO.Directory.CreateDirectory (scriptsDir) |> ignore
                IO.Directory.CreateDirectory (sourceScriptsDir) |> ignore

                let content = this.ReferencesContent

                do! ensureFileContentAsync referencesPath content
                do! ensureFileContentAsync sourceReferencesPath content
            }

        member _.DisposeAsync () = Task.CompletedTask

type TypeWrappersKindSafetyTests (fixture : TypeWrappersKindSafetyFixture) =
    interface IClassFixture<TypeWrappersKindSafetyFixture>

    [<Fact>]
    member _.``ListOf keeps input-output direction`` () : Task = task {
            let inputList : InputDef<InputOnly list> = ListOf InputOnlyType
            let outputList : OutputDef<OutputOnly list> = ListOf OutputOnlyType
            Assert.Equal ("[InputOnlyType!]!", inputList.ToString ())
            Assert.Equal ("[OutputOnlyType!]!", outputList.ToString ())
        }

    [<Fact>]
    member _.``Nullable keeps input-output direction`` () : Task = task {
            let nullableInput : InputDef<InputOnly option> = Nullable InputOnlyType
            let nullableOutput : OutputDef<OutputOnly option> = Nullable OutputOnlyType
            Assert.Equal ("InputOnlyType", nullableInput.ToString ())
            Assert.Equal ("OutputOnlyType", nullableOutput.ToString ())
        }

    [<Fact>]
    member _.``StructNullable keeps input-output direction`` () : Task = task {
            let nullableInput : InputDef<InputOnly voption> = StructNullable InputOnlyType
            let nullableOutput : OutputDef<OutputOnly voption> = StructNullable OutputOnlyType
            Assert.Equal ("InputOnlyType", nullableInput.ToString ())
            Assert.Equal ("OutputOnlyType", nullableOutput.ToString ())
        }

    [<Fact>]
    member _.``Valid script compiles successfully`` () : Task = task {
            let! exitCode = fixture.RunFsiCheckAsync (fixture.ScriptPath ("Valid.fsx"))
            Assert.Equal (0, exitCode)
        }

    [<Fact>]
    member _.``ListOf rejects output type as input at compile time`` () : Task = task {
            let! exitCode = fixture.RunFsiCheckAsync (fixture.ScriptPath ("ListOf.OutputAsInput.fsx"))
            Assert.NotEqual (0, exitCode)
        }

    [<Fact>]
    member _.``ListOf rejects input type as output at compile time`` () : Task = task {
            let! exitCode = fixture.RunFsiCheckAsync (fixture.ScriptPath ("ListOf.InputAsOutput.fsx"))
            Assert.NotEqual (0, exitCode)
        }

    [<Fact>]
    member _.``Nullable rejects output type as input at compile time`` () : Task = task {
            let! exitCode = fixture.RunFsiCheckAsync (fixture.ScriptPath ("Nullable.OutputAsInput.fsx"))
            Assert.NotEqual (0, exitCode)
        }

    [<Fact>]
    member _.``Nullable rejects input type as output at compile time`` () : Task = task {
            let! exitCode = fixture.RunFsiCheckAsync (fixture.ScriptPath ("Nullable.InputAsOutput.fsx"))
            Assert.NotEqual (0, exitCode)
        }

    [<Fact>]
    member _.``StructNullable rejects output type as input at compile time`` () : Task = task {
            let! exitCode = fixture.RunFsiCheckAsync (fixture.ScriptPath ("StructNullable.OutputAsInput.fsx"))
            Assert.NotEqual (0, exitCode)
        }

    [<Fact>]
    member _.``StructNullable rejects input type as output at compile time`` () : Task = task {
        let! exitCode = fixture.RunFsiCheckAsync (fixture.ScriptPath ("StructNullable.InputAsOutput.fsx"))
        Assert.NotEqual (0, exitCode)
    }
