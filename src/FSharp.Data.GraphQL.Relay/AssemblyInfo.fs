namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("FSharp.Data.GraphQL.Relay")>]
[<assembly: AssemblyProductAttribute("FSharp.Data.GraphQL.Relay")>]
[<assembly: AssemblyDescriptionAttribute("Relay compatibility library for FSharp implementation of Facebook GraphQL query language")>]
[<assembly: AssemblyVersionAttribute("0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.1")>]
[<assembly: InternalsVisibleTo("FSharp.Data.GraphQL.Tests")>]
[<assembly: InternalsVisibleTo("FSharp.Data.GraphQL.Benchmarks")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.1"
    let [<Literal>] InformationalVersion = "0.0.1"
