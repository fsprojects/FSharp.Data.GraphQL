namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("FSharp.Data.GraphQL.Server")>]
[<assembly: AssemblyProductAttribute("FSharp.Data.GraphQL")>]
[<assembly: AssemblyDescriptionAttribute("FSharp implementation of Facebook GraphQL query language")>]
[<assembly: AssemblyVersionAttribute("0.0.2")>]
[<assembly: AssemblyFileVersionAttribute("0.0.2")>]
[<assembly: InternalsVisibleToAttribute("FSharp.Data.GraphQL.Benchmarks")>]
[<assembly: InternalsVisibleToAttribute("FSharp.Data.GraphQL.Tests")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.2"
    let [<Literal>] InformationalVersion = "0.0.2"
