namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("FSharp.Data.GraphQL.Shared")>]
[<assembly: AssemblyProductAttribute("FSharp.Data.GraphQL")>]
[<assembly: AssemblyDescriptionAttribute("FSharp implementation of Facebook GraphQL query language")>]
[<assembly: AssemblyVersionAttribute("0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.1")>]
[<assembly: InternalsVisibleTo("FSharp.Data.GraphQL.Server")>]
[<assembly: InternalsVisibleTo("FSharp.Data.GraphQL.Client")>]
[<assembly: InternalsVisibleTo("FSharp.Data.GraphQL.Tests")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.1"
    let [<Literal>] InformationalVersion = "0.0.1"
