namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharp.Data.GraphQL.Client")>]
[<assembly: AssemblyProductAttribute("FSharp.Data.GraphQL")>]
[<assembly: AssemblyDescriptionAttribute("FSharp implementation of Facebook GraphQL query language")>]
[<assembly: AssemblyVersionAttribute("0.0.2")>]
[<assembly: AssemblyFileVersionAttribute("0.0.2")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.2"
    let [<Literal>] InformationalVersion = "0.0.2"
