namespace FSharp.Data.GraphQL.Client

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("FSharp.Data.GraphQL.Client.DesignTime.dll")>]
do ()
