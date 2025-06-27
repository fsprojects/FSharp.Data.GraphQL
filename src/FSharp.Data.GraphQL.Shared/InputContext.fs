namespace FSharp.Data.GraphQL

type IInputExecutionContext =
    abstract GetFile : string -> Result<System.IO.Stream, string>

type InputExecutionContextProvider = unit -> IInputExecutionContext

