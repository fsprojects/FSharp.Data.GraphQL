namespace FSharp.Data.GraphQL

type FileData = {
    Stream : System.IO.Stream
    ContentType : string
}

type IInputExecutionContext =
    abstract GetFile : string -> Result<FileData, string>

type InputExecutionContextProvider = unit -> IInputExecutionContext

