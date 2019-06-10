namespace FSharp.Data.GraphQL.IntegrationTests.Server

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open System.Text

type Root =
    { RequestId : string }

type InputField =
    { String : string
      Int : int
      StringOption : string option
      IntOption : int option
      Uri : System.Uri }

type Input =
    { Single : InputField option
      List : InputField list option }

type UploadedFile =
    { Name : string
      ContentType : string
      ContentAsText : string }

module Schema =
    let InputFieldType =
        Define.InputObject<InputField>(
            name = "InputField",
            fields = 
                [ Define.Input("string", String, description = "A string value.") 
                  Define.Input("int", Int, description = "An integer value.")
                  Define.Input("stringOption", Nullable String, description = "A string option value.")
                  Define.Input("intOption", Nullable Int, description = "An integer option value.")
                  Define.Input("uri", Uri, description = "An URI value.") ])

    let InputType =
        Define.InputObject<Input>(
            name ="Input",
            description = "Input object type.",
            fields = 
                [ Define.Input("single", Nullable InputFieldType, description = "A single input field.")
                  Define.Input("list", Nullable (ListOf InputFieldType), description = "A list of input fields.") ])

    let OutputFieldType =
        Define.Object<InputField>(
            name = "OutputField",
            description = "The output for a field input.",
            fields =
                [ Define.Field("string", String, resolve = (fun _ x -> x.String), description = "A string value.") 
                  Define.AutoField("int", Int, description = "An integer value.")
                  Define.AutoField("stringOption", Nullable String, description = "A string option value.")
                  Define.AutoField("intOption", Nullable Int, description = "An integer option value.")
                  Define.AutoField("uri", Uri, description = "An URI value.")
                  Define.Field("deprecated", String, resolve = (fun _ x -> x.String), description = "A string value through a deprecated field.", deprecationReason = "This field is deprecated.", args = []) ])

    let UploadedFileType =
        Define.Object<UploadedFile>(
            name = "UploadedFile",
            description = "Contains data of an uploaded file.",
            fields =
                [ Define.AutoField("name", String, description = "The name of the file.")
                  Define.AutoField("contentType", String, description = "The content type of the file.")
                  Define.AutoField("contentAsText", String, description = "The content of the file as text.") ])

    let OutputType =
        Define.Object<Input>(
            name = "Output",
            description = "The output for an input.",
            fields = 
                [ Define.AutoField("single", Nullable OutputFieldType, description = "A single output field.")
                  Define.AutoField("list", Nullable (ListOf OutputFieldType), description = "A list of output fields.") ])

    let QueryType =
        Define.Object<Root>(
            name = "Query",
            description = "The query type.",
            fields = 
                [ Define.Field(
                    name = "echo",
                    typedef = Nullable OutputType,
                    description = "Enters an input type and get it back.",
                    args = [ Define.Input("input", Nullable InputType, description = "The input to be echoed as an output.") ],
                    resolve = fun ctx _ -> ctx.TryArg("input") |> Option.flatten) ])

    let MutationType =
        let mapUploadToOutput (file : File) =
            let contentAsText =
                use reader = new System.IO.StreamReader(file.Content, Encoding.UTF8)
                reader.ReadToEnd()
            { Name = file.Name; ContentType = file.ContentType; ContentAsText = contentAsText }
        Define.Object<Root>(
            name = "Mutation",
            fields =
                [ Define.Field(
                    name = "singleUpload",
                    typedef = UploadedFileType,
                    description = "Uploads a single file to the server and get it back.",
                    args = [ Define.Input("file", Upload, description = "The file to be uploaded.") ],
                    resolve = fun ctx _ -> mapUploadToOutput (ctx.Arg("file")))
                  Define.Field(
                    name = "nullableSingleUpload",
                    typedef = Nullable UploadedFileType,
                    description = "Uploads (maybe) a single file to the server and get it back (maybe).",
                    args = [ Define.Input("file", Nullable Upload, description = "The file to be uploaded.") ],
                    resolve = fun ctx _ -> ctx.TryArg("file") |> Option.flatten |> Option.map mapUploadToOutput)
                  Define.Field(
                    name = "multipleUpload",
                    typedef = ListOf UploadedFileType,
                    description = "Uploads a list of files to the server and get them back.",
                    args = [ Define.Input("files", ListOf Upload, description = "The files to upload.") ],
                    resolve = fun ctx _ -> ctx.Arg("files") |> Seq.map mapUploadToOutput)
                  Define.Field(
                    name = "nullableMultipleUpload",
                    typedef = Nullable (ListOf UploadedFileType),
                    description = "Uploads (maybe) a list of files to the server and get them back (maybe).",
                    args = [ Define.Input("files", Nullable (ListOf Upload), description = "The files to upload.") ],
                    resolve = fun ctx _ -> ctx.TryArg("files") |> Option.flatten |> Option.map (Seq.map mapUploadToOutput))
                  Define.Field(
                    name = "nullableMultipleNullableUpload",
                    typedef = Nullable (ListOf (Nullable UploadedFileType)),
                    description = "Uploads (maybe) a list of files (maybe) to the server and get them back (maybe).",
                    args = [ Define.Input("files", Nullable (ListOf (Nullable Upload)), description = "The files to upload.") ],
                    resolve = fun ctx _ -> ctx.TryArg("files") |> Option.flatten |> Option.map (Seq.map (Option.map mapUploadToOutput))) ])

    let schema : ISchema<Root> = upcast Schema(QueryType, MutationType)

    let executor = Executor(schema)