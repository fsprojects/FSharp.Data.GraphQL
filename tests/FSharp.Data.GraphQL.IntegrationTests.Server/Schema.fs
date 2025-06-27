namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open System.Text
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

type Root(ctx : HttpContext) =

    member _.RequestAborted: System.Threading.CancellationToken = ctx.RequestAborted
    member _.ServiceProvider: IServiceProvider = ctx.RequestServices
    member root.GetRequiredService<'t>() = root.ServiceProvider.GetRequiredService<'t>()

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

type InputField =
    { String : string
      Int : int
      StringOption : string option
      IntOption : int option
      Uri : System.Uri
      Guid : System.Guid
      GuidOption : System.Guid option }

type Input =
    { Single : InputField option
      List : InputField list option }

type InputFile = { File : System.IO.Stream}

type UploadedFile =
    { Name : string
      ContentType : string
      ContentAsText : string }

type UploadedContentFile =
    { Name : string
      ContentAsText : string }

type UploadRequest =
    { Single : File
      Multiple : File list
      NullableMultiple : File list option
      NullableMultipleNullable : File option list option }

type UploadResponse =
    { Single : UploadedFile
      Multiple : UploadedFile list
      NullableMultiple : UploadedFile list option
      NullableMultipleNullable : UploadedFile option list option }

module Schema =
    let InputFieldType =
        Define.InputObject<InputField>(
            name = "InputField",
            fields =
                [ Define.Input("string", StringType, description = "A string value.")
                  Define.Input("int", IntType, description = "An integer value.")
                  Define.Input("stringOption", Nullable StringType, description = "A string option value.")
                  Define.Input("intOption", Nullable IntType, description = "An integer option value.")
                  Define.Input("uri", UriType, description = "An URI value.")
                  Define.Input("guid", GuidType, description = "A Guid value.") ])

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
                [ Define.Field("string", StringType, resolve = (fun _ x -> x.String), description = "A string value.")
                  Define.AutoField("int", IntType, description = "An integer value.")
                  Define.AutoField("stringOption", Nullable StringType, description = "A string option value.")
                  Define.AutoField("intOption", Nullable IntType, description = "An integer option value.")
                  Define.AutoField("uri", UriType, description = "An URI value.")
                  Define.AutoField("guid", GuidType, description = "A Guid value.")
                  Define.Field("guidId", IDType, description = "A Guid Id value.", resolve = fun _ o -> o.Guid |> string)
                  Define.Field("stringId", IDType, description = "A String Id value.", resolve = fun _ o -> o.String)
                  Define.Field("guidIdOption", Nullable IDType, description = "A Guid Id value.", resolve = fun _ o -> o.GuidOption |> Option.map string)
                  Define.Field("stringIdOption", Nullable IDType, description = "A String Id value.", resolve = fun _ o -> o.StringOption)
                  Define.Field("deprecated", StringType, resolve = (fun _ x -> x.String), description = "A string value through a deprecated field.", deprecationReason = "This field is deprecated.", args = []) ])

    let UploadedFileType =
        Define.Object<UploadedFile>(
            name = "UploadedFile",
            description = "Contains data of an uploaded file.",
            fields =
                [ Define.AutoField("name", StringType, description = "The name of the file.")
                  Define.AutoField("contentType", StringType, description = "The content type of the file.")
                  Define.AutoField("contentAsText", StringType, description = "The content of the file as text.") ])

    let UploadRequestType =
        Define.InputObject<UploadRequest>(
            name = "UploadRequest",
            description = "Request for uploading files in several different forms.",
            fields =
                [ Define.Input("single", Upload, description = "A single file upload.")
                  Define.Input("multiple", ListOf Upload, description = "Multiple file uploads.")
                  Define.Input("nullableMultiple", Nullable (ListOf Upload), description = "Optional list of multiple file uploads.")
                  Define.Input("nullableMultipleNullable", Nullable (ListOf (Nullable Upload)), description = "Optional list of multiple optional file uploads.") ])

    let UploadResponseType =
        Define.Object<UploadResponse>(
            name = "UploadResponse",
            description = "Contains uploaded files of an upload files request.",
            fields =
                [ Define.AutoField("single", UploadedFileType, description = "A single file upload.")
                  Define.AutoField("multiple", ListOf UploadedFileType, description = "Multiple file uploads.")
                  Define.AutoField("nullableMultiple", Nullable (ListOf UploadedFileType), description = "Optional list of multiple file uploads.")
                  Define.AutoField("nullableMultipleNullable", Nullable (ListOf (Nullable UploadedFileType)), description = "Optional list of multiple optional file uploads.") ])

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
                    typedef = StructNullable OutputType,
                    description = "Enters an input type and get it back.",
                    args = [ Define.Input("input", Nullable InputType, description = "The input to be echoed as an output.") ],
                    resolve = fun ctx _ -> ctx.TryArg("input")) ])

    let InputFileObject = Define.InputObject<Input>(
        name = "InputFile",
        fields =
            [
                Define.Input("file", FileType)
            ])
    let MutationType =
        let getFileContent (ctx : ResolveFieldContext) argName =
            let stream = ctx.Arg<System.IO.Stream> argName
            use reader = new System.IO.StreamReader(stream, Encoding.UTF8, true)
            reader.ReadToEnd()

        Define.Object<Root> (
            name = "MutationType",
            fields =
                [ Define.Field ("uploadFile", StringType, "", [ Define.Input ("input", FileType) ],
                    (fun ctx _ -> getFileContent ctx "input"));
                    Define.Field ("uploadFileComplex", StringType, "", [ Define.Input ("input", InputFileObject) ],
                    (fun ctx _ -> getFileContent ctx "input"))
                ]
        )

    let schema : ISchema<Root> = upcast Schema(QueryType, MutationType)

    let executor = Executor(schema)
