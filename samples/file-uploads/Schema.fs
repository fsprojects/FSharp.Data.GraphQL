namespace FSharp.Data.GraphQL.Samples.FileUpload

open FSharp.Data.GraphQL.Types
open type FSharp.Data.GraphQL.Types.SchemaDefinitions.Define
open FSharp.Data.GraphQL.AspNet

type Uploads =
  { Files : FSharp.Data.GraphQL.AspNet.FileUpload list option }

type Root = unit

module Schema =
    let UploadType =
        Scalar<FileUpload>(
            name = "Upload",
            description = "The `Upload` scalar type represents a file upload.",
            coerceValue = (fun value ->
                match value with
                | :? FileUpload as upload -> Some upload
                | _ -> None),
            coerceInput = fun value ->
                raise <| invalidOp("Upload cannot be coerced from an AST input.")
        )

    let UploadsType =
        InputObject<Uploads>(
            name = "Uploads",
            description = "The `Upload` scalar type represents a file upload.",
            fields = [
                Input("files", Nullable(ListOf UploadType))
            ]
        )

    let Query =
        Object<Root>(
            name = "Query",
            fields = [
                Field("Message", String, "A list of the employees", fun ctx _ -> "Hello World")
            ]
        )


    let Mutation =
        Object<Root>(
            name = "Mutation",
            fields = [
                AsyncField("singleUpload", Boolean, "upload a single file", [Input("file", UploadType)],
                    fun ctx _ -> async {
                        let file = ctx.Arg("file")
                        printfn $"File: {file}"
                        return  true
                    })
                AsyncField("multiUpload", Boolean, "upload a single file", [Input("files", ListOf UploadType)],
                    fun ctx _ -> async {
                        let files = ctx.Arg("files")
                        printfn $"Files: {files}"
                        return  true
                    })
                AsyncField("multiUploadRecord", Boolean, "upload a single file", [Input("files", UploadsType)],
                    fun ctx _ -> async {
                        let files = ctx.Arg("files")
                        printfn $"Files: {files}"
                        return  true
                    })
            ]
        )