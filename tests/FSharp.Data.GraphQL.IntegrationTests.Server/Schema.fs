namespace FSharp.Data.GraphQL.IntegrationTests.Server

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

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
                [ Define.AutoField("string", String, description = "A string value.") 
                  Define.AutoField("int", Int, description = "An integer value.")
                  Define.AutoField("stringOption", Nullable String, description = "A string option value.")
                  Define.AutoField("intOption", Nullable Int, description = "An integer option value.")
                  Define.Field("deprecated", String, resolve = (fun _ x -> x.String), description = "A string value through a deprecated field.", deprecationReason = "This field is deprecated.", args = [])
                  Define.AutoField("uri", Uri, description = "An URI value.") ])

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
                    typedef = OutputType,
                    description = "Enters an input type and get it back.",
                    args = [ Define.Input("input", InputType, description = "The input to be echoed as an output.") ],
                    resolve = (fun ctx _ -> ctx.Arg("input"))) ])

    let schema = Schema(QueryType)

    let executor = Executor(schema)