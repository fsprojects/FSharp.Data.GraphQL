namespace FSharp.Data.GraphQL.AspNet

open System
open System.IO
open System.Net
open System.Reflection
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading.Tasks
open System.Security.Cryptography
open System.Collections.Concurrent

open FSharp.Reflection
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Net.Http.Headers
open Microsoft.Extensions.Primitives
open Microsoft.AspNetCore.WebUtilities
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.AspNetCore.Http.Features

type GraphQLQuery =
    { ExecutionPlan : ExecutionPlan
      Variables : Map<string, obj> }

type GraphQLQueryRequest =
    | Single of GraphQLQuery
    | Batch of GraphQLQuery list

type WebSocketClientMessage =
    | ConnectionInit of token : string option * session : string option * scheme : string option
    | ConnectionTerminate
    | Start of id : string * payload : GraphQLQuery
    | Stop of id : string
    | ParseError of id : string option * err : string

type WebSocketServerMessage =
    | ConnectionAck
    | ConnectionError of err : string
    | Data of id : string * payload : Output
    | Error of id : string option * err : string
    | Complete of id : string

type FileUpload =
    { Name : string
      FileType : string
      Size : int
      Content : Stream
      Path : string
      Hash : string }

[<AutoOpen>]
module Constants =
    let [<Literal>] QueryJsonKey = "query"
    let [<Literal>] VariablesJsonKey = "variables"
    let [<Literal>] DataJsonKey = "data"

type NameValueLookupConverter () =
    inherit JsonConverter<NameValueLookup>()
    override _.Read(reader: byref<Utf8JsonReader>, typeToConvert: System.Type, options: JsonSerializerOptions) =
        invalidOp "deserialization not supported"

    override _.Write(writer: Utf8JsonWriter, value: NameValueLookup, options: JsonSerializerOptions) =
        writer.WriteStartObject()
        for (KeyValue(key, data)) in value do
            writer.WritePropertyName(key)
            if isNull data then
                writer.WriteNullValue()
            else
                JsonSerializer.Serialize(writer, data, data.GetType(), options)
        writer.WriteEndObject()

type GQLResponseConverter () =
    inherit JsonConverter<GQLResponse>()

    override _.Read(reader: byref<Utf8JsonReader>, typeToConvert: System.Type, options: JsonSerializerOptions) =
        invalidOp "deserialization not supported"

    override _.Write(writer: Utf8JsonWriter, value: GQLResponse, options: JsonSerializerOptions) =
        match value with
        | Direct(data, errors) ->
            JsonSerializer.Serialize(writer, data, typeof<NameValueLookup>, options)
        | _ ->
            writer.WriteNull(DataJsonKey)

type IJsonVariableReader =
    abstract IsNullable : bool
    abstract Read : replacements: Map<string, FileUpload> * path: string * element: JsonElement -> obj

type TypeReaderCache = ConcurrentDictionary<InputDef, IJsonVariableReader>


[<AutoOpen>]
module private MultipartRequest =
    let getBoundy (contentType: MediaTypeHeaderValue) (lengthLimit: int) =
        let boundary = HeaderUtilities.RemoveQuotes(contentType.Boundary)
        if System.String.IsNullOrEmpty boundary.Value then
            raise <| InvalidDataException("Missing content-type boundary.")
        elif boundary.Value.Length > lengthLimit then
            raise <| InvalidDataException(sprintf "Multiparty boundary length limit %i exceeded" lengthLimit)
        else
            boundary.Value

    let isMultipartContentType (contentType: string) =
        not(System.String.IsNullOrEmpty contentType) && contentType.IndexOf("multipart/", StringComparison.OrdinalIgnoreCase) >= 0

    let (|FormContent|FileContent|NoContent|) (section: MultipartSection) =
        match ContentDispositionHeaderValue.TryParse(StringSegment section.ContentDisposition) with
        | true, contentDisposition when not(isNull contentDisposition) ->
            if contentDisposition.IsFormDisposition() then
                FormContent contentDisposition
            else
                FileContent contentDisposition
        | _ ->
            NoContent


module SchemaDefinitions =
    let FileUpload =
        Define.Scalar<FileUpload>(
            name = "Upload",
            description = "The `Upload` scalar type represents a file upload.",
            coerceValue = (fun value ->
                match value with
                | :? FileUpload as upload -> Some upload
                | _ -> None),
            coerceInput = fun value ->
                raise <| invalidOp("Upload cannot be coerced from  AST input.")
        )


module private JsonReader =

    let (|SpecificTypeDef|_|) (def: TypeDef) (value: TypeDef) =
        if def = value then Some() else None

    let combinePath (left: string) (right: string) =
        sprintf "%s.%s" left right

    let toCamelCase (value: string) =
        JsonNamingPolicy.CamelCase.ConvertName value

    let getOptionCtors (optionInnerType: Type) =
        let optionType = typedefof<option<_>>
        let caseInfos = FSharpType.GetUnionCases(optionType.MakeGenericType(optionInnerType))
        let noneCtor = caseInfos |> Seq.find(fun case -> case.Name = "None") |> FSharpValue.PreComputeUnionConstructor
        let someCtor = caseInfos |> Seq.find(fun case -> case.Name = "Some") |> FSharpValue.PreComputeUnionConstructor
        someCtor, noneCtor

    let private getCallInfo = function
        | Quotations.Patterns.Call(_, info, _) -> info
        | _ -> failwith "Unexpected Quotation!"

    let rec getJsonReaderAux (input: InputDef) (cache: TypeReaderCache) =
        match input with
        | Nullable (Input(innerDef)) -> getNullableReader innerDef cache
        | Scalar scalarDef -> getScalarReader scalarDef
        | Enum enumDef -> getEnumReader enumDef
        | List(Input(elementDef)) -> getListReader input elementDef cache
        | InputObject inputObjectDef -> getInputObjectReader inputObjectDef cache
        | typeDef -> raise(invalidOp(sprintf "Invalid input definition '%s'" typeDef.Type.Name))
    and getJsonReader (input: InputDef) (cache: ConcurrentDictionary<InputDef, IJsonVariableReader>) =
        match cache.TryGetValue input with
        | true, value ->
            value
        | false, _ ->
            let readerFunc = lazy getJsonReaderAux input cache
            let isNullable = input :? NullableDef
            cache.GetOrAdd(input, (fun input ->
                { new IJsonVariableReader with
                    member _.IsNullable = isNullable
                    member _.Read (replacements, path, element) =
                        readerFunc.Value replacements path element }))
    and getNullableReader (innerDef: InputDef) (cache: TypeReaderCache) =
        let innerReader: IJsonVariableReader = getJsonReader innerDef cache
        let someCtor, noneCtor =  getOptionCtors innerDef.Type
        fun replacements path (element: JsonElement) ->
            match element.ValueKind with
            | JsonValueKind.Null -> noneCtor [||]
            | _ -> someCtor [| innerReader.Read(replacements, path, element) |]
    and getScalarReader (scalarDef: ScalarDef) =
        match scalarDef with
        | SpecificTypeDef SchemaDefinitions.Date ->
            fun _ _ (element: JsonElement) -> element.GetDateTime() :> _
        | SpecificTypeDef SchemaDefinitions.Guid ->
            fun _ _ (element: JsonElement) -> element.GetGuid() :> _
        | SpecificTypeDef SchemaDefinitions.Uri ->
            fun _ _ (element: JsonElement) -> System.Uri(element.GetString()) :> _
        | SpecificTypeDef SchemaDefinitions.Boolean ->
            fun _ _ (element: JsonElement) -> element.GetBoolean() :> _
        | SpecificTypeDef SchemaDefinitions.Float ->
            fun _ _ (element: JsonElement) -> element.GetDouble() :> _
        | SpecificTypeDef SchemaDefinitions.Int ->
            fun _ _ (element: JsonElement) -> element.GetInt32() :> _
        | SpecificTypeDef SchemaDefinitions.FileUpload ->
            fun replacements path (element: JsonElement) ->
                match Map.tryFind path replacements with
                | Some file -> file :> _
                | None -> failwithf "Expected file upload at %s" path
        | _ ->
            fun _ _ (element: JsonElement) -> element.GetString() :> _
    and getEnumReader (enumDef: EnumDef) =
        if enumDef.Type.IsEnum then
            fun _ _ element ->
               let value = element.GetString()
               Enum.Parse(enumDef.Type, value, ignoreCase=true)
        else
            let flags = BindingFlags.NonPublic ||| BindingFlags.Public
            let cases = FSharpType.GetUnionCases(enumDef.Type, flags)
            let ctors =
                cases
                |> Seq.map(fun case -> case.Name.ToLowerInvariant(), FSharpValue.PreComputeUnionConstructor case)
                |> Map.ofSeq
            fun _ _ element ->
                let value = element.GetString()
                match Map.tryFind (value.ToLowerInvariant()) ctors with
                | Some ctor -> ctor [||]
                | None -> failwithf "Case '%s' does not match any Union constructors in '%s'" value enumDef.Type.Name
    and getListReader (listDef: InputDef) (elementDef: InputDef) (cache: TypeReaderCache) =
        if listDef.Type.IsGenericType then
            let elementReader: IJsonVariableReader = getJsonReader elementDef cache
            let genericType = listDef.Type.GetGenericTypeDefinition()
            let collectionConverterMethInfo =
                if genericType = typedefof<list<_>> then getCallInfo <@ List.ofSeq<_> Seq.empty @>
                elif genericType = typedefof<array<_>> then getCallInfo <@ Array.ofSeq<_> Seq.empty @>
                elif genericType = typedefof<Set<_>> then getCallInfo <@ Set.ofSeq<_> Seq.empty @>
                else getCallInfo <@ Seq.cast<_> Seq.empty @>
            let genericMethInfo = collectionConverterMethInfo.GetGenericMethodDefinition()
            let methInfo = genericMethInfo.MakeGenericMethod([|elementDef.Type|])
            let convertF v = methInfo.Invoke(null, [|v|])
            fun replacements path element ->
                match element.ValueKind with
                | JsonValueKind.Array ->
                    use enumerator = element.EnumerateArray()
                    enumerator
                    |> Seq.mapi(fun i value ->
                        let elementPath = combinePath path (i.ToString())
                        match Map.tryFind elementPath replacements with
                        | Some value -> box value
                        | None -> elementReader.Read(replacements, elementPath, value) )
                    |> Array.ofSeq
                    |> convertF
                | otherElement ->
                    failwithf "Expected array element but received '%A'" otherElement
        else
            failwithf "Unsupported GraphQL 'List' type '%s'"  listDef.Type.Name
    and getInputObjectReader (inputObject: InputObjectDef) (cache: TypeReaderCache) =
        if FSharpType.IsRecord inputObject.Type then
            let inputFieldReaders =
                inputObject.Fields
                |> Array.map (fun field -> field.Name, (field, toCamelCase field.Name, getJsonReader field.TypeDef cache))
                |> Map.ofArray
            let fieldReaders =
                [ for field in FSharpType.GetRecordFields(inputObject.Type, true) do
                    match Map.tryFind field.Name inputFieldReaders with
                    | Some fieldReader -> fieldReader
                    | None -> failwithf "field '%s' exists on Record '%s' but not InputObject '%s'" field.Name inputObject.Type.Name inputObject.Name ]
            let ctor = FSharpValue.PreComputeRecordConstructor(inputObject.Type, true)
            fun replacements path element ->
                match element.ValueKind with
                | JsonValueKind.Object ->
                    let values =
                        [| for (field, name, reader) in fieldReaders do
                             let elementPath = combinePath path name
                             match Map.tryFind elementPath replacements with
                             | Some value ->
                                 box value
                             | None ->
                                match element.TryGetProperty name, field.DefaultValue with
                                | (true, value), _ ->
                                    reader.Read(replacements, elementPath, value)
                                | (false, _), Some(defaultValue) ->
                                    box defaultValue
                                | (false, _), None when reader.IsNullable ->
                                    match field.TypeDef with
                                    | Nullable(Input(innerDef)) ->
                                        let _, noneCtor =  getOptionCtors innerDef.Type
                                        noneCtor [||]
                                    | _ ->
                                        failwithf "Expected field '%s' to be a nullable type" field.Name
                                | (false, _), None ->
                                    failwithf "Field '%s' is required, but not present in json object" field.Name |]
                    ctor values
                | fieldKind ->
                    failwithf "Expected Json Object for InputObject '%s' but received '%A'" inputObject.Name fieldKind
        else
            failwithf "InputObject '%s' must be a Record type." inputObject.Type.Name




    let readVariables (replacements: Map<string, FileUpload>) (index: int option) (variablesJson: JsonElement option) (vars: list<VarDef>) =
        let readerCache = ConcurrentDictionary()
        let basePath =
            match index with
            | Some idx -> combinePath (idx.ToString()) VariablesJsonKey
            | None -> VariablesJsonKey
        let tryReadProperty =
            match variablesJson with
            | Some variables ->
                fun (name: string) ->
                    match variables.TryGetProperty(name) with
                    | true, value -> Some value
                    | false, _ -> None
            | None ->
                fun (name: string) -> None
        Map.ofList [
            for var in vars do
                match tryReadProperty var.Name with
                | Some element ->
                    let path = combinePath basePath var.Name
                    let reader = getJsonReader var.TypeDef readerCache
                    let variableValue = reader.Read(replacements, path, element)
                    (var.Name, variableValue)
                | None ->
                    match var.DefaultValue, var.TypeDef with
                    | Some _, _ -> ()
                    | _, Nullable _ -> ()
                    | _  -> failwithf "Variable '%s' is missing and there is no default value" var.Name
        ]

module private ExecutionHandler =

    let bin2hex bytes =
        let byteToChar b = char(if b > 9uy then b + 0x37uy else b + 0x30uy)
        new string [| for byte in bytes do byteToChar(byte >>> 4); byteToChar(byte &&& 0xFuy) |]

    let tryReadSection (reader: MultipartReader) = async {
        let! result = reader.ReadNextSectionAsync() |> Async.AwaitTask
        return Option.ofObj result
    }

    let tryReadForm (reader: MultipartReader) = async {
        match! tryReadSection reader with
        | Some(FormContent contentDisposition & section) ->
            let form = section.AsFormDataSection()
            let! value = form.GetValueAsync() |> Async.AwaitTask
            return Some(contentDisposition.Name.Value, value)
        | _ ->
            return None
    }

    let tryReadFile (reader: MultipartReader) = async {
        match! tryReadSection reader with
        | Some(FileContent contentDisposition & section) ->
            let fileName = WebUtility.HtmlEncode(contentDisposition.FileName.Value)
            let filepath = Path.GetTempFileName()
            let cryptoHash = SHA256.Create()
            let flags = FileOptions.Asynchronous ||| FileOptions.SequentialScan
            let stream = new FileStream(filepath, FileMode.Create, FileAccess.Write, FileShare.None, 4096, flags)
            use hash = new CryptoStream(stream, cryptoHash, CryptoStreamMode.Write)
            do! section.Body.CopyToAsync(hash) |> Async.AwaitTask
            hash.FlushFinalBlock()
            stream.Position <- 0L
            let file =
                { Name = fileName; FileType =  section.ContentType; Size = stream.Length |> int
                  Path = filepath; Content = stream; Hash = bin2hex cryptoHash.Hash }
            return Some(contentDisposition.Name.Value, file)
        | _ ->
            return None
    }

    let readFiles (reader: MultipartReader) = async {
        let rec loop files = async {
            match! tryReadFile reader with
            | Some file -> return! loop (file::files)
            | None -> return Map.ofList files
        }
        return! loop []
    }

    let rec readMappingAux (reader: byref<Utf8JsonReader>) (files: Map<string, FileUpload>) (accum: (string * FileUpload) list) =
        match reader.TokenType with
        | JsonTokenType.String ->
            let mappedFile = reader.GetString()
            let file = files.[mappedFile]
            match reader.TokenType with
            | JsonTokenType.StartArray ->
                reader.Read() |> ignore
                let path = reader.GetString()
                reader.Read() |> ignore
                readMappingAux &reader files ((path, file)::accum)
            | invalidToken ->
                let message = sprintf "Expected 'BeginArray' but received json token '%A'" invalidToken
                raise <| invalidOp message
        | JsonTokenType.EndObject ->
            Map.ofSeq accum
        | invalidToken ->
            let message = sprintf "Expected PropertyName or EndObject for property mapping but received json token '%A'" invalidToken
            raise <| invalidOp message

    let readMapping (json: string) (fileMapping: Map<string, FileUpload>) =
        let bytes = System.Text.Encoding.UTF8.GetBytes(json)
        let readonlySpan =  ReadOnlySpan(bytes)
        let mutable reader = Utf8JsonReader(readonlySpan)
        match reader.TokenType with
        | JsonTokenType.StartObject ->
            reader.Read() |> ignore
            readMappingAux &reader fileMapping []
        | invalidToken ->
            let message = sprintf "Expected PropertyName or EndObject for property mapping but received json token '%A'" invalidToken
            raise <| invalidOp message



    let readOperation (executor: Executor<'T>) (element: JsonElement) (replacements:  Map<string, FileUpload>) (index: int option) =
        match element.ValueKind with
        | JsonValueKind.Object ->
            let queryText =
                match element.TryGetProperty(QueryJsonKey) with
                | true, queryElement -> queryElement.GetString()
                | false, _ -> Introspection.IntrospectionQuery

            let variablesElement =
                match element.TryGetProperty(VariablesJsonKey) with
                | true, value -> Some value
                | false, value -> None

            let plan = executor.CreateExecutionPlan(queryText)
            let variables = JsonReader.readVariables replacements index variablesElement plan.Variables
            { ExecutionPlan = plan; Variables = variables }
        | invalidElementKind ->
            let message = sprintf "Expected 'Object' element kind but received '%A'" invalidElementKind
            raise <| invalidOp message

    let readOperations (executor: Executor<'T>) (json: string) (replacements:  Map<string, FileUpload>) =
        use document = JsonDocument.Parse(json)
        match document.RootElement.ValueKind with
        | JsonValueKind.Array ->
            let operations =
                document.RootElement.EnumerateArray()
                |> Seq.mapi(fun i operation -> readOperation executor operation replacements (Some i))
                |> Seq.toList
            Batch operations
        | _ ->
            let operation = readOperation executor document.RootElement replacements None
            Single operation

    let serializeResponse (ctx: HttpContext) (serializerOptions: JsonSerializerOptions) (body: 'T) = async {
        let outputStream = ctx.Response.Body
        do! JsonSerializer.SerializeAsync(outputStream, body, serializerOptions) |> Async.AwaitTask
    }

    let processMultipartQuery (executor: Executor<'T>) (root: 'T)  (ctx: HttpContext) (serializerOptions: JsonSerializerOptions) = async {
        let boundary = ctx.Request.GetMultipartBoundary()
        let reader = MultipartReader(boundary, ctx.Request.Body)
        let! operationPart = tryReadForm reader
        let! mappingPart = tryReadForm reader
        let! fileParts = readFiles reader
        match mappingPart with
        | Some("map", mappingJson) ->
            match operationPart with
            | Some("operations", operationsJson) ->
                let replacements = readMapping mappingJson fileParts
                match readOperations executor operationsJson replacements with
                | Single request ->
                    let! response = executor.AsyncExecute(request.ExecutionPlan, data = root, variables = request.Variables)
                    do! serializeResponse ctx serializerOptions response
                | Batch requests ->
                    let hasMutation = requests |> List.exists (fun req -> req.ExecutionPlan.Operation.OperationType = Ast.OperationType.Mutation)
                    let operations = requests |> List.map(fun req -> executor.AsyncExecute(req.ExecutionPlan, data = root, variables = req.Variables))
                    let! responses = if hasMutation then Async.Sequential operations else Async.Parallel operations
                    do! serializeResponse ctx serializerOptions responses
            | _ ->
                let message = "Expected operations as first segment of multipart request"
                return raise <| invalidOp message
        | _ ->
            let message = "Expected mapping as second segment of multipart request"
            return raise <| invalidOp message
    }

    let readBody (ctx: HttpContext) = async {
        use reader = new StreamReader(ctx.Request.Body, Text.Encoding.UTF8)
        return! reader.ReadToEndAsync() |> Async.AwaitTask
    }

    let processQuery (executor: Executor<'T>) (root: 'T)  (ctx: HttpContext) (serializerOptions: JsonSerializerOptions) = async {
        let! body  = readBody ctx
        let! response =
            if System.String.IsNullOrWhiteSpace body then
                executor.AsyncExecute(Introspection.IntrospectionQuery, root)
            else
                async {
                    use document = JsonDocument.Parse(body)
                    let request = readOperation executor document.RootElement Map.empty None
                    return! executor.AsyncExecute(request.ExecutionPlan, data = root, variables = request.Variables)
                }
        let outputStream = ctx.Response.Body
        do! JsonSerializer.SerializeAsync(outputStream, response, serializerOptions) |> Async.AwaitTask
    }

    let queryHandler (buildArguments: HttpContext -> Async<'T>) (executor: Executor<'T>) =
        let graphQLSerializationOptions = JsonSerializerOptions()
        graphQLSerializationOptions.Converters.Add(GQLResponseConverter())
        graphQLSerializationOptions.Converters.Add(NameValueLookupConverter())
        fun (ctx: HttpContext) ->
            async {
                ctx.Response.Headers.Add("Content-Type", StringValues("application/json"))
                let! root = ctx |> buildArguments
                let execute =
                    if isMultipartContentType ctx.Request.ContentType
                    then processMultipartQuery
                    else processQuery
                do! execute executor root ctx graphQLSerializationOptions
              } |> Async.StartAsTask :> Task

[<AutoOpen>]
module ApplicationBuilderExtensions =
    type IApplicationBuilder with
        member builder.UseGraphQL(executor: Executor<'T>, buildArguments: HttpContext -> Async<'T>, ?path: string) =
            let urlPath = defaultArg path "/graphql"
            let graphQLHandler = ExecutionHandler.queryHandler buildArguments executor
            builder.Map(PathString urlPath, (fun (builder:IApplicationBuilder) ->
                builder.Run(RequestDelegate(graphQLHandler))))