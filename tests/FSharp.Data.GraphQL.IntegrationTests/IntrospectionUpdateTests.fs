module FSharp.Data.GraphQL.IntegrationTests.IntrospectionUpdateTests

open System
open System.IO
open System.Net.Http.Json
open System.Text.Json
open System.Threading
open Xunit

let introspectionFilePath =
    Path.Combine (__SOURCE_DIRECTORY__, "integration-introspection.json")
    |> Path.GetFullPath

let normalizeJsonDocument options (document : JsonDocument) =
    use buffer = new MemoryStream ()
    use writer = new Utf8JsonWriter (buffer, options)
    document.WriteTo writer
    writer.Flush ()
    buffer.Seek (0L, SeekOrigin.Begin) |> ignore
    JsonDocument.Parse buffer

let parseAndNormalizeJsonAsync ct options stream =
    task {
        let! document = JsonDocument.ParseAsync (stream, cancellationToken = ct)
        return normalizeJsonDocument options document
    }

let areSchemasEqual (document1 : JsonDocument) (document2 : JsonDocument) =
    let schema1 = document1.RootElement.GetProperty("data").GetProperty("__schema")
    let schema2 = document2.RootElement.GetProperty("data").GetProperty("__schema")
    schema1.GetRawText() = schema2.GetRawText()

let readDestinationDocumentAsync ct (stream : FileStream) =
    task {
        try
            let! document = JsonDocument.ParseAsync (stream, cancellationToken = ct)
            return ValueSome document
        with :? JsonException ->
            return ValueNone
    }

let updateIntrospectionFileAsync ct sourceStream =
    task {
        use destinationStream =
            new FileStream (introspectionFilePath, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.Read)

        let options = JsonWriterOptions(Indented = true)
        let! sourceDocument = parseAndNormalizeJsonAsync ct options sourceStream
        destinationStream.Seek (0L, SeekOrigin.Begin) |> ignore
        let! destinationDocument = readDestinationDocumentAsync ct destinationStream

        let shouldUpdate =
            match destinationDocument with
            | ValueNone -> true
            | ValueSome document -> not (areSchemasEqual document sourceDocument)

        if shouldUpdate then
            destinationStream.Seek (0L, SeekOrigin.Begin) |> ignore
            destinationStream.SetLength 0
            use writer = new Utf8JsonWriter (destinationStream, options)
            sourceDocument.WriteTo writer
            writer.Flush ()

        return shouldUpdate
    }

[<Fact>]
let ``Get GraphQL introspection response returns schema`` () =
    task {
        use httpClient = TestHosts.createIntegrationHttpClient ()
        let! response = httpClient.GetFromJsonAsync<JsonElement>("/", CancellationToken.None)
        let schema = response.GetProperty("data").GetProperty("__schema")
        Assert.NotEqual(Unchecked.defaultof<JsonElement>, schema)
        let hasErrors, _ = response.TryGetProperty "errors"
        Assert.False hasErrors
    }

[<Fact>]
let ``Update integration introspection file when schema changes`` () =
    task {
        use httpClient = TestHosts.createIntegrationHttpClient ()
        let! sourceStream = httpClient.GetStreamAsync("/")
        let! wasUpdated = updateIntrospectionFileAsync CancellationToken.None sourceStream
        Assert.True(File.Exists introspectionFilePath)
        if wasUpdated then
            let! sourceStreamSecondRun = httpClient.GetStreamAsync("/")
            use sourceStreamForVerification = sourceStreamSecondRun
            let! wasUpdatedSecondRun = updateIntrospectionFileAsync CancellationToken.None sourceStreamForVerification
            Assert.False wasUpdatedSecondRun
    }
