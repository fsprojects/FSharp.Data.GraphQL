namespace FSharp.Data.GraphQL.Benchmarks.Sql

open System
open System.Data.SqlClient
open FSharp.Data

type GenomeScores = CsvProvider<"ml-latest/genome-scores.csv">
type GenomeTags = CsvProvider<"ml-latest/genome-tags.csv">
type Links = CsvProvider<"ml-latest/links.csv">
type Movies = CsvProvider<"ml-latest/movies.csv">
type Ratings = CsvProvider<"ml-latest/ratings.csv">
type Tags = CsvProvider<"ml-latest/tags.csv">

module internal Run =
    let sequentially =
        Seq.reduce (fun fst snd -> async { 
            do! fst
            return! snd })

module internal DatabaseSetup =
    let private openConnection connectionString = async {
        let conn = new SqlConnection(connectionString)
        do! conn.OpenAsync() |> Async.AwaitTask
        return conn }

    let private createCommand connection = new SqlCommand("", connection)

    let private setParameters (parameters : seq<string * obj>) (command : SqlCommand) =
        let toDbNull (value : obj) =
            value 
            |> Option.ofObj 
            |> Option.orElse (Some (upcast DBNull.Value)) 
            |> Option.get
        command.Parameters.Clear()
        parameters
        |> Seq.map (fun (name, value) -> name, toDbNull value)
        |> Seq.iter (fun (name, value) -> command.Parameters.AddWithValue(name, value) |> ignore)
        command

    let private executeNonQuery sql (command : SqlCommand) = async {
        command.CommandText <- sql
        do! command.ExecuteNonQueryAsync() |> Async.AwaitTask |> Async.Ignore
        command.Parameters.Clear() }
    
    let private executeScalar<'T> sql (command : SqlCommand) = async {
        command.CommandText <- sql
        let! res = command.ExecuteScalarAsync() |> Async.AwaitTask
        command.Parameters.Clear()
        return res :?> 'T }

    let buildIfNotExists connectionString = async {
        printfn "Checking if database exists..."
        use! connection = openConnection connectionString
        use command = createCommand connection
        // Create database
        do! command |> executeNonQuery "USE master"
        let! databaseExists = command |> executeScalar<bool> "IF NOT EXISTS (SELECT * FROM sys.databases WHERE name = 'MovieLens') SELECT (CAST(0 AS BIT)); ELSE SELECT (CAST(1 AS BIT))"
        if databaseExists 
        then
            printfn "Database MovieLens found. Skipping database setup."
        else
            printfn "Database MovieLens not found. Creating database..."
            do! command |> executeNonQuery "CREATE DATABASE MovieLens"
            do! command |> executeNonQuery "USE MovieLens"
            // Movies
            printfn "Creating table Movies..."
            do! command |> executeNonQuery "CREATE TABLE Movies (MovieId INT NOT NULL PRIMARY KEY, Genres VARCHAR(255) NOT NULL, Title VARCHAR(255) NOT NULL)"
            let! movies = Movies.AsyncGetSample()
            do!
                movies.Rows
                |> Seq.mapi (fun ix movie -> async {
                    printfn "Inserting Movies table row #%i..." ix
                    do!
                        command
                        |> setParameters ["@MovieId", upcast movie.MovieId; "@Genres", upcast movie.Genres; "@Title", upcast movie.Title]
                        |> executeNonQuery "INSERT INTO Movies (MovieId, Genres, Title) VALUES (@MovieId, @Genres, @Title)" })
                |> Run.sequentially
            // Ratings
            printfn "Creating table Ratings..."
            do! command |> executeNonQuery "CREATE TABLE Ratings (
                MovieId INT NOT NULL FOREIGN KEY REFERENCES Movies(MovieId),
                UserId INT NOT NULL,
                Rating DECIMAL(2, 1) NOT NULL,
                [Timestamp] INT NOT NULL,
                PRIMARY KEY (MovieId, UserId))"
            let! ratings = Ratings.AsyncGetSample()
            do!
                ratings.Rows
                |> Seq.mapi (fun ix rating -> async {
                    printfn "Inserting Ratings table row #%i..." ix
                    do!
                        command
                        |> setParameters ["@MovieId", upcast rating.MovieId; "@UserId", upcast rating.UserId; "@Rating", upcast rating.Rating; "@Timestamp", upcast rating.Timestamp]
                        |> executeNonQuery "INSERT INTO Ratings (MovieId, UserId, Rating, [Timestamp]) VALUES (@MovieId, @UserId, @Rating, @Timestamp)" })
                |> Run.sequentially
            // Tags
            printfn "Creating table Tags..."
            do! command |> executeNonQuery "CREATE TABLE Tags (
                TagId INT NOT NULL IDENTITY(1, 1) PRIMARY KEY,
                MovieId INT NOT NULL FOREIGN KEY REFERENCES Movies(MovieId),
                UserId INT NOT NULL,
                Tag VARCHAR(255) NOT NULL,
                [Timestamp] INT NOT NULL)"
            let! tags = Tags.AsyncGetSample()
            do!
                tags.Rows
                |> Seq.mapi (fun ix tag -> async {
                    printfn "Inserting Tags table row #%i..." ix
                    do!
                        command
                        |> setParameters ["@MovieId", upcast tag.MovieId; "@UserId", upcast tag.UserId; "@Tag", upcast tag.Tag; "@Timestamp", upcast tag.Timestamp]
                        |> executeNonQuery "INSERT INTO Tags (MovieId, UserId, Tag, [Timestamp]) VALUES (@MovieId, @UserId, @Tag, @Timestamp)" })
                |> Run.sequentially
            // Links
            printfn "Creating table Links..."
            do! command |> executeNonQuery "CREATE TABLE Links (
                MovieId INT NOT NULL FOREIGN KEY REFERENCES Movies(MovieId),
                ImdbId INT NOT NULL,
                TmdbId INT NULL,
                PRIMARY KEY (MovieId),
                UNIQUE (ImdbId))"
            let! links = Links.AsyncGetSample()
            do!
                links.Rows
                |> Seq.mapi (fun ix link -> async {
                    printfn "Inserting Links table row #%i..." ix
                    do!
                        command
                        |> setParameters ["@MovieId", upcast link.MovieId; "@ImdbId", upcast link.ImdbId; "@TmdbId", upcast link.TmdbId]
                        |> executeNonQuery "INSERT INTO Links (MovieId, ImdbId, TmdbId) VALUES (@MovieId, @ImdbId, @TmdbId)" })
                |> Run.sequentially
            // GenomeTags
            printfn "Creating table GenomeTags..."
            do! command |> executeNonQuery "CREATE TABLE GenomeTags (TagId INT NOT NULL PRIMARY KEY, Tag VARCHAR(255) NOT NULL)"
            let! genomeTags = GenomeTags.AsyncGetSample()
            do!
                genomeTags.Rows
                |> Seq.mapi (fun ix genomeTag -> async {
                    printfn "Inserting GenomeTags table row #%i..." ix
                    do!
                        command
                        |> setParameters ["@TagId", upcast genomeTag.TagId; "@Tag", upcast genomeTag.Tag]
                        |> executeNonQuery "INSERT INTO GenomeTags (TagId, Tag) VALUES (@TagId, @Tag)" })
                |> Run.sequentially
            // GenomeScores
            printfn "Creating table GenomeScores..."
            do! command |> executeNonQuery "CREATE TABLE GenomeScores (
    	        MovieId INT NOT NULL FOREIGN KEY REFERENCES Movies(MovieId),
    	        TagId INT NOT NULL FOREIGN KEY REFERENCES GenomeTags(TagId),
    	        Relevance DECIMAL(28,27) NOT NULL
    	        PRIMARY KEY (MovieId, TagId))"
            let! genomeScores = GenomeScores.AsyncGetSample()
            do!
                genomeScores.Rows
                |> Seq.mapi (fun ix genomeScore -> async {
                    printfn "Inserting GenomeScores table row #%i..." ix
                    do!
                        command
                        |> setParameters ["@MovieId", upcast genomeScore.MovieId; "@TagId", upcast genomeScore.TagId; "@Relevance", upcast genomeScore.Relevance]
                        |> executeNonQuery "INSERT INTO GenomeScores(MovieId, TagId, Relevance) VALUES (@MovieId, @TagId, @Relevance)" })
                |> Run.sequentially }

