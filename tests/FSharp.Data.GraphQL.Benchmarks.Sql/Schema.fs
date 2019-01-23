namespace FSharp.Data.GraphQL.Benchmarks.Sql

#nowarn "40"

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open System.Collections.Generic
open System.Data.SqlClient
open System.Dynamic
open Dapper

type Root =
    { RequestId : string }

type Movie =
    { MovieId : int
      Genres : string
      Title : string }

type Rating =
    { MovieId : int
      UserId : int
      Rating : decimal
      Timestamp : int }

type Link =
    { MovieId : int
      ImdbId : int
      TmdbId : System.Nullable<int> }

type Tag =
    { TagId : int
      MovieId : int
      UserId : int 
      Tag : string
      Timestamp : int }

type GenomeTag =
    { TagId : int
      Tag : string }

type GenomeScore =
    { MovieId : int
      TagId : int
      Relevance : decimal }

module Dapper =
    let query<'Result> (query : string) (connection : SqlConnection) : 'Result seq =
        let cmd = CommandDefinition(query, flags = CommandFlags.None)
        connection.QueryAsync<'Result>(cmd) 
        |> Async.AwaitTask
        |> Observable.ofAsync
        |> Observable.concatSeq
        |> Observable.toSeq

    let queryFirst<'Result> (query : string) (connection : SqlConnection) : Async<'Result> =
        let cmd = CommandDefinition(query, flags = CommandFlags.None)
        connection.QueryFirstAsync<'Result>(cmd) |> Async.AwaitTask
    
    let paramQuery<'Result> (query : string) (param : obj) (connection : SqlConnection) : 'Result seq =
        let cmd = CommandDefinition(query, param, flags = CommandFlags.None)
        connection.QueryAsync<'Result>(cmd)
        |> Async.AwaitTask
        |> Observable.ofAsync
        |> Observable.concatSeq
        |> Observable.toSeq

    let paramQueryFirst<'Result> (query : string) (param : obj) (connection : SqlConnection) : Async<'Result> =
        let cmd = CommandDefinition(query, param, flags = CommandFlags.None)
        connection.QueryFirstAsync<'Result>(cmd) |> Async.AwaitTask
    
    let mapParamQuery<'Result> (query : string) (param : Map<string,_>) (connection : SqlConnection) : 'Result seq =
        let expando = ExpandoObject()
        let expandoDictionary = expando :> IDictionary<string,obj>
        for paramValue in param do
            expandoDictionary.Add(paramValue.Key, paramValue.Value :> obj)
        connection |> paramQuery query expando

    let mapParamQueryFirst<'Result> (query : string) (param : Map<string,_>) (connection : SqlConnection) : Async<'Result> =
        let expando = ExpandoObject()
        let expandoDictionary = expando :> IDictionary<string,obj>
        for paramValue in param do
            expandoDictionary.Add(paramValue.Key, paramValue.Value :> obj)
        connection |> paramQueryFirst query expando

module Database =
    let [<Literal>] private connectionString = 
        "Data Source=.;Initial Catalog=MovieLens;Integrated Security=True"

    let openConnection connectionString = async {
        let conn = new SqlConnection(connectionString)
        do! conn.OpenAsync() |> Async.AwaitTask
        return conn }

    let getMovie (id : int) = async {
        let! connection = openConnection connectionString
        return!
            connection
            |> Dapper.mapParamQueryFirst<Movie> "SELECT TOP 1 * FROM Movies WHERE MovieId = @MovieId" (Map.ofSeq ["@MovieId", id]) }

    let tryGetMovie id = async {
        let! movie = getMovie id
        if movie = Unchecked.defaultof<Movie>
        then return None
        else return Some movie }

    let getTag (movieId : int) = async {
        let! connection = openConnection connectionString
        return!
            connection
            |> Dapper.mapParamQueryFirst<Tag> "SELECT TOP 1 * FROM Tags WHERE MovieId = @MovieId" (Map.ofSeq ["@MovieId", movieId]) }

    let tryGetTag id = async {
        let! tag = getTag id
        if tag = Unchecked.defaultof<Tag>
        then return None
        else return Some tag }

    let getTagsOfUser (userId : int) (movieId : int) = async {
        let! connection = openConnection connectionString
        return
            connection
            |> Dapper.mapParamQuery<Tag> "SELECT * FROM Tags WHERE UserId = @UserId AND MovieId = @MovieId" (Map.ofSeq ["@UserId", userId; "@MovieId", movieId]) }

    let getTags (movieId : int) = async {
        let! connection = openConnection connectionString
        return
            connection
            |> Dapper.mapParamQuery<Tag> "SELECT * FROM Tags WHERE MovieId = @MovieId" (Map.ofSeq ["@MovieId", movieId]) }

    let getRatingsOfUser (userId : int) (movieId : int) = async {
        let! connection = openConnection connectionString
        return
            connection
            |> Dapper.mapParamQuery<Rating> "SELECT * FROM Ratings WHERE UserId = @UserId AND MovieId = @MovieId" (Map.ofSeq ["@UserId", userId; "@MovieId", movieId]) }

    let getRatings (movieId : int) = async {
        let! connection = openConnection connectionString
        return
            connection
            |> Dapper.mapParamQuery<Rating> "SELECT * FROM Ratings WHERE MovieId = @MovieId" (Map.ofSeq ["@MovieId", movieId]) }

    let getLinks (movieId : int) = async {
        let! connection = openConnection connectionString
        return
            connection
            |> Dapper.mapParamQuery<Link> "SELECT * FROM Links WHERE MovieId = @MovieId" (Map.ofSeq ["@MovieId", movieId]) }

    let getGenomeScoresForTag (tagId : int) (movieId : int) = async {
        let! connection = openConnection connectionString
        return
            connection
            |> Dapper.mapParamQuery<GenomeScore> "SELECT * FROM GenomeScores WHERE TagId = @TagId AND MovieId = @MovieId" (Map.ofSeq ["@TagId", tagId; "@MovieId", movieId]) }

    let getGenomeScores (movieId : int) = async {
        let! connection = openConnection connectionString
        return
            connection
            |> Dapper.mapParamQuery<GenomeScore> "SELECT * FROM GenomeScores WHERE MovieId = @MovieId" (Map.ofSeq ["@MovieId", movieId]) }

module SchemaDefinition =
    let rec Rating =
        Define.Object(
            name = "Rating",
            isTypeOf = (fun o -> o :? Rating),
            fieldsFn = fun () ->
                [ Define.Field("movieId", Int, resolve = fun _ (r : Rating) -> r.MovieId)
                  Define.AsyncField("movie", Movie, resolve = fun _ (r : Rating) -> Database.getMovie r.MovieId)
                  Define.Field("userId", Int, resolve = fun _ (r : Rating) -> r.UserId)
                  Define.Field("rating", Float, resolve = fun _ (r : Rating) -> System.Convert.ToDouble(r.Rating))
                  Define.Field("timestamp", Int, resolve = fun _ (r : Rating) -> r.Timestamp) ])

    and Link =
        Define.Object(
            name = "Link",
            isTypeOf = (fun o -> o :? Link),
            fieldsFn = fun () ->
                [ Define.Field("movieId", Int, resolve = fun _ (l : Link) -> l.MovieId)
                  Define.AsyncField("movie", Movie, resolve = fun _ (l : Link) -> Database.getMovie l.MovieId)
                  Define.Field("imdbId", Int, resolve = fun _ (l : Link) -> l.ImdbId)
                  Define.Field("tmdbId", Nullable Int, resolve = fun _ (l : Link) -> l.TmdbId |> Option.ofNullable) ])

    and Tag =
        Define.Object(
            name = "Tag",
            isTypeOf = (fun o -> o :? Tag),
            fieldsFn = fun () ->
                [ Define.Field("tagId", Int, resolve = fun _ (t : Tag) -> t.TagId)
                  Define.Field("movieId", Int, resolve = fun _ (t : Tag) -> t.MovieId)
                  Define.AsyncField("movie", Movie, resolve = fun _ (t : Tag) -> Database.getMovie t.MovieId)
                  Define.Field("userId", Int, resolve = fun _ (t : Tag) -> t.UserId)
                  Define.Field("timestamp", Int, resolve = fun _ (t : Tag) -> t.Timestamp) ])

    and GenomeTag =
        Define.Object(
            name = "GenomeTag",
            isTypeOf = (fun o -> o :? GenomeTag),
            fieldsFn = fun () ->
                [ Define.Field("tagId", Int, resolve = fun _ (gt : GenomeTag) -> gt.TagId)
                  Define.AsyncField("tag", Tag, resolve = fun _ (gt : GenomeTag) -> Database.getTag gt.TagId)
                  Define.Field("value", String, resolve = fun _ (gt : GenomeTag) -> gt.Tag) ])

    and GenomeScore =
        Define.Object(
            name = "GenomeScore",
            isTypeOf = (fun o -> o :? GenomeScore),
            fieldsFn = fun () ->
                [ Define.Field("movieId", Int, resolve = fun _ (gs : GenomeScore) -> gs.MovieId)
                  Define.AsyncField("movie", Movie, resolve = fun _ (gs : GenomeScore) -> Database.getMovie gs.MovieId)
                  Define.Field("tagId", Int, resolve = fun _ (gs : GenomeScore) -> gs.TagId)
                  Define.AsyncField("tag", Tag, resolve = fun _ (gs : GenomeScore) -> Database.getTag gs.MovieId)
                  Define.Field("relevance", Float, resolve = fun _ (gs : GenomeScore) -> System.Convert.ToDouble(gs.Relevance)) ])

    and Movie =
        Define.Object(
            name = "Movie",
            isTypeOf = (fun o -> o :? Movie),
            fieldsFn = fun () ->
                [ Define.Field("movieId", Int, resolve = fun _ (m : Movie) -> m.MovieId)
                  Define.Field("title", String, resolve = fun _ (m : Movie) -> m.Title)
                  Define.Field("genres", ListOf String, resolve = fun _ (m : Movie) -> m.Genres.Split('|'))
                  Define.AsyncField(
                    "ratings", 
                    ListOf Rating,
                    "Gets movie ratings",
                    args = [ Define.Input("userId", Nullable Int) ],
                    resolve = fun ctx (m : Movie) -> 
                        match ctx.TryArg("userId") with
                        | Some (Some userId) -> Database.getRatingsOfUser userId m.MovieId
                        | _ -> Database.getRatings m.MovieId)
                  Define.AsyncField(
                    "links", 
                    ListOf Link,
                    "Gets movie links",
                    resolve = fun _ (m : Movie) -> Database.getLinks m.MovieId)
                  Define.AsyncField(
                    "tags", 
                    ListOf Tag, 
                    "Gets movie tags",
                    args = [ Define.Input("userId", Nullable Int) ],
                    resolve = fun ctx (m : Movie) -> 
                        match ctx.TryArg("userId") with
                        | Some (Some userId) -> Database.getTagsOfUser userId m.MovieId
                        | _ -> Database.getTags m.MovieId)
                  Define.AsyncField(
                    "genomeScores", 
                    ListOf GenomeScore,
                    "Gets movie genome scores",
                    args = [ Define.Input("tagId", Nullable Int) ],
                    resolve = fun ctx (m : Movie) -> 
                        match ctx.TryArg("tagId") with
                        | Some (Some tagId) -> Database.getGenomeScoresForTag tagId m.MovieId
                        | _ -> Database.getGenomeScores m.MovieId) ])
    let Query = 
        Define.Object<Root>(
            name = "Query",
            fields = [
                Define.Field(
                    "requestId",
                    String,
                    resolve = fun _ (r : Root) -> r.RequestId)
                Define.AsyncField(
                    "movie", 
                    Nullable Movie, 
                    "Gets movie by it's id",
                    args = [ Define.Input("movieId", Int) ], 
                    resolve = fun ctx _ -> Database.tryGetMovie (ctx.Arg("movieId"))) ])