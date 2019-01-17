namespace FSharp.Data.GraphQL.Benchmarks

#nowarn "40"

open System.Linq
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Middlewares
open System.Data.Entity
open System.ComponentModel.DataAnnotations
open System.ComponentModel.DataAnnotations.Schema

[<CLIMutable>]
type Movie =
    { [<Key>] MovieId : int
      Genres : string
      Title : string }

[<CLIMutable>]
type Rating =
    { [<Key; Column(Order = 0)>] MovieId : int
      [<Key; Column(Order = 1)>] UserId : int
      Rating : decimal
      Timestamp : int }

[<CLIMutable>]
type Link =
    { [<Key>] MovieId : int
      ImdbId : int
      TmdbId : System.Nullable<int> }

[<CLIMutable>]
type Tag =
    { [<Key>] TagId : int
      MovieId : int
      UserId : int 
      Tag : string
      Timestamp : int }

[<CLIMutable>]
type GenomeTag =
    { [<Key>] TagId : int
      Tag : string }

[<CLIMutable>]
type GenomeScore =
    { [<Key; Column(Order = 0)>] MovieId : int
      [<Key; Column(Order = 1)>] TagId : int
      Relevance : decimal }

type MovieDbContext() =
    inherit DbContext()

    [<DefaultValue>]
    val mutable private movies : DbSet<Movie>
    member this.Movies 
        with get() = this.movies
        and set(value) = this.movies <- value

    [<DefaultValue>]
    val mutable private ratings : DbSet<Rating>
    member this.Ratings
        with get() = this.ratings
        and set(value) = this.ratings <- value

    [<DefaultValue>]
    val mutable private links : DbSet<Link>
    member this.Links
        with get() = this.links
        and set(value) = this.links <- value

    [<DefaultValue>]
    val mutable private tags : DbSet<Tag>
    member this.Tags
        with get() = this.tags
        and set(value) = this.tags <- value

    [<DefaultValue>]
    val mutable private genomeTags : DbSet<GenomeTag>
    member this.GenomeTags
        with get() = this.genomeTags
        and set(value) = this.genomeTags <- value

    [<DefaultValue>]
    val mutable private genomeScores : DbSet<GenomeScore>
    member this.GenomeScores
        with get() = this.genomeScores
        and set(value) = this.genomeScores <- value

module ComplexSchemaDefinition =
    let Context = new MovieDbContext()

    let rec Rating =
        Define.Object(
            name = "Rating",
            isTypeOf = (fun o -> o :? Rating),
            fieldsFn = fun () ->
                [ Define.Field("movieId", Int, resolve = fun _ (r : Rating) -> r.MovieId)
                  Define.Field("movie", Movie, resolve = fun _ (r : Rating) -> query {
                    for movie in Context.Movies do
                    find (r.MovieId = movie.MovieId) })
                  Define.Field("userId", Int, resolve = fun _ (r : Rating) -> r.UserId)
                  Define.Field("rating", Float, resolve = fun _ (r : Rating) -> System.Convert.ToDouble(r.Rating))
                  Define.Field("timestamp", Int, resolve = fun _ (r : Rating) -> r.Timestamp) ])

    and Link =
        Define.Object(
            name = "Link",
            isTypeOf = (fun o -> o :? Link),
            fieldsFn = fun () ->
                [ Define.Field("movieId", Int, resolve = fun _ (l : Link) -> l.MovieId)
                  Define.Field("movie", Movie, resolve = fun _ (l : Link) -> query {
                    for movie in Context.Movies do
                    find (l.MovieId = movie.MovieId) })
                  Define.Field("imdbId", Int, resolve = fun _ (l : Link) -> l.ImdbId)
                  Define.Field("tmdbId", Nullable Int, resolve = fun _ (l : Link) -> l.TmdbId |> Option.ofNullable) ])

    and Tag =
        Define.Object(
            name = "Tag",
            isTypeOf = (fun o -> o :? Tag),
            fieldsFn = fun () ->
                [ Define.Field("tagId", Int, resolve = fun _ (t : Tag) -> t.TagId)
                  Define.Field("movieId", Int, resolve = fun _ (t : Tag) -> t.MovieId)
                  Define.Field("movie", Movie, resolve = fun _ (t : Tag) -> query {
                    for movie in Context.Movies do
                    find (t.MovieId = movie.MovieId) })
                  Define.Field("userId", Int, resolve = fun _ (t : Tag) -> t.UserId)
                  Define.Field("timestamp", Int, resolve = fun _ (t : Tag) -> t.Timestamp) ])

    and GenomeTag =
        Define.Object(
            name = "GenomeTag",
            isTypeOf = (fun o -> o :? GenomeTag),
            fieldsFn = fun () ->
                [ Define.Field("tagId", Int, resolve = fun _ (gn : GenomeTag) -> gn.TagId)
                  Define.Field("tag", Tag, resolve = fun _ (gn : GenomeTag) -> query {
                    for tag in Context.Tags do
                    find (gn.TagId = tag.TagId) })
                  Define.Field("value", String, resolve = fun _ (gn : GenomeTag) -> gn.Tag) ])

    and GenomeScore =
        Define.Object(
            name = "GenomeScore",
            isTypeOf = (fun o -> o :? GenomeScore),
            fieldsFn = fun () ->
                [ Define.Field("movieId", Int, resolve = fun _ (gs : GenomeScore) -> gs.MovieId)
                  Define.Field("movie", Movie, resolve = fun _ (gs : GenomeScore) -> query {
                    for movie in Context.Movies do
                    find (gs.MovieId = movie.MovieId) })
                  Define.Field("tagId", Int, resolve = fun _ (gs : GenomeScore) -> gs.TagId)
                  Define.Field("tag", Tag, resolve = fun _ (gs : GenomeScore) -> query {
                    for tag in Context.Tags do
                    find (gs.TagId = tag.TagId) })
                  Define.Field("relevance", Float, resolve = fun _ (gs : GenomeScore) -> System.Convert.ToDouble(gs.Relevance)) ])

    and Movie =
        Define.Object(
            name = "Movie",
            isTypeOf = (fun o -> o :? Movie),
            fieldsFn = fun () ->
                [ Define.Field("movieId", Int, resolve = fun _ (m : Movie) -> m.MovieId)
                  Define.Field("title", String, resolve = fun _ (m : Movie) -> m.Title)
                  Define.Field("genres", ListOf String, resolve = fun _ (m : Movie) -> m.Genres.Split('|'))
                  Define.Field(
                    "ratings", 
                    ListOf Rating, 
                    args = [ Define.Input("userId", Nullable Int) ],
                    resolve = fun ctx (m : Movie) -> 
                        let ratings = query {
                            for rating in Context.Ratings do
                            where (rating.MovieId = m.MovieId)
                            select rating }
                        match ctx.Arg("userId") with
                        | Some userId -> query {
                            for rating in ratings do
                            where (rating.UserId = userId)
                            select rating }
                        | None -> ratings)
                  Define.Field("links", ListOf Link, resolve = fun _ (m : Movie) -> query {
                    for link in Context.Links do
                    where (link.MovieId = m.MovieId)
                    select link })
                  Define.Field(
                    "tags", 
                    ListOf Tag, 
                    args = [ Define.Input("userId", Nullable Int) ],
                    resolve = fun ctx (m : Movie) -> 
                        let tags = query {
                            for tag in Context.Tags do
                            where (tag.MovieId = m.MovieId)
                            select tag }
                        match ctx.Arg("userId") with
                        | Some userId -> query {
                            for tag in tags do
                            where (tag.UserId = userId)
                            select tag }
                        | None -> tags)
                  Define.Field(
                    "genomeScores", 
                    ListOf GenomeScore,
                    args = [ Define.Input("tagId", Nullable Tag) ],
                    resolve = fun ctx (m : Movie) -> 
                        let scores = query {
                            for score in Context.GenomeScores do
                            where (score.MovieId = m.MovieId)
                            select score }
                        match ctx.Arg("tagId") with
                        | Some tagId -> query { 
                            for score in scores do
                            where (score.TagId = tagId) 
                            select score }
                        | None -> scores) ])