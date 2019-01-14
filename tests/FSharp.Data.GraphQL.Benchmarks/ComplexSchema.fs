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

type MovieDbContext() =
    inherit DbContext()

    [<DefaultValue>]
    val mutable movies : DbSet<Movie>
    member this.Movies 
        with get() = this.movies
        and set(value) = this.movies <- value

    [<DefaultValue>]
    val mutable ratings : DbSet<Rating>
    member this.Ratings
        with get() = this.ratings
        and set(value) = this.ratings <- value

    [<DefaultValue>]
    val mutable links : DbSet<Link>
    member this.Links
        with get() = this.links
        and set(value) = this.links <- value

    [<DefaultValue>]
    val mutable tags : DbSet<Tag>
    member this.Tags
        with get() = this.tags
        and set(value) = this.tags <- value

    [<DefaultValue>]
    val mutable genomeTags : DbSet<GenomeTag>
    member this.GenomeTags
        with get() = this.genomeTags
        and set(value) = this.genomeTags <- value

module SchemaDefinition =
    let Context = new MovieDbContext()

    let rec Rating =
        Define.Object(
            name = "Rating",
            isTypeOf = (fun o -> o :? Rating),
            fieldsFn = fun () ->
                [ Define.Field("movieId", Int, resolve = fun _ (r : Rating) -> r.MovieId)
                  Define.Field("userId", Int, resolve = fun _ (r : Rating) -> r.UserId)
                  Define.Field("rating", Float, resolve = fun _ (r : Rating) -> System.Convert.ToDouble(r.Rating))
                  Define.Field("timestamp", Int, resolve = fun _ (r : Rating) -> r.Timestamp) ])

    let Link =
        Define.Object(
            name = "Link",
            isTypeOf = (fun o -> o :? Link),
            fieldsFn = fun () ->
                [ Define.Field("movieId", Int, resolve = fun _ (l : Link) -> l.MovieId)
                  Define.Field("imdbId", Int, resolve = fun _ (l : Link) -> l.ImdbId)
                  Define.Field("tmdbId", Nullable Int, resolve = fun _ (l : Link) -> l.TmdbId |> Option.ofNullable) ])

    let Movie =
        Define.Object(
            name = "Movie",
            isTypeOf = (fun o -> o :? Movie),
            fieldsFn = fun () ->
                [ Define.Field("movieId", Int, resolve = fun _ (m : Movie) -> m.MovieId)
                  Define.Field("title", String, resolve = fun _ (m : Movie) -> m.Title)
                  Define.Field("genres", ListOf String, resolve = fun _ (m : Movie) -> m.Genres.Split('|'))
                  Define.Field("ratings", ListOf Rating, resolve = fun _ (m : Movie) -> query { 
                    for rating in Context.Ratings do
                    where (rating.MovieId = m.MovieId)
                    select rating })
                  Define.Field("links", ListOf Link, resolve = fun _ (m : Movie) -> query {
                    for link in Context.Links do
                    where (link.MovieId = m.MovieId)
                    select link }) ])