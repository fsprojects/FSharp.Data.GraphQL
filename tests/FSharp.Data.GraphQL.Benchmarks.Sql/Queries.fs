namespace FSharp.Data.GraphQL.Benchmarks.Sql

type DeferralType =
    | Direct
    | Deferred
    | Streamed
    override this.ToString() =
        match this with
        | Direct -> ""
        | Deferred -> "@defer"
        | Streamed -> "@stream"

module Queries =
    let singleMovie = """query q {
	    requestId
      movie (movieId : 1) {
        title
      }
    }"""

    let singleMovieSingleUserRating = """query q {
	    requestId
      movie (movieId: 1) {
        title
        genres
        ratings (userId: 283199) {
          rating
          timestamp
        }
      }
    }"""

    let movieTags (deferralType : DeferralType) = 
        sprintf """query q {
	    requestId
      movie (movieId: 1) {
        title
        genres
        tags %A {
          name
          userId
          timestamp
          relevance
        }
      }
    }""" deferralType

    let movieTagsAndLinks (tagsDeferralType : DeferralType) (linksDeferralType : DeferralType) = 
        sprintf """query q {
	    requestId
      movie (movieId: 1) {
        title
        genres
        tags %A {
          name
          userId
          timestamp
          relevance
        }
        links %A {
          imdbId
          tmdbId
        }
      }
    }""" tagsDeferralType linksDeferralType