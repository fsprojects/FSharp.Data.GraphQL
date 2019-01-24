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
      objects {
        movie(movieId: 1) {
          title
        }
      }
    }"""

    let singleMovieSingleUserRating = 
        """query q {
      requestId
      objects {
        movie(movieId: 1) {
          title
          genres
          ratings(userId: 283199) {
            rating
            timestamp
          }
        }
      }
    }"""

    let movieTags (deferralType : DeferralType) = 
        sprintf """query q {
      requestId
      objects {
        movie(movieId: 1) {
          title
          genres
          tags %s {
            name
            userId
            timestamp
            relevance
          }
        }
      }
    }""" (deferralType.ToString())

    let movieTagsAndRatings (tagsDeferralType : DeferralType) (linksDeferralType : DeferralType) = 
        sprintf """query q {
      requestId
      objects {
        movie (movieId: 1) {
          title
          genres
          tags %s {
            name
            userId
            timestamp
            relevance
          }
          ratings %s {
            userId
            rating
            timestamp
          }
        }
      }
    }""" (tagsDeferralType.ToString()) (linksDeferralType.ToString())

    let allMovies (deferralType : DeferralType) =
        sprintf """query q {
        requestId
        collections {
        movies %s {
            movieId
            title
            genres
        }
        }
    }""" (deferralType.ToString())

    let fiftyMoviesWithLinks (moviesDeferralType :DeferralType) (linksDeferralType : DeferralType) =
        sprintf """query q {
      requestId
      collections {
        movies (count: 50) %s {
          movieId
          title
          genres
          links %s {
            imdbId
            tmdbId
          }
        }
      }
    }""" (moviesDeferralType.ToString()) (linksDeferralType.ToString())