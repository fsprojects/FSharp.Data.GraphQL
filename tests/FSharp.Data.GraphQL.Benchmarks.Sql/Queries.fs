module FSharp.Data.GraphQL.Benchmarks.Sql.Queries

let movie = """query q {
	requestId
  movie (movieId : 1) {
    title
  }
}"""

let movieUserRating = """query q {
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

let private movieRatings = sprintf """query q {
	requestId
  movie (movieId: 1) {
    title
    genres
    tags %s {
      name
      userId
      timestamp
      relevance
    }
  }
}"""

let movieRatingsDirect = movieRatings ""

let movieRatingsStreamed = movieRatings "@stream"

let movieRatingsDeferred = movieRatings "@defer"