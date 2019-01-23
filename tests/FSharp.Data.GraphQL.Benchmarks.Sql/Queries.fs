module FSharp.Data.GraphQL.Benchmarks.Sql.Queries

let simple = """query simpleQuery {
	requestId
  movie (movieId : 1) {
    title
  }
}"""

let flat = """query flatQuery {
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

let longList = """query longListQuery {
	requestId
  movie (movieId: 1) {
    title
    genres
    ratings {
      userId
      rating
      timestamp
    }
  }
}"""

let longStream = """query longStreamQuery {
	requestId
  movie (movieId: 1) {
    title
    genres
    ratings @stream {
      userId
      rating
      timestamp
    }
  }
}"""