# relay-book-store

This project demonstrates how to build [relay style pagination](https://relay.dev/graphql/connections.htm) against an SQL-like data-source.

To start the project:

```bash
dotnet run
```

Then visit http://localhost:5000/graphiql

Here are some queries you can try:

```gql
{
  books(first: 5) {
    totalCount
    pageInfo {
      hasPreviousPage
      hasNextPage
      startCursor
      endCursor
    }
    edges {
      node {
        id
        title
        year
      }
    }
  }
}
```

```gql
{
  node(id: "Qm9vazpkdW5l") {
    ... on Book {
      id
      title
      year
    }
  }
}
```

Regenerate the database file like this:

```bash
rm -f ./app.db && dotnet fsi ./GenerateDB.fsx
```
