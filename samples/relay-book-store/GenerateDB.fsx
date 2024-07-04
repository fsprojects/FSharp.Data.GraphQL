#r "nuget: Microsoft.Data.Sqlite, 8.0.6"
#r "nuget: Donald, 10.1.0"

open Microsoft.Data.Sqlite
open Donald

let slugify (x : string) =
  x.Replace(' ', '-').Replace(''', '-').ToLowerInvariant()

let books =
  [
    "Accelerando", 2005
    "Consider Phlebas", 1987
    "Dune", 1965
    "Ender's Game", 1985
    "Interface", 1994
    "Jurrasic Park", 1990
    "Roadside Picnic", 1972
    "Stand on Zanzibar", 1968
    "The Sheep Look Up", 1972
    "The Mountain Trail and its Message", 1997
  ]

let db = new SqliteConnection("Data Source=app.db")

db
|> Db.newCommand "CREATE TABLE books (id PRIMARY KEY, title, year); "
|> Db.exec

db
|> Db.newCommand "INSERT INTO books (id, title, year) VALUES (@id, @title, @year)"
|> Db.execMany
  [
    for book in books do
      let title, year = book
      let id = slugify title
      [ "id", SqlType.String id; "title", SqlType.String title; "year", SqlType.Int year ]
  ]
