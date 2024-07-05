module FSharp.Data.GraphQL.Samples.RelayBookStore.DB

open System.Data
open System.Data.Common
open Donald

let fetchBooksTotalCount (db : IDbConnection) = async {
    let sql = "SELECT COUNT(id) AS n FROM books"

    let! ct = Async.CancellationToken

    let! count =
        db
        |> Db.newCommand sql
        |> Db.setCancellationToken ct
        |> Db.Async.querySingle (fun read -> read.ReadInt32 ("n"))
        |> Async.AwaitTask

    return Option.get count
}

let private readBook (read : DbDataReader) : Book = {
    ID = read.ReadString ("id")
    Title = read.ReadString ("title")
    Year = read.ReadInt32 ("year")
}

let tryFetchBook (id : string) (db : IDbConnection) = async {
    let sql = "SELECT * FROM books WHERE id = @id LIMIT 1"

    let parameters = [ "id", SqlType.String id ]

    let! ct = Async.CancellationToken

    let! maybeBook =
        db
        |> Db.newCommand sql
        |> Db.setParams parameters
        |> Db.setCancellationToken ct
        |> Db.Async.querySingle readBook
        |> Async.AwaitTask

    return maybeBook
}

let fetchBooksPage (maybeCursor : BookCursor option) (isCursorInclusive : bool) (isForward : bool) (limit : int) (db : IDbConnection) =
    if limit < 0 then
        invalidArg (nameof limit) "must be non-negative"

    async {
        let whereClause =
            match maybeCursor with
            | Some _ ->
                if isForward then
                    if isCursorInclusive then
                        "WHERE title > @cursor_title OR (title = @cursor_title AND id >= @cursor_id)"
                    else
                        "WHERE title > @cursor_title OR (title = @cursor_title AND id  > @cursor_id)"
                else if isCursorInclusive then
                    "WHERE title < @cursor_title OR (title = @cursor_title AND id <= @cursor_id)"
                else
                    "WHERE title < @cursor_title OR (title = @cursor_title AND id  < @cursor_id)"
            | None -> ""

        let orderByClause =
            if isForward then
                "ORDER BY title ASC, id ASC"
            else
                "ORDER BY title DESC, id DESC"

        let sql =
            $"""
      SELECT *
      FROM books
      %s{whereClause}
      %s{orderByClause}
      LIMIT %i{limit}
      """

        let parameters = [
            match maybeCursor with
            | Some cursor ->
                "cursor_id", SqlType.String cursor.ID
                "cursor_title", SqlType.String cursor.Title
            | None -> ()
        ]

        let! ct = Async.CancellationToken

        let! records =
            db
            |> Db.newCommand sql
            |> Db.setParams parameters
            |> Db.setCancellationToken ct
            |> Db.Async.query readBook
            |> Async.AwaitTask

        return records
    // if isForward then
    //   records
    // else
    //   List.rev records
    }
