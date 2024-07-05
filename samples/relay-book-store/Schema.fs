module FSharp.Data.GraphQL.Samples.RelayBookStore.Schema

open System.Data
open Microsoft.Extensions.Logging
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Relay

[<NoComparison>]
type Root(logger : ILogger, db : IDbConnection) =
  member this.TryFetchBook(id : string) =
    async {
      logger.LogInformation($"tryFetchBook id={id}")

      return! DB.tryFetchBook id db
    }

  member this.FetchBooksTotalCount() =
    async {
      logger.LogInformation($"fetchBooksTotalCount")

      return! DB.fetchBooksTotalCount db
    }

  member this.FetchBooksPage(maybeCursor : BookCursor option, isCursorInclusive : bool, isForward : bool, limit : int) =
    async {
      logger.LogInformation($"fetchBooksPage maybeCursor={maybeCursor} isCursorInclusive={isCursorInclusive} isForward={isForward} limit={limit}")

      return! DB.fetchBooksPage maybeCursor isCursorInclusive isForward limit db
    }

#nowarn "40"

let rec nodeType = Define.Node<obj> (fun () -> [ bookType ])

and bookType =
  Define.Object(
    "Book",
    fields =
      [
        Define.GlobalIdField(fun _ (x : Book)  -> x.ID)
        Define.Field("title", StringType, fun _ (x : Book) -> x.Title)
        Define.Field("year", IntType, fun _ (x : Book) -> x.Year)
      ],
    interfaces = [ nodeType ]
  )

let booksField =
  Define.Field(
    "books",
    ConnectionOf bookType,
    "Pages through all books",
    Connection.allArgs,
    fun ctx (root : Root) ->
      let sliceInfo =
        let first = ctx.TryArg("first")

        let after =
          ctx.TryArg("after")
          |> Option.map
            (fun s ->
              match BookCursor.tryDecode s with
              | Some c -> c
              | None -> raise (GQLMessageException("Invalid cursor value for after")))

        let last = ctx.TryArg("last")

        let before =
          ctx.TryArg("before")
          |> Option.map
            (fun s ->
              match BookCursor.tryDecode s with
              | Some c -> c
              | None -> raise (GQLMessageException("Invalid cursor value for before")))

        match first, after, last, before with
        | Some first, _, None, None ->
          if first < 0 then
            raise (GQLMessageException($"first must be at least 0"))

          Forward (first, after)
        | None, None, Some last, _ ->
          if last < 0 then
            raise (GQLMessageException($"last must be at least 0"))

          Backward (last, before)
        | None, _, None, _ ->
          raise (GQLMessageException($"Must specify first or last"))
        | Some _, _, _, _ ->
          raise (GQLMessageException($"Must not combine first with last or before"))
        | _, _, Some _, _ ->
          raise (GQLMessageException($"Must not combine last with first or after"))

      let totalCount =
        async {
          let! count = root.FetchBooksTotalCount()

          return Some count
        }

      let fetchItems =
        async {
          match sliceInfo with
          | Forward (first, after) ->
            return! root.FetchBooksPage(after, false, true, first + 1)
          | Backward (last, before) ->
            return! root.FetchBooksPage(before, false, false, last + 1)
        }
        |> Async.memoize

      let hasPreviousPage =
        async {
          match sliceInfo with
          | Forward (_, None) ->
            return false
          | Forward (_, after) ->
            let! items = root.FetchBooksPage(after, true, false, 1)

            return not (List.isEmpty items)
          | Backward (last, _) ->
            let! items = fetchItems

            return List.length items > last
        }

      let hasNextPage =
        async {
          match sliceInfo with
          | Forward (first, _) ->
            let! items = fetchItems

            return List.length items > first
          | Backward (_, None) ->
            return false
          | Backward (_, before) ->
            let! items = root.FetchBooksPage(before, true, true, 1)

            return List.isEmpty items |> not
        }

      let edges =
        async {
          let! items = fetchItems

          let nodes =
            match sliceInfo with
            | Forward _ ->
              items
              |> List.truncate sliceInfo.PageSize
            | Backward _ ->
              items
              |> List.truncate sliceInfo.PageSize
              |> List.rev

          return
            nodes
            |> Seq.map
              (fun node ->
                {
                  Node = node
                  Cursor = node |> BookCursor.ofBook |> BookCursor.encode
                })
        }

      let startCursor =
        async {
          let! edges = edges

          return
            edges
            |> Seq.tryHead
            |> Option.map (fun x -> x.Cursor)
        }

      let endCursor =
        async {
          let! edges = edges

          return
            edges
            |> Seq.tryLast
            |> Option.map (fun x -> x.Cursor)
        }

      {
        TotalCount = totalCount
        PageInfo =
          {
            HasNextPage = hasNextPage
            HasPreviousPage = hasPreviousPage
            StartCursor = startCursor
            EndCursor = endCursor
          }
        Edges = edges
      }
  )

let nodeField =
  Define.NodeAsyncField(
    nodeType,
    fun _ (root : Root) id ->
      async {
        match id with
        | GlobalId ("Book", id) ->
          let! maybeBook = root.TryFetchBook(id)

          return maybeBook |> Option.map box
        | _ ->
          return None
      }
    )

let queryType =
  Define.Object(
    "Query",
    [
      booksField
      nodeField
    ]
  )
