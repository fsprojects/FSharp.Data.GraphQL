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
      logger.LogInformation($"fetchBooksPage id={id}")

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
  Define.AsyncField(
    "books",
    ConnectionOf bookType,
    "Pages through all books",
    Connection.allArgs,
    fun ctx (root : Root) ->
      async {
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

        let fetchTotalCount =
          async {
            let! count = root.FetchBooksTotalCount()

            return Some count
          }

        let! hasPreviousPage, hasNextPage, nodes =
          match sliceInfo with
          | Forward (first, after) ->
            async {
              let hasPreviousPage =
                async {
                  if Option.isSome after then
                    let! items = root.FetchBooksPage(after, true, false, 1)

                    return not (List.isEmpty items)
                  else
                    return false
                }

              let! items = root.FetchBooksPage(after, false, true, first + 1)

              let hasNextPage = async { return List.length items > first }
              let edges = List.truncate first items

              return hasPreviousPage, hasNextPage, edges
            }
          | Backward (last, before) ->
            async {
              let hasNextPage =
                async {
                  if Option.isSome before then
                    let! items = root.FetchBooksPage(before, true, true, 1)

                    return List.isEmpty items |> not
                  else
                    return false
                }

              let! items = root.FetchBooksPage(before, false, false, last + 1)

              let hasPreviousPage =
                async {
                  return List.length items > last
                }

              let edges =
                items
                |> List.truncate last
                |> List.rev

              return hasPreviousPage, hasNextPage, edges
            }

        let edges =
          nodes
          |> List.map
            (fun node ->
              {
                Node = node
                Cursor = node |> BookCursor.ofBook |> BookCursor.encode
              })

        let startCursor =
          nodes
          |> Seq.tryHead
          |> Option.map (BookCursor.ofBook >> BookCursor.encode)

        let endCursor =
          nodes
          |> Seq.tryLast
          |> Option.map (BookCursor.ofBook >> BookCursor.encode)

        return
          {
            TotalCount = fetchTotalCount
            PageInfo =
              {
                HasNextPage = hasNextPage
                HasPreviousPage = hasPreviousPage
                StartCursor = startCursor
                EndCursor = endCursor
              }
            Edges = edges
          }
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
