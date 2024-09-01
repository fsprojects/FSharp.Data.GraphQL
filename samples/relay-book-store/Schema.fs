module FSharp.Data.GraphQL.Samples.RelayBookStore.Schema

open System.Data
open Microsoft.Extensions.Logging
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Relay

[<NoComparison>]
type Root (logger : ILogger, db : IDbConnection) =
    member this.TryFetchBook (id : string) = async {
        logger.LogInformation ($"tryFetchBook id={id}")

        return! DB.tryFetchBook id db
    }

    member this.FetchBooksTotalCount () = async {
        logger.LogInformation ($"fetchBooksTotalCount")

        return! DB.fetchBooksTotalCount db
    }

    member this.FetchBooksPage (maybeCursor : BookCursor voption, isCursorInclusive : bool, isForward : bool, limit : int) = async {
        logger.LogInformation ($"fetchBooksPage maybeCursor={maybeCursor} isCursorInclusive={isCursorInclusive} isForward={isForward} limit={limit}")

        return! DB.fetchBooksPage maybeCursor isCursorInclusive isForward limit db
    }

#nowarn "40"

let rec nodeType = Define.Node<obj> (fun () -> [ bookType ])

and bookType =
    Define.Object (
        "Book",
        fields = [
            Define.GlobalIdField (fun _ (x : Book) -> x.ID)
            Define.Field ("title", StringType, (fun _ (x : Book) -> x.Title))
            Define.Field ("year", IntType, (fun _ (x : Book) -> x.Year))
        ],
        interfaces = [ nodeType ]
    )

let booksField =
    Define.Field (
        "books",
        ConnectionOf bookType,
        "Pages through all books",
        Connection.allArgs,
        fun ctx (root : Root) ->
            let sliceInfo =
                let first = ctx.TryArg ("first")

                let after =
                    ctx.TryArg ("after")
                    |> Option.map (fun s ->
                        match BookCursor.tryDecode s with
                        | Some c -> c
                        | None -> raise (GQLMessageException ("Invalid cursor value for after")))

                let last = ctx.TryArg ("last")

                let before =
                    ctx.TryArg ("before")
                    |> Option.map (fun s ->
                        match BookCursor.tryDecode s with
                        | Some c -> c
                        | None -> raise (GQLMessageException ("Invalid cursor value for before")))

                match first, after, last, before with
                | Some first, _, None, None ->
                    if first < 0 then
                        raise (GQLMessageException ($"first must be at least 0"))

                    Forward (first, vopt after)
                | None, None, Some last, _ ->
                    if last < 0 then
                        raise (GQLMessageException ($"last must be at least 0"))

                    Backward (last, vopt before)
                | None, _, None, _ -> raise (GQLMessageException ($"Must specify first or last"))
                | Some _, _, _, _ -> raise (GQLMessageException ($"Must not combine first with last or before"))
                | _, _, Some _, _ -> raise (GQLMessageException ($"Must not combine last with first or after"))

            // The total number of edges in the data-store, not the number of edges in the page!
            let totalCount = async {
                let! count = root.FetchBooksTotalCount ()

                return Some count
            }

            // Workflow that fetches all of the page items plus an extra one
            // The extra item is used to determine if there is another page
            // This is memoized because it is used to resolve many fields, but we do not know which at this point
            let fetchItems =
                async {
                    match sliceInfo with
                    | Forward (first, after) -> return! root.FetchBooksPage (after, isCursorInclusive = false, isForward = true, limit = first + 1)
                    | Backward (last, before) -> return! root.FetchBooksPage (before, isCursorInclusive = false, isForward = false, limit = last + 1)
                }
                // Store the result similar to what Task does so that we don't go to DB multiple times for the same data
                |> Async.memoize

            // Is there a previous page?
            // If we are paging forward and have not specified an `after`, there can be no previous page by definition
            // If we are paging forward with an `after`, then we must query backwards to test for a previous item
            // If we are paging backward, we can use the extra item to determine if there is a previous page
            let hasPreviousPage = async {
                match sliceInfo with
                | Forward (_, ValueNone) -> return false
                | Forward (_, after) ->
                    let! items = root.FetchBooksPage (after, isCursorInclusive = true, isForward = false, limit = 1)

                    return not (List.isEmpty items)
                | Backward (last, _) ->
                    let! items = fetchItems

                    // We requested `last + 1` items, so if the number of items is greater than `last` we know there is a previous page
                    return List.length items > last
            }

            // Is there a next page?
            // If we are paging forward, then we can use the extra item to check
            // If we are paging backward and have not specified an `before`, there can be no next page by definition
            // If we are paging backward with a `before`, then we must query forwards to test for a previous item
            let hasNextPage = async {
                match sliceInfo with
                | Forward (first, _) ->
                    let! items = fetchItems

                    // We requested `first + 1` items, so if the number of items is greater than `first` we know there is a next page
                    return List.length items > first
                | Backward (_, ValueNone) -> return false
                | Backward (_, before) ->
                    let! items = root.FetchBooksPage (before, isCursorInclusive = true, isForward = true, limit = 1)

                    return List.isEmpty items |> not
            }

            // The edges that are returned to the resolver
            // According to the spec, the edge order must be the same for forward and backward pagination
            // Our database query flips the order, so we must correct it here
            // We also drop the extra item fetched to determine hasNextPage / hasPreviousPage
            let edges = async {
                let! items = fetchItems

                let nodes =
                    match sliceInfo with
                    | Forward _ -> items |> List.truncate sliceInfo.PageSize
                    | Backward _ -> items |> List.truncate sliceInfo.PageSize |> List.rev

                return
                    nodes
                    |> Seq.map (fun node -> { Node = node; Cursor = node |> BookCursor.ofBook |> BookCursor.encode })
            }

            // The cursor of the first edge
            let startCursor = async {
                let! edges = edges

                return edges |> Seq.tryHead |> Option.map (fun x -> x.Cursor)
            }

            // The cursor of the last edge
            let endCursor = async {
                let! edges = edges

                return edges |> Seq.tryLast |> Option.map (fun x -> x.Cursor)
            }

            {
                TotalCount = totalCount
                PageInfo = {
                    HasNextPage = hasNextPage
                    HasPreviousPage = hasPreviousPage
                    StartCursor = startCursor
                    EndCursor = endCursor
                }
                Edges = edges
            }
    )

let nodeField =
    Define.NodeAsyncField (
        nodeType,
        fun _ (root : Root) id -> async {
            match id with
            | GlobalId ("Book", id) ->
                let! maybeBook = root.TryFetchBook (id)

                return maybeBook |> Option.map box
            | _ -> return None
        }
    )

let queryType = Define.Object ("Query", [ booksField; nodeField ])
