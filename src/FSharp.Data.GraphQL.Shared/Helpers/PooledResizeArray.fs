namespace FSharp.Data.GraphQL

open Collections.Pooled

type internal PooledResizeArray<'T> = PooledList<'T>

module internal PooledResizeArray =

    let ofSeqWithCapacity capacity items =
        let list = new PooledResizeArray<'T> (capacity = capacity)
        list.AddRange (collection = items)
        list

    let exists f (list : PooledResizeArray<'T>) = list.Exists (f)

    let length (list : PooledResizeArray<'T>) = list.Count

    let vChoose f (list : PooledResizeArray<'T>) =
        let res = new PooledResizeArray<_> (list.Count)

        for i = 0 to length list - 1 do
            match f list[i] with
            | ValueNone -> ()
            | ValueSome b -> res.Add (b)

        res

    let choose f (list : PooledResizeArray<'T>) =
        let res = new PooledResizeArray<_> (list.Count)

        for i = 0 to length list - 1 do
            match f list[i] with
            | None -> ()
            | Some b -> res.Add (b)

        res
