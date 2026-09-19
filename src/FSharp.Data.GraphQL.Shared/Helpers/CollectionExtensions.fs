namespace rec FSharp.Data.GraphQL

open System.Collections.Generic

[<AutoOpen>]
module KeyValuePairExtensions =

    let inline kvp key value = KeyValuePair (key, value)
    let inline kvpObj key (value : obj) = KeyValuePair (key, value)

module internal Seq =

    let vtryHead (source : 'T seq) =
        use enumerator = source.GetEnumerator ()
        if not (enumerator.MoveNext ()) then
            ValueNone
        else
            ValueSome enumerator.Current

    let vtryLast (source : 'T seq) =
        use enumerator = source.GetEnumerator ()
        if not (enumerator.MoveNext ()) then
            ValueNone
        else
            let mutable last = enumerator.Current
            while enumerator.MoveNext () do
                last <- enumerator.Current
            ValueSome last

    let vchoose mapping seq =
        seq
        |> Seq.map mapping
        |> Seq.where ValueOption.isSome
        |> Seq.map ValueOption.get

    let vtryFind predicate (source : 'T seq) = source |> Seq.where predicate |> Seq.vtryHead

    let vtryItem index (source : 'T seq) =
        if index < 0 then
            ValueNone
        else
            use enumerator = source.GetEnumerator ()
            let mutable currentIndex = 0
            let mutable result = ValueNone
            let mutable found = false

            while not found && enumerator.MoveNext () do
                if currentIndex = index then
                    result <- ValueSome enumerator.Current
                    found <- true
                else
                    currentIndex <- currentIndex + 1

            result

module internal List =

    let vchoose mapping list = list |> Seq.vchoose mapping |> Seq.toList

    /// <summary>
    /// Merges elements of two lists, returning a new list without duplicates.
    /// </summary>
    /// <param name="f">Function used to determine if any two given elements are considered equal.</param>
    /// <param name="listx">First list with elements to merge.</param>
    /// <param name="listy">Second list with elements to merge.</param>
    let mergeBy f listx listy =
        let uniqx =
            listx
            |> List.filter (fun x -> not <| List.exists (fun y -> f x = f y) listy)
        uniqx @ listy

    /// <summary>
    /// Attempts to find the first element in a list that satisfies the given predicate.
    /// </summary>
    /// <param name="predicate">Function to test each element.</param>
    /// <param name="source">The input list.</param>
    /// <returns>ValueSome of the first matching element, or ValueNone if no match is found.</returns>
    let vtryFind predicate list = list |> Seq.where predicate |> Seq.vtryHead

    let vtryItem index list =
        let rec loop currentIndex list =
            match currentIndex, list with
            | _, [] -> ValueNone
            | 0, head :: _ -> ValueSome head
            | currentIndex, _ :: tail when currentIndex > 0 -> loop (currentIndex - 1) tail
            | _ -> ValueNone

        loop index list

    /// <summary>
    /// Applies a function to each element of a list and returns the first result where the function returns ValueSome.
    /// </summary>
    /// <param name="mapping">Function to apply to each element.</param>
    /// <param name="source">The input list.</param>
    /// <returns>ValueSome of the first successful mapping result, or ValueNone if no match is found.</returns>
    let rec vtryPick mapping (source : 'T list) =
        match source with
        | [] -> ValueNone
        | head :: tail ->
            match mapping head with
            | ValueSome result -> ValueSome result
            | ValueNone -> vtryPick mapping tail

module internal Array =

    let vchoose mapping array = array |> Seq.vchoose mapping |> Seq.toArray

    /// <summary>
    /// Returns a new array with unique elements. Uniqueness is determined by
    /// output of the <paramref name="keyf"/> function.
    /// </summary>
    /// <param name="keyf">Function, which output is used to determine uniqueness of input elements.</param>
    /// <param name="array">Array of elements.</param>
    let distinctBy keyf (array : 'T array) =
        let temp = Array.zeroCreate array.Length
        let mutable i = 0
        let hashSet = HashSet<_>(HashIdentity.Structural<_>)
        for v in array do
            if hashSet.Add (keyf v) then
                temp.[i] <- v
                i <- i + 1
        Array.sub temp 0 i

    let vtryItem index (array : 'T array) =
        if index < 0 || index >= array.Length then
            ValueNone
        else
            ValueSome array[index]

    /// <summary>
    /// Attempts to find the first element in an array that satisfies the given predicate.
    /// </summary>
    /// <param name="predicate">Function to test each element.</param>
    /// <param name="source">The input array.</param>
    /// <returns>ValueSome of the first matching element, or ValueNone if no match is found.</returns>
    let vtryFind predicate (array : 'T array) =
        let mutable i = 0
        let mutable result = ValueNone
        while i < array.Length && result.IsNone do
            if predicate array[i] then
                result <- ValueSome array[i]
            i <- i + 1
        result

    /// <summary>
    /// Applies a function to each element of an array and returns the first result where the function returns ValueSome.
    /// </summary>
    /// <param name="mapping">Function to apply to each element.</param>
    /// <param name="source">The input array.</param>
    /// <returns>ValueSome of the first successful mapping result, or ValueNone if no match is found.</returns>
    let vtryPick (chooser : 'T -> 'U voption) (source : 'T array) =
        let mutable i = 0
        let mutable result = ValueNone
        while i < source.Length && result.IsNone do
            result <- chooser source[i]
            i <- i + 1
        result

module internal Map =

    let vtryFind key (map : Map<_, _>) =
        match map.TryGetValue key with
        | true, value -> ValueSome value
        | false, _ -> ValueNone

    /// <summary>
    /// Merges the entries of two maps by their key, returning new map in result.
    /// </summary>
    /// <param name="mergeFn">
    /// Function, which takes key shared by entries in both maps, first entry's value,
    /// second entry's value to produce a result value used in newly generated map.
    /// </param>
    /// <param name="mapx">First map with elements to merge.</param>
    /// <param name="mapy">Second map with elements to merge.</param>
    let merge mergeFn mapx mapy =
        mapy
        |> Map.fold
            (fun acc ky vy ->
                match Map.tryFind ky acc with
                | Some vx -> Map.add ky (mergeFn ky vx vy) acc
                | None -> Map.add ky vy acc)
            mapx

module Dictionary =

    let addWith (f : 'V -> 'V -> 'V) (key : 'K) (value : 'V) (dict : Dictionary<'K, 'V>) : unit =
        match dict.TryGetValue (key) with
        | true, v -> dict.[key] <- f value v
        | false, _ -> dict.Add (key, value)

module internal DictionaryExtensions =

    type IDictionary<'TKey, 'TValue> with

        member x.TryFind (key : 'TKey) =
            match x.TryGetValue (key) with
            | true, value -> ValueSome value
            | _ -> ValueNone

module internal Set =

    /// <summary>
    /// Maps over each of the <paramref name="set"/> elements, applying function
    /// over each one of them to generate new Set. Sets generated this way are
    /// then flattened into single output set.
    /// </summary>
    /// <param name="f">Function used to generate Set from each of the input's elements.</param>
    /// <param name="set">Input set.</param>
    let collect f set = set |> Set.fold (fun acc e -> acc + f e) Set.empty
