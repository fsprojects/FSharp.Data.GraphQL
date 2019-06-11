/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System
open System.Collections.Generic

/// Extensions for types used by the GraphQL client library.
[<AutoOpen>]
module internal Extensions =
    type String with
        /// Returns the input string with the first character in upper case.
        member this.FirstCharUpper() = 
            this.Substring(0, 1).ToUpperInvariant() + this.Substring(1)

        /// Returns the input string with the first character in lower case.
        member this.FirstCharLower() =
            this.Substring(0, 1).ToLowerInvariant() + this.Substring(1)

/// Basic operations on lists.
module internal List =
    /// Produces all possible combination sets for an input list, with all possible sizes.
    /// For each combination set, returns also items that are outside of the combination.
    /// For each item of the result list, the first tuple item is the combination, and
    /// the second tuple item is a list of items that are not in the combination set.
    let combinations (input : 'T list) =
        let rec helper (input : 'T list) (startIndex : int) (length : int) =
            let mutable combinations = List<List<'T>>()
            if length = 2
            then
                let mutable combinationsIndex = 0
                for inputIndex = startIndex to (input.Length - 1) do
                    for i = (inputIndex + 1) to (input.Length - 1) do
                        combinations.Add(List<'T>())
                        combinations.[combinationsIndex].Add(input.[inputIndex])
                        while combinations.[combinationsIndex].Count < length do
                            combinations.[combinationsIndex].Add(input.[i])
                        combinationsIndex <- combinationsIndex + 1
                combinations
            else
                let combinationsOfMore = List<List<'T>>()
                for i = startIndex to (input.Length - length) do
                    combinations <- helper input (i + 1) (length - 1)
                    for index = 0 to (combinations.Count - 1) do
                        combinations.[index].Insert(0, input.[i])
                    for y = 0 to (combinations.Count - 1) do
                        combinationsOfMore.Add(combinations.[y])
                combinationsOfMore
        let output = List<List<'T>>()
        output.Add(List<'T>())
        for i = 0 to (input.Length - 1) do
            let item = List<'T>()
            item.Add(input.[i])
            output.Add(item)
        for i = 2 to input.Length do
            helper input 0 i |> output.AddRange
        output
        |> Seq.map List.ofSeq
        |> List.ofSeq
        |> List.map (fun x -> x, List.except x input)