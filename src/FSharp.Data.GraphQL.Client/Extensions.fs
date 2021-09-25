/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System
open System.Collections
open System.Collections.Generic
open System.Text
open System.Globalization
open System.Text.RegularExpressions
open System.Threading
open System.Security.Cryptography

module internal Casing =
    let private nonWordChars = Regex(@"[^a-zA-Z0-9]+", RegexOptions.Compiled)
    let private word = Regex( @"^(?<word>\d+|^[a-z]+|[A-Z]+|[A-Z][a-z]+|\d[a-z]+)+$", RegexOptions.Compiled)
    let toTileCase (text: string) (textInfo: TextInfo) =
        let result = StringBuilder()
        let tokens = nonWordChars.Split(text)
        for token in tokens do
            let wordMatch = word.Match(token)
            let groups = wordMatch.Groups.["word"]
            for capture in groups.Captures do
                result.Append(textInfo.ToTitleCase(capture.Value.ToString())) |> ignore
        result.ToString()

/// Extensions for types used by the GraphQL client library.
[<AutoOpen>]
module internal Extensions =
    type String with
        member this.MD5Hash() =
            Encoding.UTF8.GetBytes(this)
            |> MD5.Create().ComputeHash
            |> Array.map (fun x -> x.ToString("x2"))
            |> Array.reduce (+)

        member this.ToTitleCase(?cultureInfo: CultureInfo) =
            let culture = defaultArg cultureInfo Thread.CurrentThread.CurrentCulture
            let textInfo = culture.TextInfo
            Casing.toTileCase this textInfo

module internal Printing =
    let prettyPrintRecord (record: seq<KeyValuePair<string, obj>>) =
        let builder = StringBuilder()
        let newLine indentation plus =
          builder.AppendLine() |> ignore
          builder.Append(' ', indentation + plus) |> ignore
        let rec print indentation (value: obj) =
            match value with
            | :? seq<KeyValuePair<string, obj>> as r ->
                builder.Append("{") |> ignore
                for i, KeyValue(key, value) in Seq.indexed r do
                    if i > 0 then
                        builder.Append(";") |> ignore
                        newLine indentation 2
                    builder.Append(key.ToTitleCase() + " = ") |> ignore
                    print (indentation + 2) value |> ignore
                builder.Append(" }")
            | :? string as s ->
                builder.Append(sprintf "%A" s)
            | :? IEnumerable as e ->
                let values = e |> Seq.cast<obj> |> Array.ofSeq
                builder.Append("[") |> ignore
                for i = 0 to values.Length - 1 do
                    if i > 0 then builder.Append(";") |> ignore
                    newLine indentation 2
                    print (indentation + 2) values.[i] |> ignore
                if values.Length > 0 then newLine indentation 0
                builder.Append("]")
            | value ->
                builder.Append(sprintf "%A" value)
        let builder = print 0 record
        builder.ToString()


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