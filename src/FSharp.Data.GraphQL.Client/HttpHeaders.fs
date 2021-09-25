/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open FSharp.Core
open FSharp.Data.GraphQL.Client

/// Contains helpers to build HTTP header sequences to be used in GraphQLProvider Run methods.
module HttpHeaders =
    /// Builds a sequence of HTTP headers as a sequence from a pre-formatted header string.
    /// The input headers string should be a string containing headers in the same way they are
    /// organized in a HTTP request (each header in a line, names and values separated by commas).
    let ofString (headers : string) : seq<string * string> =
        upcast (headers.Replace("\r\n", "\n").Split('\n')
                |> Array.map (fun header ->
                    let separatorIndex = header.IndexOf(':')
                    if separatorIndex = -1
                    then failwithf "Header \"%s\" has an invalid header format. Must provide a name and a value, both separated by a comma." header
                    else
                        let name = header.Substring(0, separatorIndex).Trim()
                        let value = header.Substring(separatorIndex + 1).Trim()
                        (name, value)))

    /// Builds a sequence of HTTP headers as a sequence from a header file.
    /// The input file should be a file containing headers in the same way they are
    /// organized in a HTTP request (each header in a line, names and values separated by commas).
    let ofFile (path : string) =
        System.IO.File.ReadAllText path |> ofString

    let internal load (location : StringLocation) : seq<string * string> =
        let headersString =
            match location with
            | String headers -> headers
            | File path -> System.IO.File.ReadAllText path
        if headersString = "" then upcast [||]
        else headersString |> ofString