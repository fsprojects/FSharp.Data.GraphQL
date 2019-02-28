/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module internal FSharp.Data.GraphQL.Client.IO

open System.IO

let readTextFile basePath relativePath = 
    Path.Combine(basePath, relativePath) |> File.ReadAllText