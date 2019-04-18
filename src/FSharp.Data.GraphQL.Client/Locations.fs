/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System

type TextLocation =
    | String of query : string
    | File of path : string
    static member internal Create(value : string, resolutionFolder : string) =
        try
            let file = System.IO.Path.Combine(resolutionFolder, value)
            if System.IO.File.Exists(file)
            then File file
            else String value
        with _ -> String value

type IntrospectionLocation =
    | Uri of address : string
    | IntrospectionFile of path : string
    static member internal Create(value : string, resolutionFolder : string) =
        let tryUri address =
            match Uri.TryCreate(address, UriKind.Absolute) with
            | (true, _) -> Uri value
            | _ -> failwithf "Could not determine location of introspection. The introspection should be a valid GraphQL server URL, or a introspection JSON file on the path of the project or script."
        try
            let file = System.IO.Path.Combine(resolutionFolder, value)
            if System.IO.File.Exists(file)
            then IntrospectionFile file
            else tryUri value
        with _ -> tryUri value
