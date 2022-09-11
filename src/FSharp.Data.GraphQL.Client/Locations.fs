// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System

/// Represents the location of a string resource.
type StringLocation =
    /// Specifies that the string resource is provided as a value.
    | String of string
    /// Specifies that the string resource is contained in a file.
    | File of path : string
    /// <summary>
    /// Tries to create a string resource using providing information.
    /// </summary>
    /// <param name="value">
    /// The value of the resource. It can be a string containing the file name or the file path, or the string itself.
    /// The method tries to identify if a file exists with the value. If it does not exist, the string is provided as a value.
    /// </param>
    /// <param name="folderPath">An optional folder path. Provide it when the value parameter is a file without its path.</param>
    static member Create(value : string, ?folderPath : string) =
        try
            let file =
                match folderPath with
                | Some folderPath -> System.IO.Path.Combine(folderPath, value)
                | None -> value
            if System.IO.File.Exists(file)
            then File file
            else String value
        with _ -> String value

/// Represents the location of an introspection schema.
type IntrospectionLocation =
    /// Specifies that the introspection schema resource is provided as a value.
    | Uri of address : string
    /// Specifies that the introspection schema resource is provided in a JSON file.
    | IntrospectionFile of path : string
    /// <summary>
    /// Tries to create a introspection schema resource using providing information.
    /// </summary>
    /// <param name="value">
    /// The value of the resource. It can be a string containing the file name or the file path, or the string itself.
    /// The method tries to identify if a file exists with the value. If it does not exist, the introspection schema is provided as a value.
    /// </param>
    /// <param name="folderPath">An optional folder path. Provide it when the value parameter is a file without its path.</param>
    static member Create(value : string, ?folderPath : string) =
        let tryUri address =
            match Uri.TryCreate(address, UriKind.Absolute) with
            | (true, _) -> Uri value
            | _ -> failwithf "Could not determine location of introspection. The introspection should be a valid GraphQL server URL, or a introspection JSON file on the path of the project or script."
        try
            let file =
                match folderPath with
                | Some folderPath -> System.IO.Path.Combine(folderPath, value)
                | None -> value
            if System.IO.File.Exists(file)
            then IntrospectionFile file
            else tryUri value
        with _ -> tryUri value
