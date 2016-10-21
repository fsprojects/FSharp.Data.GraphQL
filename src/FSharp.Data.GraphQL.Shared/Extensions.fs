/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module internal FSharp.Data.GraphQL.Extensions

open System.Reflection

type TypeInfo with
    /// If no property is found with the specified name, it will try changing the case of the first letter
    member x.GetDeclaredProperty(propertyName: string, ignoreCase: bool) =
        match x.GetDeclaredProperty(propertyName), ignoreCase with
        | null, true ->
            let first =
                let first = propertyName.Substring(0,1)
                match first.ToUpper() with
                | upper when upper <> first -> upper
                | _ -> first.ToLower()
            x.GetDeclaredProperty(first + propertyName.Substring(1))
        | prop, _ -> prop

    /// If no method is found with the specified name, it will try changing the case of the first letter
    member x.GetDeclaredMethod(propertyName: string, ignoreCase: bool) =
        match x.GetDeclaredMethod(propertyName), ignoreCase with
        | null, true ->
            let first =
                let first = propertyName.Substring(0,1)
                match first.ToUpper() with
                | upper when upper <> first -> upper
                | _ -> first.ToLower()
            x.GetDeclaredMethod(first + propertyName.Substring(1))
        | prop, _ -> prop
