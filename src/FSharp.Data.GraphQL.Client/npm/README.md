# fable-providers-graphql

> The MIT License (MIT) - Copyright (c) 2016 Bazinga Technologies Inc

GraphQL F# Type Provider for Fable. You can find the version of
the type provider for .NET in [NuGet](https://www.nuget.org/packages/FSharp.Data.GraphQL.Client/0.0.1-beta).

This is an alpha release, more info coming soon!

## Installation

```sh
$ npm install --save fable-core
$ npm install --save-dev fable-providers-regex
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-providers-graphql/FSharp.Data.GraphQL.Client.dll" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-providers-graphql/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL
```