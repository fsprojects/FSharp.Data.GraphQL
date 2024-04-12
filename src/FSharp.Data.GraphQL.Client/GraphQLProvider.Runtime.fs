// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

#if !IS_DESIGNTIME
namespace FSharp.Data.GraphQL.Client

open FSharp.Core.CompilerServices

[<assembly:TypeProviderAssembly("FSharp.Data.GraphQL.Client.DesignTime.dll")>]
do ()
#endif
