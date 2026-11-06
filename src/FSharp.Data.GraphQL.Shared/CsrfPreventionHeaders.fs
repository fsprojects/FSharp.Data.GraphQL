namespace FSharp.Data.GraphQL

/// <summary>
/// The names of the request headers that show a GraphQL server a browser would have sent the request only after a CORS preflight.
/// </summary>
/// <remarks>
/// They are defined here, rather than next to the server options that accept them, so that the client that sends one and
/// the server that checks for it share a single definition.
/// </remarks>
[<RequireQualifiedAccess>]
module CsrfPreventionHeaders =

    /// <summary>
    /// The header the GraphQL client provider of this library sends, with the value <c>1</c>, with every request a browser
    /// would send without a preflight: <c>GET</c> requests and multipart file uploads.
    /// </summary>
    [<Literal>]
    let GraphQLPreflight = "GraphQL-Preflight"

    /// The header Apollo Server's CSRF prevention accepts from clients that send otherwise simple requests, such as file uploads.
    [<Literal>]
    let ApolloRequirePreflight = "Apollo-Require-Preflight"

    /// The header with the operation name that Apollo Server's CSRF prevention accepts, and some Apollo clients send.
    [<Literal>]
    let ApolloOperationName = "X-Apollo-Operation-Name"
