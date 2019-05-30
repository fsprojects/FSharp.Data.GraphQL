﻿/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System
open FSharp.Data.GraphQL

/// A subscription request, containing essential data for requesting a subscription query on the server.
type GraphQLSubscriptionRequest  =
      /// Contains the name of the operation, or None in case the operation is not named.
    { OperationName : string option
      /// Contains the GraphQL query string which will be parsed by the server.
      Query : string
      /// Contains variables used by the query.
      Variables : (string * obj) [] }

/// A subscription response, containing both immediate and deferred results for a subscription request.
type GraphQLSubscriptionResponse =
      /// Contains immediate result for the subscription request.
    { Data : string
      /// Contains the delayed results for the subscription request, which needs to be subscribed to.
      Deferred : IObservable<string> }

/// An interface for implementing subscription handlers for GraphQLProvider.
type IGraphQLSubscriptionHandler =
    /// Wnen implemented, starts a connection to a GraphQL server which supports subscriptions accordingly to the implementation protocol.
    abstract member Connect : string * (string * obj) [] -> IDisposable
    /// When implemented, start a subscription to the GraphQL using the implementation protocol.
    abstract member Subscribe : GraphQLSubscriptionRequest -> GraphQLSubscriptionResponse

type internal WebSocketClientMessage =
    | ConnectionInit of payload : (string * obj) []
    | ConnectionTerminate
    | Start of id : string * payload : GraphQLSubscriptionRequest
    | Stop of id : string

type internal WebSocketServerMessage =
    | ConnectionAck
    | ConnectionError of payload : (string * obj) []
    | Data of id : string * payload : GraphQLSubscriptionResponse
    | Error of id : string option * payload : (string * obj) []
    | Complete of id : string

/// A GraphQL subscription handler which uses GraphQL Over Web Socket interface.
/// See https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md for information.
type GraphQLOverWebSocketSubscriptionHandler() =
    interface IGraphQLSubscriptionHandler with
        member __.Connect(serverUrl, connectionParams) = { new IDisposable with member __.Dispose() = () }
        member __.Subscribe(request) = { Data = null; Deferred = Observable.empty<string> }
