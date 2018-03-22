import React from 'react';
import ReactDOM from 'react-dom';
import GraphiQL from 'graphiql';
import fetch from 'isomorphic-fetch';

function graphQLFetcher(graphQLParams) {
  return fetch('http://localhost:8084/', {
    method: 'post',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(graphQLParams),
  }).then(response => response.json());
}

var subscriptionsClient = new window.SubscriptionsTransportWs.SubscriptionClient('ws://localhost:8084/', { reconnect: true });
var subscriptionsFetcher = window.GraphiQLSubscriptionsFetcher.graphQLFetcher(subscriptionsClient, graphQLFetcher);

ReactDOM.render(<GraphiQL fetcher={subscriptionsFetcher} />, document.getElementById('graphiql-app'));
