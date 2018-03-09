import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import ApolloClient from 'apollo-client';
import { WebSocketLink } from 'apollo-link-ws';
import { InMemoryCache } from 'apollo-cache-inmemory';

const link = new WebSocketLink({
    uri: 'ws://localhost:8084/',
    options: {
        reconnect: true
    }
});

const client = new ApolloClient({
    link: link,
    cache: new InMemoryCache()
})

let app = <h1>Testing ApolloClient</h1>

ReactDOM.render(app, document.getElementById('root'));
