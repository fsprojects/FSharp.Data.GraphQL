import React from 'react';
import ApolloClient from 'apollo-client';
import { InMemoryCache } from 'apollo-cache-inmemory';
import { SubscriptionClient } from 'subscriptions-transport-ws';
import gql from 'graphql-tag';

const deferQuery = `# This is a sample query with defer directive.
# The GraphQL Giraffe server is configured to return deferred results after 5 seconds,
# so you can observe better how the effects of a defer directive works.

query TestQuery {
    hero(id:"1000") {
        id
        name
        appearsIn
        homePlanet
        friends @defer {
            id
            name
        }
    }
}`;

const streamQuery = `# This is a sample query with stream and defer directive.
# The GraphQL Giraffe server is configured to return each result after 5 seconds,
# so you can observe better how the effects of stream and defer directives work.

query TestQuery {
    hero(id:"1000") {
        id
        name
        appearsIn @defer
        homePlanet
        friends @stream {
            ... on Human {
              name
            }
          	... on Droid {
              name
              primaryFunction
            }
        }
    }
}`

const thresholdQuery = `# This is a sample query that will be denied by a query complexity threshold middleware.
# The field "friends" of both Human and Droid have a query weight of 0.5.
# The maximum query weight (threshold) is set to 2.0 on the server.
# Hero friends will have a weight of 0.5.
# Friends of friends of hero will have a weight of 1.0 (since they can be both Human and Droid).
# Also, their inner friends will have a weight of 1.0.
# Total weight is 2.5, making the query too complex for the threshold meter.

query TestQuery {
    hero(id:"1000") {
        id, friends { id, friends { id, friends { id } } }
    }
}`

const liveQuery = `# This is a sample query using live directive.
# This query will subscribe to updates on the ismoon field by the mutation "setMoon".
# Whenever the mutation is done, the new value will be sent after 5 seconds.

query testQuery {
  planet (id : 1) {
    id
    name
    ismoon @live
  }
}`

export default class QueryRunner extends React.Component {
    constructor(props) {
        super(props);
        var subscription = new SubscriptionClient('ws://localhost:8084/', {
            reconnect: true
        });
        var client = new ApolloClient({
            link: subscription,
            cache: new InMemoryCache()
        });
        this.state = { 
            query: deferQuery,
            subscription: subscription,
            client: client,
            results : ''
        };

        this.handleChange = this.handleChange.bind(this);
        this.handleRun = this.handleRun.bind(this);
        this.handleSampleQueryChange = this.handleSampleQueryChange.bind(this);
    }

    handleChange(event) {
        this.setState({ query: event.target.value });
    }

    handleSampleQueryChange(event) {
        switch (event.target.value)
        {
            case "defer":
                this.setState({ query: deferQuery });
                break;
            case "stream":
                this.setState({ query: streamQuery });
                break;
            case "threshold":
                this.setState({ query: thresholdQuery });
                break;
            case "live":
                this.setState({ query: liveQuery });
            default:
                break;
        }
    }

    handleRun(event) {
        this.setState({ results: '' });
        this.state.subscription.unsubscribeAll();
        this.state.client.subscribe({
            query: gql`${this.state.query}`,
            variables: {}
        }).subscribe({
            next: result => {
                this.setState({ results: this.state.results + JSON.stringify(result) + "\n\n" });
            },
            error: e => {
                this.setState({ results: this.state.results + JSON.stringify(e) + "\n\n" });
            }
        });
        event.preventDefault();
    }

    render() {
        return (
            <div>
                <div>
                    <p>
                        Type a query here, or select one of the sample queries in the selection below. 
                        You can type subscriptions and defer directives, 
                        the client will wait for more responses over Web Socket connection.
                    </p>
                    <p>Once typed, just click "Run query" to connect to the socket and wait for responses.</p>
                </div>
                <div>
                    <p> 
                        Sample queries:&nbsp;
                        <select onChange={this.handleSampleQueryChange}>
                            <option value="defer">Deferred query sample</option>
                            <option value="stream">Streamed query sample</option>
                            <option value="threshold">Threshold for complex queries sample</option>
                            <option value="live">Live query sample</option>
                        </select>
                    </p>
                </div>
                <div>
                    <textarea cols="100" rows="20" value={this.state.query} onChange={this.handleChange}></textarea>
                </div>
                <div>
                    <button onClick={this.handleRun}>Run query</button>
                </div>
                <div>
                    <p>Results are displayed here.</p>
                    <p>
                        As explained above, queries are run by GraphQL over WebSocket protocol. 
                        If the query is a subscription or defer query, the client will wait for subsequent responses from the Socket.
                    </p>
                </div>
                <div>
                    <textarea cols="100" rows="20" value={this.state.results}></textarea>
                </div>
            </div>
        );
    }
}