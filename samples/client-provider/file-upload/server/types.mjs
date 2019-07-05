export default /* GraphQL */ `
  type File {
    id: ID!
    path: String!
    filename: String!
    mimetype: String!
  }

  type Query {
    uploads: [File]
  }

  input SingleUploadRequest {
    file: Upload!
  }

  input MultipleUploadRequest {
    files: [Upload!]!
  }

  type Mutation {
    singleUpload(request: SingleUploadRequest!): File!
    multipleUpload(request: MultipleUploadRequest!): [File!]!
  }
`
