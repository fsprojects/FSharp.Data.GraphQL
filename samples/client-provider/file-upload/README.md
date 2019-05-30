# TypeProvider file upload sample

In this folder you can run a sample which allows the client type provider to upload files to a GraphQL server using a [Multipart Request Spec](https://github.com/jaydenseric/graphql-multipart-request-spec) used by common server implementations, such as Apollo.

To run the sample, first go to the (server folder)[server]. Be sure that Node.js and npm are installed in your machine, and run `npm install` to install packages. After that, run `npm run build && npm start` to start the Apollo server. Then, run one of the sample fsx files:

1. [uploaded_files.fsx](uploaded_files.fsx): This script allows you to run a query to see the uploaded files on the server.
2. [single_file_upload.fsx](single_file_upload.fsx): This script shows an example of uploading a single file to the server using a mutation operation.
3. [multiple_files_upload.fsx](multiple_files_upload.fsx): This script shows an example of uploading multiple files to the server using a mutation operation.