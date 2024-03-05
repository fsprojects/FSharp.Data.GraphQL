module FSharp.Data.GraphQL.Samples.ChatApp.Exceptions

open FSharp.Data.GraphQL

let Member_With_This_Name_Already_Exists (theName : string) : GraphQLException =
  GQLMessageException $"member with name \"%s{theName}\" already exists"

let Organization_Doesnt_Exist (theId : OrganizationId) : GraphQLException =
  match theId with
  | OrganizationId x ->
    GQLMessageException $"organization with ID \"%s{x.ToString()}\" doesn't exist"

let ChatRoom_Doesnt_Exist (theId : ChatRoomId) : GraphQLException =
  GQLMessageException $"chat room with ID \"%s{theId.ToString()}\" doesn't exist"

let PrivMember_Doesnt_Exist (theId : MemberPrivateId) : GraphQLException =
  match theId with
  | MemberPrivateId x ->
    GQLMessageException $"member with private ID \"%s{x.ToString()}\" doesn't exist"

let Member_Isnt_Part_Of_Org () : GraphQLException =
  GQLMessageException "this member is not part of this organization"

let ChatRoom_Isnt_Part_Of_Org () : GraphQLException =
  GQLMessageException "this chat room is not part of this organization"