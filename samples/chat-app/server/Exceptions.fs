module FSharp.Data.GraphQL.Samples.ChatApp.Exceptions

open FSharp.Data.GraphQL

let Member_With_This_Name_Already_Exists (theName : string) : GraphQLException =
  GQLMessageException(sprintf "member with name \"%s\" already exists" theName)

let Organization_Doesnt_Exist (theId : OrganizationId) : GraphQLException =
  match theId with
  | OrganizationId x ->
    GQLMessageException (sprintf "organization with ID \"%s\" doesn't exist" (x.ToString()))

let ChatRoom_Doesnt_Exist (theId : ChatRoomId) : GraphQLException =
  GQLMessageException(sprintf "chat room with ID \"%s\" doesn't exist" (theId.ToString()))

let PrivMember_Doesnt_Exist (theId : MemberPrivateId) : GraphQLException =
  match theId with
  | MemberPrivateId x ->
    GQLMessageException(sprintf "member with private ID \"%s\" doesn't exist" (x.ToString()))

let Member_Isnt_Part_Of_Org () : GraphQLException =
  GQLMessageException("this member is not part of this organization")

let ChatRoom_Isnt_Part_Of_Org () : GraphQLException =
  GQLMessageException("this chat room is not part of this organization")