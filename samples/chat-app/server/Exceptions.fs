namespace FSharp.Data.GraphQL.Samples.ChatApp

module Exceptions =
  open FSharp.Data.GraphQL

  let Member_With_This_Name_Already_Exists (theName : string) =
    GraphQLException(sprintf "member with name \"%s\" already exists" theName)

  let Organization_Doesnt_Exist (theId : OrganizationId) =
    match theId with
    | OrganizationId x ->
      GraphQLException (sprintf "organization with ID \"%s\" doesn't exist" (x.ToString()))

  let ChatRoom_Doesnt_Exist (theId : ChatRoomId) =
    GraphQLException(sprintf "chat room with ID \"%s\" doesn't exist" (theId.ToString()))

  let PrivMember_Doesnt_Exist (theId : MemberPrivateId) =
    match theId with
    | MemberPrivateId x ->
      GraphQLException(sprintf "member with private ID \"%s\" doesn't exist" (x.ToString()))

  let Member_Isnt_Part_Of_Org () =
    GraphQLException("this member is not part of this organization")

  let ChatRoom_Isnt_Part_Of_Org () =
    GraphQLException("this chat room is not part of this organization")