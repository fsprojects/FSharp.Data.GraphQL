namespace chat_app

open System

//
// Common model
//
type OrganizationId = OrganizationId of Guid
type MemberId = MemberId of Guid
type MemberPrivateId = MemberPrivateId of Guid
type ChatRoomId = ChatRoomId of Guid
type MessageId = MessageId of Guid

type MemberRoleInChat =
  | ChatAdmin
  | ChatGuest

type ChatRoomMessage =
  { Id : MessageId
    ChatRoomId : ChatRoomId
    Date : DateTime
    AuthorId : MemberId
    Text : string }

type ChatRoomSpecificEvent =
  | NewMessage of ChatRoomMessage
  | EditedMessage of ChatRoomMessage
  | DeletedMessage of MessageId
  | MemberJoined of MemberId * string
  | MemberLeft of MemberId * string

type ChatRoomEvent =
  { ChatRoomId : ChatRoomId
    Time : DateTime
    SpecificData : ChatRoomSpecificEvent }

//
// Persistence model
//
type Member_In_Db =
  { PrivId : MemberPrivateId
    Id : MemberId
    Name : string }

type ChatMember_In_Db =
  { ChatRoomId : ChatRoomId
    MemberId : MemberId
    Role : MemberRoleInChat }

type ChatRoom_In_Db =
  { Id : ChatRoomId
    Name : string
    Members : MemberId list }

type Organization_In_Db =
  { Id : OrganizationId
    Name : string
    Members : MemberId list 
    ChatRooms : ChatRoomId list }

//
// GraphQL models
//
type Member =
  { Id : MemberId
    Name : string }

type MeAsAMember =
  { PrivId : MemberPrivateId
    Id : MemberId
    Name : string }

type ChatMember =
  { Id : MemberId
    Name : string 
    Role : MemberRoleInChat }

type MeAsAChatMember =
  { PrivId : MemberPrivateId
    Id : MemberId
    Name : string
    Role : MemberRoleInChat }

type ChatRoom =
  { Id : ChatRoomId
    Name : string
    Members: ChatMember list }

type ChatRoomForMember =
  { Id : ChatRoomId
    Name : string
    MeAsAChatMember : MeAsAChatMember
    OtherChatMembers: ChatMember list }

type Organization =
  { Id : OrganizationId
    Name : string
    Members : Member list
    ChatRooms : ChatRoom list }

type OrganizationForMember =
  { Id : OrganizationId
    Name : string
    MeAsAMember : MeAsAMember
    OtherMembers : Member list
    ChatRooms : ChatRoom list }
