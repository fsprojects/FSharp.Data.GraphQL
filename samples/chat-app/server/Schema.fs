namespace chat_app

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open System

type Root =
  { RequestId : string }

type FakePersistence() =
  static let mutable _members = Map.empty<MemberId, Member_In_Db>
  static let mutable _chatMembers = Map.empty<ChatRoomId * MemberId, ChatMember_In_Db>
  static let mutable _chatRoomMessages = Map.empty<ChatRoomId * MessageId, ChatRoomMessage>
  static let mutable _chatRooms = Map.empty<ChatRoomId, ChatRoom_In_Db>
  static let mutable _organizations =
    let newId = OrganizationId (Guid.Parse("51f823ef-2294-41dc-9f39-a4b9a237317a"))
    ( newId,
      { Organization_In_Db.Id = newId
        Name = "Public"
        Members = []
        ChatRooms = [] }
    )
    |> List.singleton
    |> Map.ofList

  static member Members
    with get() = _members
    and set(v) = _members <- v

  static member ChatMembers
    with get() = _chatMembers
    and set(v) = _chatMembers <- v

  static member ChatRoomMessages
    with get() = _chatRoomMessages
    and set(v) = _chatRoomMessages <- v

  static member ChatRooms
    with get() = _chatRooms
    and set(v) = _chatRooms <- v

  static member Organizations
    with get() = _organizations
    and set(v) = _organizations <- v

module MapFrom =
  let memberInDb_To_Member (x : Member_In_Db) : Member =
    { Id = x.Id
      Name = x.Name }

  let memberInDb_To_MeAsAMember (x : Member_In_Db) : MeAsAMember =
    { PrivId = x.PrivId
      Id = x.Id
      Name = x.Name }

  let chatMemberInDb_To_ChatMember (membersToGetDetailsFrom : Member_In_Db seq) (x : ChatMember_In_Db) : ChatMember =
    let memberDetails = membersToGetDetailsFrom |> Seq.find (fun m -> m.Id = x.MemberId)
    { Id = x.MemberId
      Name = memberDetails.Name
      Role = x.Role }

  let chatMemberInDb_To_MeAsAChatMember (membersToGetDetailsFrom : Member_In_Db seq) (x : ChatMember_In_Db) : MeAsAChatMember =
    let memberDetails = membersToGetDetailsFrom |> Seq.find (fun m -> m.Id = x.MemberId)
    { PrivId = memberDetails.PrivId
      Id = x.MemberId
      Name = memberDetails.Name 
      Role = x.Role }

  let chatRoomInDb_To_ChatRoom (membersToGetDetailsFrom : Member_In_Db seq) (x : ChatRoom_In_Db) : ChatRoom =
    { Id = x.Id
      Name = x.Name
      Members =
        FakePersistence.ChatMembers.Values
        |> Seq.filter (fun m -> x.Members |> List.contains m.MemberId)
        |> Seq.map (chatMemberInDb_To_ChatMember membersToGetDetailsFrom)
        |> List.ofSeq }

  let chatRoomInDb_To_ChatRoomForMember (membersToGetDetailsFrom : Member_In_Db seq) (chatMember : ChatMember_In_Db) (x : ChatRoom_In_Db) : ChatRoomForMember =
    { Id = x.Id
      Name = x.Name
      MeAsAChatMember = chatMember |> chatMemberInDb_To_MeAsAChatMember membersToGetDetailsFrom
      OtherChatMembers =
        FakePersistence.ChatMembers.Values
        |> Seq.filter (fun m -> m.MemberId <> chatMember.MemberId && x.Members |> List.contains m.MemberId)
        |> Seq.map (chatMemberInDb_To_ChatMember membersToGetDetailsFrom)
        |> List.ofSeq }

  let organizationInDb_To_Organization (x : Organization_In_Db) : Organization =
    let members =
      FakePersistence.Members.Values
        |> Seq.filter (fun m -> x.Members |> List.contains m.Id)
    { Id = x.Id
      Name = x.Name
      Members = members |> Seq.map memberInDb_To_Member |> List.ofSeq
      ChatRooms =
        FakePersistence.ChatRooms.Values
        |> Seq.filter (fun c -> x.ChatRooms |> List.contains c.Id)
        |> Seq.map (chatRoomInDb_To_ChatRoom members)
        |> List.ofSeq }

  let organizationInDb_To_OrganizationForMember (memberId : MemberId) (x : Organization_In_Db) : OrganizationForMember option =
    let mapToOrganizationForMemberForMember (memberInDb : Member_In_Db) =
      let organizationStats = x |> organizationInDb_To_Organization
      { OrganizationForMember.Id =  x.Id
        Name = x.Name
        MeAsAMember = memberInDb |> memberInDb_To_MeAsAMember
        OtherMembers = organizationStats.Members |> List.filter(fun m -> m.Id <> memberInDb.Id)
        ChatRooms = organizationStats.ChatRooms }
    FakePersistence.Members.Values
    |> Seq.tryFind (fun m -> m.Id = memberId)
    |> Option.map mapToOrganizationForMemberForMember


module Schema =
  open FSharp.Data.GraphQL.Server.AppInfrastructure.Rop

  let validationException_Member_With_This_Name_Already_Exists (theName : string) =
    GraphQLException(sprintf "member with name \"%s\" already exists" theName)

  let validationException_Organization_Doesnt_Exist (theId : OrganizationId) =
    match theId with
    | OrganizationId x ->
      GraphQLException (sprintf "organization with ID \"%s\" doesn't exist" (x.ToString()))

  let validationException_ChatRoom_Doesnt_Exist (theId : ChatRoomId) =
    GraphQLException(sprintf "chat room with ID \"%s\" doesn't exist" (theId.ToString()))

  let validationException_PrivMember_Doesnt_Exist (theId : MemberPrivateId) =
    match theId with
    | MemberPrivateId x ->
      GraphQLException(sprintf "member with private ID \"%s\" doesn't exist" (x.ToString()))

  let validationException_Member_Isnt_Part_Of_Org () =
    GraphQLException("this member is not part of this organization")

  let validationException_ChatRoom_Isnt_Part_Of_Org () =
    GraphQLException("this chat room is not part of this organization")

  let authenticateMemberInOrganization (organizationId : OrganizationId) (memberPrivId : MemberPrivateId) : RopResult<(Organization_In_Db * Member_In_Db), GraphQLException> =
    let maybeOrganization = FakePersistence.Organizations |> Map.tryFind organizationId
    let maybeMember = FakePersistence.Members.Values |> Seq.tryFind (fun x -> x.PrivId = memberPrivId)

    match (maybeOrganization, maybeMember) with
    | None, _ ->
      fail <| (organizationId |> validationException_Organization_Doesnt_Exist)
    | _, None ->
      fail <| (memberPrivId |> validationException_PrivMember_Doesnt_Exist)
    | Some organization, Some theMember ->
      if not (organization.Members |> List.contains theMember.Id) then
        fail <| (validationException_Member_Isnt_Part_Of_Org())
      else
        succeed (organization, theMember)

  let validateChatRoomExistence (organization : Organization_In_Db) (chatRoomId : ChatRoomId) : RopResult<ChatRoom_In_Db, GraphQLException> =
    match FakePersistence.ChatRooms |> Map.tryFind chatRoomId with
    | None ->
      fail <| validationException_ChatRoom_Doesnt_Exist chatRoomId
    | Some chatRoom ->
      if not (organization.ChatRooms |> List.contains chatRoom.Id) then
        fail <| validationException_ChatRoom_Isnt_Part_Of_Org()
      else
        succeed <| chatRoom

  let validateMessageExistence (chatRoom : ChatRoom_In_Db) (messageId : MessageId) : RopResult<ChatRoomMessage, GraphQLException> =
    match FakePersistence.ChatRoomMessages |> Map.tryFind (chatRoom.Id, messageId) with
    | None ->
      fail (GraphQLException("chat message doesn't exist (anymore)"))
    | Some chatMessage ->
      succeed chatMessage

  let succeedOrRaiseGraphQLEx<'T> (ropResult : RopResult<'T, GraphQLException>) : 'T =
    match ropResult with
    | Failure exs ->
      let firstEx = exs |> List.head
      raise firstEx
    | Success (s, _) ->
      s

  let chatRoomEvents_subscription_name = "chatRoomEvents"

  let memberRoleInChatEnumDef =
    Define.Enum<MemberRoleInChat>(
      name = nameof MemberRoleInChat,
      options = [
        Define.EnumValue(ChatAdmin.ToString(), ChatAdmin)
        Define.EnumValue(ChatGuest.ToString(), ChatGuest)
      ]
    )

  let memberDef =
    Define.Object<Member>(
      name = nameof Member,
      description = "An organization member",
      isTypeOf = (fun o -> o :? Member),
      fieldsFn = fun () -> [
        Define.Field("id", SchemaDefinitions.Guid, "the member's ID", fun _ (x : Member) -> match x.Id with MemberId theId -> theId)
        Define.Field("name", SchemaDefinitions.String, "the member's name", fun _ (x : Member) -> x.Name)
      ]
    )

  let meAsAMemberDef =
    Define.Object<MeAsAMember>(
      name = nameof MeAsAMember,
      description = "An organization member",
      isTypeOf = (fun o -> o :? MeAsAMember),
      fieldsFn = fun () -> [
        Define.Field("privId", SchemaDefinitions.Guid, "the member's private ID used for authenticating their requests", fun _ (x : MeAsAMember) -> match x.PrivId with MemberPrivateId theId -> theId)
        Define.Field("id", SchemaDefinitions.Guid, "the member's ID", fun _ (x : MeAsAMember) -> match x.Id with MemberId theId -> theId)
        Define.Field("name", SchemaDefinitions.String, "the member's name", fun _ (x : MeAsAMember) -> x.Name)
      ]
    )

  let chatMemberDef =
    Define.Object<ChatMember>(
      name = nameof ChatMember,
      description = "A chat member is an organization member participating in a chat room",
      isTypeOf = (fun o -> o :? ChatMember),
      fieldsFn = fun () -> [
        Define.Field("id", SchemaDefinitions.Guid, "the member's ID", fun _ (x : ChatMember) -> match x.Id with MemberId theId -> theId)
        Define.Field("name", SchemaDefinitions.String, "the member's name", fun _ (x : ChatMember) -> x.Name)
        Define.Field("role", memberRoleInChatEnumDef, "the member's role in the chat", fun _ (x : ChatMember) -> x.Role)
      ]
    )

  let meAsAChatMemberDef =
    Define.Object<MeAsAChatMember>(
      name = nameof MeAsAChatMember,
      description = "A chat member is an organization member participating in a chat room",
      isTypeOf = (fun o -> o :? MeAsAChatMember),
      fieldsFn = fun () -> [
        Define.Field("privId", SchemaDefinitions.Guid, "the member's private ID used for authenticating their requests", fun _ (x : MeAsAChatMember) -> match x.PrivId with MemberPrivateId theId -> theId)
        Define.Field("id", SchemaDefinitions.Guid, "the member's ID", fun _ (x : MeAsAChatMember) -> match x.Id with MemberId theId -> theId)
        Define.Field("name", SchemaDefinitions.String, "the member's name", fun _ (x : MeAsAChatMember) -> x.Name)
        Define.Field("role", memberRoleInChatEnumDef, "the member's role in the chat", fun _ (x : MeAsAChatMember) -> x.Role)
      ]
    )

  let chatRoomStatsDef =
    Define.Object<ChatRoom>(
      name = nameof ChatRoom,
      description = "A chat room as viewed from the outside",
      isTypeOf = (fun o -> o :? ChatRoom),
      fieldsFn = fun () -> [
        Define.Field("id", SchemaDefinitions.Guid, "the chat room's ID", fun _ (x : ChatRoom) -> match x.Id with ChatRoomId theId -> theId)
        Define.Field("name", SchemaDefinitions.String, "the chat room's name", fun _ (x : ChatRoom) -> x.Name)
        Define.Field("members", ListOf chatMemberDef, "the members in the chat room", fun _ (x : ChatRoom) -> x.Members)
      ]
    )
  
  let chatRoomDetailsDef =
    Define.Object<ChatRoomForMember>(
      name = nameof ChatRoomForMember,
      description = "A chat room as viewed by a chat room member",
      isTypeOf = (fun o -> o :? ChatRoomForMember),
      fieldsFn = fun () -> [
        Define.Field("id", SchemaDefinitions.Guid, "the chat room's ID", fun _ (x : ChatRoomForMember) -> match x.Id with ChatRoomId theId -> theId)
        Define.Field("name", SchemaDefinitions.String, "the chat room's name", fun _ (x : ChatRoomForMember) -> x.Name)
        Define.Field("meAsAChatMember", meAsAChatMemberDef, "the chat member that queried the details", fun _ (x : ChatRoomForMember) -> x.MeAsAChatMember)
        Define.Field("otherChatMembers", ListOf chatMemberDef, "the chat members excluding the one who queried the details", fun _ (x : ChatRoomForMember) -> x.OtherChatMembers)
      ]
    )

  let organizationStatsDef =
    Define.Object<Organization>(
      name = nameof Organization,
      description = "An organization as seen from the outside",
      isTypeOf = (fun o -> o :? Organization),
      fieldsFn = fun () -> [
        Define.Field("id", SchemaDefinitions.Guid, "the organization's ID", fun _ (x : Organization) -> match x.Id with OrganizationId theId -> theId)
        Define.Field("name", SchemaDefinitions.String, "the organization's name", fun _ (x : Organization) -> x.Name)
        Define.Field("members", ListOf memberDef, "members of this organization", fun _ (x : Organization) -> x.Members)
        Define.Field("chatRooms", ListOf chatRoomStatsDef, "chat rooms in this organization", fun _ (x : Organization) -> x.ChatRooms)
      ]
    )

  let organizationDetailsDef =
    Define.Object<OrganizationForMember>(
      name = nameof OrganizationForMember,
      description = "An organization as seen by one of the organization's members",
      isTypeOf = (fun o -> o :? OrganizationForMember),
      fieldsFn = fun () -> [
        Define.Field("id", SchemaDefinitions.Guid, "the organization's ID", fun _ (x : OrganizationForMember) -> match x.Id with OrganizationId theId -> theId)
        Define.Field("name", SchemaDefinitions.String, "the organization's name", fun _ (x : OrganizationForMember) -> x.Name)
        Define.Field("meAsAMember", meAsAMemberDef, "the member that queried the details", fun _ (x : OrganizationForMember) -> x.MeAsAMember)
        Define.Field("otherMembers", ListOf memberDef, "members of this organization", fun _ (x : OrganizationForMember) -> x.OtherMembers)
        Define.Field("chatRooms", ListOf chatRoomStatsDef, "chat rooms in this organization", fun _ (x : OrganizationForMember) -> x.ChatRooms)
      ]
    )

  let aChatRoomMessageDef description name =
    Define.Object<ChatRoomMessage>(
      name = name,
      description = description,
      isTypeOf = (fun o -> o :? ChatRoomMessage),
      fieldsFn = fun () -> [
        Define.Field("id", SchemaDefinitions.Guid, "the message's ID", fun _ (x : ChatRoomMessage) -> match x.Id with MessageId theId -> theId)
        Define.Field("chatRoomId", SchemaDefinitions.Guid, "the ID of the chat room the message belongs to", fun _ (x : ChatRoomMessage) -> match x.ChatRoomId with ChatRoomId theId -> theId)
        Define.Field("date", SchemaDefinitions.Date, "the time the message was received at the server", fun _ (x : ChatRoomMessage) -> x.Date)
        Define.Field("authorId", SchemaDefinitions.Guid, "the member ID of the message's author", fun _ (x : ChatRoomMessage) -> match x.AuthorId with MemberId theId -> theId)
        Define.Field("text", SchemaDefinitions.String, "the message's text", fun _ (x : ChatRoomMessage) -> x.Text)
      ]
    )

  let anEmptyChatRoomEvent description name =
    Define.Object<unit>(
      name = name,
      description = description,
      isTypeOf = (fun o -> o :? unit),
      fieldsFn = fun () -> [
        Define.Field("doNotUse", SchemaDefinitions.Boolean, "this is just to satify the expected structure of this type", fun _ _ -> true)
      ]
    )

  let aChatRoomEventForMessageId description name =
    Define.Object<MessageId>(
      name = name,
      description = description,
      isTypeOf = (fun o -> o :? MessageId),
      fieldsFn = (fun () -> [
        Define.Field("messageId", SchemaDefinitions.Guid, "this is the message ID", fun _ (x : MessageId) -> match x with MessageId theId -> theId)
      ])
    )

  let aChatRoomEventForMemberIdAndName description name =
    Define.Object<MemberId * string>(
      name = name,
      description = description,
      isTypeOf = (fun o -> o :? (MemberId * string)),
      fieldsFn = (fun () -> [
        Define.Field("memberId", SchemaDefinitions.Guid, "this is the member's ID", fun _ (mId : MemberId, _ : string) -> match mId with MemberId theId -> theId)
        Define.Field("memberName", SchemaDefinitions.String, "this is the member's name", fun _ (_ : MemberId, name : string) -> name)
      ])
    )

  let newMessageDef = nameof NewMessage |> aChatRoomMessageDef "a new public message has been sent in the chat room"
  let editedMessageDef = nameof EditedMessage |> aChatRoomMessageDef "a public message of the chat room has been edited"
  let deletedMessageDef = nameof DeletedMessage |> aChatRoomEventForMessageId "a public message of the chat room has been deleted"
  let memberJoinedDef = nameof MemberJoined |> aChatRoomEventForMemberIdAndName "a member has joined the chat"
  let memberLeftDef = nameof MemberLeft |> aChatRoomEventForMemberIdAndName "a member has left the chat"

  let chatRoomSpecificEventDef =
    Define.Union(
      name = nameof ChatRoomSpecificEvent,
      options =
        [ newMessageDef
          editedMessageDef
          deletedMessageDef
          memberJoinedDef
          memberLeftDef ],
      resolveValue =
        (fun o ->
          match o with
          | NewMessage x -> box x
          | EditedMessage x -> upcast x
          | DeletedMessage x -> upcast x
          | MemberJoined (mId, mName) -> upcast (mId, mName)
          | MemberLeft (mId, mName) -> upcast (mId, mName)
        ),
      resolveType =
        (fun o ->
          match o with
          | NewMessage _ -> newMessageDef
          | EditedMessage _ -> editedMessageDef
          | DeletedMessage _ -> deletedMessageDef
          | MemberJoined _ -> memberJoinedDef
          | MemberLeft _ -> memberLeftDef
        ),
      description = "data which is specific to a certain type of event"
    )

  let chatRoomEventDef =
    Define.Object<ChatRoomEvent>(
      name = nameof ChatRoomEvent,
      description = "Something that happened in the chat room, like a new message sent",
      isTypeOf = (fun o -> o :? ChatRoomEvent),
      fieldsFn = (fun () -> [
        Define.Field("chatRoomId", SchemaDefinitions.Guid, "the ID of the chat room in which the event happened", fun _ (x : ChatRoomEvent) -> match x.ChatRoomId with ChatRoomId theId -> theId)
        Define.Field("time", SchemaDefinitions.Date, "the time the message was received at the server", fun _ (x : ChatRoomEvent) -> x.Time)
        Define.Field("specificData", chatRoomSpecificEventDef, "the event's specific data", fun _ (x : ChatRoomEvent) -> x.SpecificData)
      ])
    )

  let query =
    Define.Object<Root>(
      name = "Query",
      fields = [
        Define.Field(
          "organizations",
          ListOf organizationStatsDef,
          "gets all available organizations",
          fun _ _ ->
            FakePersistence.Organizations.Values
            |> Seq.map MapFrom.organizationInDb_To_Organization
            |> List.ofSeq
        )
      ]
    )

  let schemaConfig = SchemaConfig.Default

  let publishChatRoomEvent (specificEvent : ChatRoomSpecificEvent) (chatRoomId : ChatRoomId) : unit =
    { ChatRoomId = chatRoomId
      Time = DateTime.UtcNow
      SpecificData = specificEvent }
    |> schemaConfig.SubscriptionProvider.Publish<ChatRoomEvent> chatRoomEvents_subscription_name

  let mutation =
    Define.Object<Root>(
      name = "Mutation",
      fields = [
        Define.Field(
          "enterOrganization",
          organizationDetailsDef,
          "makes a new member enter an organization",
          [ Define.Input ("organizationId", SchemaDefinitions.Guid, description = "the ID of the organization")
            Define.Input ("member", SchemaDefinitions.String, description = "the new member's name")
          ],
          fun ctx root ->
            let organizationId = OrganizationId (ctx.Arg("organizationId"))
            let newMemberName : string = ctx.Arg("member")
            let maybeResult =
              FakePersistence.Organizations
              |> Map.tryFind organizationId
              |> Option.map MapFrom.organizationInDb_To_Organization
              |> Option.map
                  (fun organization ->
                    if organization.Members |> List.exists (fun m -> m.Name = newMemberName) then
                      raise (newMemberName |> validationException_Member_With_This_Name_Already_Exists)
                    else
                      let newMemberPrivId = MemberPrivateId (Guid.NewGuid())
                      let newMemberId = MemberId (Guid.NewGuid())
                      let newMember =
                        { Member_In_Db.PrivId = newMemberPrivId
                          Id = newMemberId
                          Name = newMemberName }
                      FakePersistence.Members <-
                        FakePersistence.Members
                        |> Map.add newMemberId newMember

                      FakePersistence.Organizations <-
                        FakePersistence.Organizations
                        |> Map.change organizationId
                            (Option.bind
                              (fun organization ->
                                Some { organization with Members = newMemberId :: organization.Members }
                              )
                            )
                      FakePersistence.Organizations
                      |> Map.find organizationId
                      |> MapFrom.organizationInDb_To_OrganizationForMember newMemberId
                  )
              |> Option.flatten
            match maybeResult with
            | None ->
              raise (GraphQLException("couldn't enter organization (maybe the ID is incorrect?)"))
            | Some res ->
              res
        )
        Define.Field(
          "createChatRoom",
          chatRoomDetailsDef,
          "creates a new chat room for a user",
          [ Define.Input ("organizationId", SchemaDefinitions.Guid, description = "the ID of the organization in which the chat room will be created")
            Define.Input ("memberId", SchemaDefinitions.Guid, description = "the member's private ID")
            Define.Input ("name", SchemaDefinitions.String, description = "the chat room's name")
          ],
          fun ctx root ->
            let organizationId = OrganizationId (ctx.Arg("organizationId"))
            let memberPrivId = MemberPrivateId (ctx.Arg("memberId"))
            let chatRoomName : string = ctx.Arg("name")

            memberPrivId
            |> authenticateMemberInOrganization organizationId
            |> mapR
                (fun (organization, theMember) ->
                  let newChatRoomId = ChatRoomId (Guid.NewGuid())
                  let newChatMember : ChatMember_In_Db =
                    { ChatRoomId = newChatRoomId
                      MemberId = theMember.Id
                      Role = ChatAdmin }
                  let newChatRoom : ChatRoom_In_Db =
                    { Id = newChatRoomId
                      Name = chatRoomName
                      Members = [ theMember.Id ] }
                  FakePersistence.ChatRooms <-
                    FakePersistence.ChatRooms |> Map.add newChatRoomId newChatRoom
                  FakePersistence.ChatMembers <-
                    FakePersistence.ChatMembers |> Map.add (newChatRoomId, theMember.Id) newChatMember
                  FakePersistence.Organizations <-
                    FakePersistence.Organizations
                    |> Map.change
                        organizationId
                        (Option.map(fun org -> { org with ChatRooms = newChatRoomId :: org.ChatRooms }))

                  MapFrom.chatRoomInDb_To_ChatRoomForMember
                    (FakePersistence.Members.Values |> Seq.filter (fun x -> organization.Members |> List.contains x.Id))
                    newChatMember
                    newChatRoom
                )
            |> succeedOrRaiseGraphQLEx
        )
        Define.Field(
          "enterChatRoom",
          chatRoomDetailsDef,
          "makes a member enter a chat room",
          [ Define.Input ("organizationId", SchemaDefinitions.Guid, description = "the ID of the organization the chat room and member are in")
            Define.Input ("chatRoomId", SchemaDefinitions.Guid, description = "the ID of the chat room")
            Define.Input ("memberId", SchemaDefinitions.Guid, description = "the member's private ID")
          ],
          fun ctx root ->
            let organizationId = OrganizationId (ctx.Arg("organizationId"))
            let chatRoomId = ChatRoomId (ctx.Arg("chatRoomId"))
            let memberPrivId = MemberPrivateId (ctx.Arg("memberId"))

            memberPrivId
            |> authenticateMemberInOrganization organizationId
            |> bindR
                (fun (organization, theMember) ->
                  chatRoomId
                  |> validateChatRoomExistence organization
                  |> mapR (fun chatRoom -> (organization, chatRoom, theMember)) 
                )
            |> mapR
                (fun (_, chatRoom, theMember) ->
                  let newChatMember : ChatMember_In_Db =
                    { ChatRoomId = chatRoom.Id
                      MemberId = theMember.Id
                      Role = ChatGuest
                    }
                  FakePersistence.ChatMembers <-
                    FakePersistence.ChatMembers
                    |> Map.add
                        (newChatMember.ChatRoomId, newChatMember.MemberId)
                        newChatMember
                  FakePersistence.ChatRooms <-
                    FakePersistence.ChatRooms
                    |> Map.change
                        chatRoom.Id
                        (Option.map (fun theChatRoom -> { theChatRoom with Members = newChatMember.MemberId :: theChatRoom.Members}))
                  let theChatRoom = FakePersistence.ChatRooms |> Map.find chatRoomId
                  let result =
                    MapFrom.chatRoomInDb_To_ChatRoomForMember
                      (FakePersistence.Members.Values
                      |> Seq.filter
                          (fun x -> theChatRoom.Members |> List.contains x.Id)
                      )
                      newChatMember
                      theChatRoom

                  chatRoom.Id
                  |> publishChatRoomEvent (MemberJoined (theMember.Id, theMember.Name))

                  result
                )
            |> succeedOrRaiseGraphQLEx
        )
        Define.Field(
          "leaveChatRoom",
          SchemaDefinitions.Boolean,
          "makes a member leave a chat room",
          [ Define.Input ("organizationId", SchemaDefinitions.Guid, description = "the ID of the organization the chat room and member are in")
            Define.Input ("chatRoomId", SchemaDefinitions.Guid, description = "the ID of the chat room")
            Define.Input ("memberId", SchemaDefinitions.Guid, description = "the member's private ID")
          ],
          fun ctx root ->
            let organizationId = OrganizationId (ctx.Arg("organizationId"))
            let chatRoomId = ChatRoomId (ctx.Arg("chatRoomId"))
            let memberPrivId = MemberPrivateId (ctx.Arg("memberId"))

            memberPrivId
            |> authenticateMemberInOrganization organizationId
            |> bindR
                (fun (organization, theMember) ->
                  chatRoomId
                  |> validateChatRoomExistence organization
                  |> mapR (fun chatRoom -> (organization, chatRoom, theMember)) 
                )
            |> mapR
                (fun (_, chatRoom, theMember) ->
                  FakePersistence.ChatMembers <-
                    FakePersistence.ChatMembers |> Map.remove (chatRoom.Id, theMember.Id)
                  FakePersistence.ChatRooms <-
                    FakePersistence.ChatRooms
                    |> Map.change
                        chatRoom.Id
                        (Option.map (fun theChatRoom -> { theChatRoom with Members = theChatRoom.Members |> List.filter (fun mId -> mId <> theMember.Id)}))
                  true
                )
            |> succeedOrRaiseGraphQLEx
        )
        Define.Field(
          "sendChatMessage",
          SchemaDefinitions.Boolean,
          [ Define.Input("organizationId", SchemaDefinitions.Guid, description = "the ID of the organization the chat room and member are in")
            Define.Input("chatRoomId", SchemaDefinitions.Guid, description = "the chat room's ID")
            Define.Input("memberId", SchemaDefinitions.Guid, description = "the member's private ID")
            Define.Input("text", SchemaDefinitions.String, description = "the chat message's contents")],
          fun ctx _ ->
            let organizationId = OrganizationId (ctx.Arg("organizationId"))
            let chatRoomId = ChatRoomId (ctx.Arg("chatRoomId"))
            let memberPrivId = MemberPrivateId (ctx.Arg("memberId"))
            let text : string =  ctx.Arg("text")

            memberPrivId
            |> authenticateMemberInOrganization organizationId
            |> bindR
                (fun (organization, theMember) ->
                  chatRoomId
                  |> validateChatRoomExistence organization
                  |> mapR (fun chatRoom -> (organization, chatRoom, theMember))
                )
            |> mapR
                (fun (_, chatRoom, theMember) ->
                  let newChatRoomMessage =
                    { Id = MessageId (Guid.NewGuid())
                      ChatRoomId = chatRoom.Id
                      Date = DateTime.UtcNow
                      AuthorId = theMember.Id
                      Text = text }
                  FakePersistence.ChatRoomMessages <-
                    FakePersistence.ChatRoomMessages
                    |> Map.add
                        (chatRoom.Id, newChatRoomMessage.Id)
                        newChatRoomMessage

                  chatRoom.Id
                  |> publishChatRoomEvent (NewMessage newChatRoomMessage)

                  true
                )
            |> succeedOrRaiseGraphQLEx
        )
        Define.Field(
          "editChatMessage",
          SchemaDefinitions.Boolean,
          [ Define.Input("organizationId", SchemaDefinitions.Guid, description = "the ID of the organization the chat room and member are in")
            Define.Input("chatRoomId", SchemaDefinitions.Guid, description = "the chat room's ID")
            Define.Input("memberId", SchemaDefinitions.Guid, description = "the member's private ID")
            Define.Input("messageId", SchemaDefinitions.Guid, description = "the existing message's ID")
            Define.Input("text", SchemaDefinitions.String, description = "the chat message's contents")],
          fun ctx _ ->
            let organizationId = OrganizationId (ctx.Arg("organizationId"))
            let chatRoomId = ChatRoomId (ctx.Arg("chatRoomId"))
            let memberPrivId = MemberPrivateId (ctx.Arg("memberId"))
            let messageId = MessageId (ctx.Arg("messageId"))
            let text : string =  ctx.Arg("text")

            memberPrivId
            |> authenticateMemberInOrganization organizationId
            |> bindR
                (fun (organization, theMember) ->
                  chatRoomId
                  |> validateChatRoomExistence organization
                  |> bindR (fun chatRoom -> messageId |> validateMessageExistence chatRoom |> mapR (fun x -> (chatRoom, x)))
                  |> mapR (fun (chatRoom, chatMessage) -> (organization, chatRoom, theMember, chatMessage))
                )
            |> mapR
                (fun (_, chatRoom, theMember, chatMessage) ->
                  let newChatRoomMessage =
                    { Id = chatMessage.Id
                      ChatRoomId = chatRoom.Id
                      Date = chatMessage.Date
                      AuthorId = theMember.Id
                      Text = text }
                  FakePersistence.ChatRoomMessages <-
                    FakePersistence.ChatRoomMessages
                    |> Map.change
                        (chatRoom.Id, newChatRoomMessage.Id)
                        (Option.map (fun _ -> newChatRoomMessage))

                  chatRoom.Id
                  |> publishChatRoomEvent (EditedMessage newChatRoomMessage)

                  true
                )
            |> succeedOrRaiseGraphQLEx
        )
        Define.Field(
          "deleteChatMessage",
          SchemaDefinitions.Boolean,
          [ Define.Input("organizationId", SchemaDefinitions.Guid, description = "the ID of the organization the chat room and member are in")
            Define.Input("chatRoomId", SchemaDefinitions.Guid, description = "the chat room's ID")
            Define.Input("memberId", SchemaDefinitions.Guid, description = "the member's private ID")
            Define.Input("messageId", SchemaDefinitions.Guid, description = "the existing message's ID")],
          fun ctx _ ->
            let organizationId = OrganizationId (ctx.Arg("organizationId"))
            let chatRoomId = ChatRoomId (ctx.Arg("chatRoomId"))
            let memberPrivId = MemberPrivateId (ctx.Arg("memberId"))
            let messageId = MessageId (ctx.Arg("messageId"))

            memberPrivId
            |> authenticateMemberInOrganization organizationId
            |> bindR
                (fun (organization, theMember) ->
                  chatRoomId
                  |> validateChatRoomExistence organization
                  |> bindR (fun chatRoom -> messageId |> validateMessageExistence chatRoom |> mapR (fun x -> (chatRoom, x)))
                  |> mapR (fun (chatRoom, chatMessage) -> (organization, chatRoom, theMember, chatMessage))
                )
            |> mapR
                (fun (_, chatRoom, theMember, chatMessage) ->
                  FakePersistence.ChatRoomMessages <-
                    FakePersistence.ChatRoomMessages
                    |> Map.remove (chatRoom.Id, chatMessage.Id)

                  chatRoom.Id
                  |> publishChatRoomEvent (DeletedMessage chatMessage.Id)

                  true
                )
            |> succeedOrRaiseGraphQLEx
        )
      ]
    )

  let rootDef =
    Define.Object<Root>(
      name = "Root",
      description = "contains general request information",
      isTypeOf = (fun o -> o :? Root),
      fieldsFn = fun () ->
      [ Define.Field("requestId", SchemaDefinitions.String, "The request's unique ID.", fun _ (r : Root) -> r.RequestId) ]
    )

  let subscription =
    Define.SubscriptionObject<Root>(
      name = "Subscription",
      fields = [
        Define.SubscriptionField(
          chatRoomEvents_subscription_name,
          rootDef,
          chatRoomEventDef,
          "events related to a specific chat room",
          [ Define.Input("chatRoomId", SchemaDefinitions.Guid, description = "the ID of the chat room to listen to events from")
            Define.Input("memberId", SchemaDefinitions.Guid, description = "the member's private ID")],
          (fun ctx _ (chatRoomEvent : ChatRoomEvent) ->
            let chatRoomIdOfInterest = ChatRoomId (ctx.Arg("chatRoomId"))
            let memberId = MemberPrivateId (ctx.Arg("memberId"))

            if chatRoomEvent.ChatRoomId <> chatRoomIdOfInterest then
              None
            else
              let chatRoom = FakePersistence.ChatRooms |> Map.find chatRoomEvent.ChatRoomId
              let chatRoomMembersPrivIds =
                FakePersistence.Members.Values
                |> Seq.filter (fun m -> chatRoom.Members |> List.contains m.Id)
                |> Seq.map (fun m -> m.PrivId)
              if not (chatRoomMembersPrivIds |> Seq.contains memberId) then
                None
              else
                Some chatRoomEvent
          )
        )
      ]
    )

  let schema : ISchema<Root> = Schema(query, mutation, subscription, schemaConfig)

  let executor = Executor(schema, [])