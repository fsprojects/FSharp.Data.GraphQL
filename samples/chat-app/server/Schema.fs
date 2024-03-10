namespace FSharp.Data.GraphQL.Samples.ChatApp

open System
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

type Root = { RequestId : string }

module MapFrom =

    let memberInDb_To_Member (x : Member_In_Db) : Member = { Id = x.Id; Name = x.Name }

    let memberInDb_To_MeAsAMember (x : Member_In_Db) : MeAsAMember = { PrivId = x.PrivId; Id = x.Id; Name = x.Name }

    let chatMemberInDb_To_ChatMember (membersToGetDetailsFrom : Member_In_Db seq) (x : ChatMember_In_Db) : ChatMember =
        let memberDetails =
            membersToGetDetailsFrom
            |> Seq.find (fun m -> m.Id = x.MemberId)
        { Id = x.MemberId; Name = memberDetails.Name; Role = x.Role }

    let chatMemberInDb_To_MeAsAChatMember (membersToGetDetailsFrom : Member_In_Db seq) (x : ChatMember_In_Db) : MeAsAChatMember =
        let memberDetails =
            membersToGetDetailsFrom
            |> Seq.find (fun m -> m.Id = x.MemberId)
        {
            PrivId = memberDetails.PrivId
            Id = x.MemberId
            Name = memberDetails.Name
            Role = x.Role
        }

    let chatRoomInDb_To_ChatRoom (membersToGetDetailsFrom : Member_In_Db seq) (x : ChatRoom_In_Db) : ChatRoom = {
        Id = x.Id
        Name = x.Name
        Members =
            FakePersistence.ChatMembers.Values
            |> Seq.filter (fun m -> x.Members |> List.contains m.MemberId)
            |> Seq.map (chatMemberInDb_To_ChatMember membersToGetDetailsFrom)
            |> List.ofSeq
    }

    let chatRoomInDb_To_ChatRoomForMember
        (membersToGetDetailsFrom : Member_In_Db seq)
        (chatMember : ChatMember_In_Db)
        (x : ChatRoom_In_Db)
        : ChatRoomForMember =
        {
            Id = x.Id
            Name = x.Name
            MeAsAChatMember =
                chatMember
                |> chatMemberInDb_To_MeAsAChatMember membersToGetDetailsFrom
            OtherChatMembers =
                FakePersistence.ChatMembers.Values
                |> Seq.filter (fun m ->
                    m.MemberId <> chatMember.MemberId
                    && x.Members |> List.contains m.MemberId)
                |> Seq.map (chatMemberInDb_To_ChatMember membersToGetDetailsFrom)
                |> List.ofSeq
        }

    let organizationInDb_To_Organization (x : Organization_In_Db) : Organization =
        let members =
            FakePersistence.Members.Values
            |> Seq.filter (fun m -> x.Members |> List.contains m.Id)
        {
            Id = x.Id
            Name = x.Name
            Members = members |> Seq.map memberInDb_To_Member |> List.ofSeq
            ChatRooms =
                FakePersistence.ChatRooms.Values
                |> Seq.filter (fun c -> x.ChatRooms |> List.contains c.Id)
                |> Seq.map (chatRoomInDb_To_ChatRoom members)
                |> List.ofSeq
        }

    let organizationInDb_To_OrganizationForMember (memberId : MemberId) (x : Organization_In_Db) : OrganizationForMember option =
        let mapToOrganizationForMemberForMember (memberInDb : Member_In_Db) =
            let organizationStats = x |> organizationInDb_To_Organization
            {
                OrganizationForMember.Id = x.Id
                Name = x.Name
                MeAsAMember = memberInDb |> memberInDb_To_MeAsAMember
                OtherMembers =
                    organizationStats.Members
                    |> List.filter (fun m -> m.Id <> memberInDb.Id)
                ChatRooms = organizationStats.ChatRooms
            }
        FakePersistence.Members.Values
        |> Seq.tryFind (fun m -> m.Id = memberId)
        |> Option.map mapToOrganizationForMemberForMember


module Schema =

    let authenticateMemberInOrganization
        (organizationId : OrganizationId)
        (memberPrivId : MemberPrivateId)
        : Result<(Organization_In_Db * Member_In_Db), GraphQLException> =
        let maybeOrganization = FakePersistence.Organizations |> Map.tryFind organizationId
        let maybeMember =
            FakePersistence.Members.Values
            |> Seq.tryFind (fun x -> x.PrivId = memberPrivId)

        match (maybeOrganization, maybeMember) with
        | None, _ -> Error (organizationId |> Exceptions.Organization_Doesnt_Exist)
        | _, None -> Error (memberPrivId |> Exceptions.PrivMember_Doesnt_Exist)
        | Some organization, Some theMember ->
            if not (organization.Members |> List.contains theMember.Id) then
                Error (Exceptions.Member_Isnt_Part_Of_Org ())
            else
                Ok (organization, theMember)

    let validateChatRoomExistence (organization : Organization_In_Db) (chatRoomId : ChatRoomId) : Result<ChatRoom_In_Db, GraphQLException> =
        match FakePersistence.ChatRooms |> Map.tryFind chatRoomId with
        | None -> Error <| Exceptions.ChatRoom_Doesnt_Exist chatRoomId
        | Some chatRoom ->
            if not (organization.ChatRooms |> List.contains chatRoom.Id) then
                Error <| Exceptions.ChatRoom_Isnt_Part_Of_Org ()
            else
                Ok chatRoom

    let validateMessageExistence (chatRoom : ChatRoom_In_Db) (messageId : MessageId) : Result<ChatRoomMessage, GraphQLException> =
        match
            FakePersistence.ChatRoomMessages
            |> Map.tryFind (chatRoom.Id, messageId)
        with
        | None -> Error (GQLMessageException ("chat message doesn't exist (anymore)"))
        | Some chatMessage -> Ok chatMessage

    let succeedOrRaiseGraphQLEx<'T> (result : Result<'T, GraphQLException>) : 'T =
        match result with
        | Error ex -> raise ex
        | Ok s -> s

    let chatRoomEvents_subscription_name = "chatRoomEvents"

    let memberRoleInChatEnumDef =
        Define.Enum<MemberRoleInChat> (
            name = nameof MemberRoleInChat,
            options = [
                Define.EnumValue (ChatAdmin.ToString (), ChatAdmin)
                Define.EnumValue (ChatGuest.ToString (), ChatGuest)
            ]
        )

    let memberDef =
        Define.Object<Member> (
            name = nameof Member,
            description = "An organization member",
            isTypeOf = (fun o -> o :? Member),
            fieldsFn =
                fun () -> [
                    Define.Field (
                        "id",
                        GuidType,
                        "the member's ID",
                        fun _ (x : Member) ->
                            match x.Id with
                            | MemberId theId -> theId
                    )
                    Define.Field ("name", StringType, "the member's name", (fun _ (x : Member) -> x.Name))
                ]
        )

    let meAsAMemberDef =
        Define.Object<MeAsAMember> (
            name = nameof MeAsAMember,
            description = "An organization member",
            isTypeOf = (fun o -> o :? MeAsAMember),
            fieldsFn =
                fun () -> [
                    Define.Field (
                        "privId",
                        GuidType,
                        "the member's private ID used for authenticating their requests",
                        fun _ (x : MeAsAMember) ->
                            match x.PrivId with
                            | MemberPrivateId theId -> theId
                    )
                    Define.Field (
                        "id",
                        GuidType,
                        "the member's ID",
                        fun _ (x : MeAsAMember) ->
                            match x.Id with
                            | MemberId theId -> theId
                    )
                    Define.Field ("name", StringType, "the member's name", (fun _ (x : MeAsAMember) -> x.Name))
                ]
        )

    let chatMemberDef =
        Define.Object<ChatMember> (
            name = nameof ChatMember,
            description = "A chat member is an organization member participating in a chat room",
            isTypeOf = (fun o -> o :? ChatMember),
            fieldsFn =
                fun () -> [
                    Define.Field (
                        "id",
                        GuidType,
                        "the member's ID",
                        fun _ (x : ChatMember) ->
                            match x.Id with
                            | MemberId theId -> theId
                    )
                    Define.Field ("name", StringType, "the member's name", (fun _ (x : ChatMember) -> x.Name))
                    Define.Field ("role", memberRoleInChatEnumDef, "the member's role in the chat", (fun _ (x : ChatMember) -> x.Role))
                ]
        )

    let meAsAChatMemberDef =
        Define.Object<MeAsAChatMember> (
            name = nameof MeAsAChatMember,
            description = "A chat member is an organization member participating in a chat room",
            isTypeOf = (fun o -> o :? MeAsAChatMember),
            fieldsFn =
                fun () -> [
                    Define.Field (
                        "privId",
                        GuidType,
                        "the member's private ID used for authenticating their requests",
                        fun _ (x : MeAsAChatMember) ->
                            match x.PrivId with
                            | MemberPrivateId theId -> theId
                    )
                    Define.Field (
                        "id",
                        GuidType,
                        "the member's ID",
                        fun _ (x : MeAsAChatMember) ->
                            match x.Id with
                            | MemberId theId -> theId
                    )
                    Define.Field ("name", StringType, "the member's name", (fun _ (x : MeAsAChatMember) -> x.Name))
                    Define.Field ("role", memberRoleInChatEnumDef, "the member's role in the chat", (fun _ (x : MeAsAChatMember) -> x.Role))
                ]
        )

    let chatRoomStatsDef =
        Define.Object<ChatRoom> (
            name = nameof ChatRoom,
            description = "A chat room as viewed from the outside",
            isTypeOf = (fun o -> o :? ChatRoom),
            fieldsFn =
                fun () -> [
                    Define.Field (
                        "id",
                        GuidType,
                        "the chat room's ID",
                        fun _ (x : ChatRoom) ->
                            match x.Id with
                            | ChatRoomId theId -> theId
                    )
                    Define.Field ("name", StringType, "the chat room's name", (fun _ (x : ChatRoom) -> x.Name))
                    Define.Field ("members", ListOf chatMemberDef, "the members in the chat room", (fun _ (x : ChatRoom) -> x.Members))
                ]
        )

    let chatRoomDetailsDef =
        Define.Object<ChatRoomForMember> (
            name = nameof ChatRoomForMember,
            description = "A chat room as viewed by a chat room member",
            isTypeOf = (fun o -> o :? ChatRoomForMember),
            fieldsFn =
                fun () -> [
                    Define.Field (
                        "id",
                        GuidType,
                        "the chat room's ID",
                        fun _ (x : ChatRoomForMember) ->
                            match x.Id with
                            | ChatRoomId theId -> theId
                    )
                    Define.Field ("name", StringType, "the chat room's name", (fun _ (x : ChatRoomForMember) -> x.Name))
                    Define.Field (
                        "meAsAChatMember",
                        meAsAChatMemberDef,
                        "the chat member that queried the details",
                        fun _ (x : ChatRoomForMember) -> x.MeAsAChatMember
                    )
                    Define.Field (
                        "otherChatMembers",
                        ListOf chatMemberDef,
                        "the chat members excluding the one who queried the details",
                        fun _ (x : ChatRoomForMember) -> x.OtherChatMembers
                    )
                ]
        )

    let organizationStatsDef =
        Define.Object<Organization> (
            name = nameof Organization,
            description = "An organization as seen from the outside",
            isTypeOf = (fun o -> o :? Organization),
            fieldsFn =
                fun () -> [
                    Define.Field (
                        "id",
                        GuidType,
                        "the organization's ID",
                        fun _ (x : Organization) ->
                            match x.Id with
                            | OrganizationId theId -> theId
                    )
                    Define.Field ("name", StringType, "the organization's name", (fun _ (x : Organization) -> x.Name))
                    Define.Field ("members", ListOf memberDef, "members of this organization", (fun _ (x : Organization) -> x.Members))
                    Define.Field ("chatRooms", ListOf chatRoomStatsDef, "chat rooms in this organization", (fun _ (x : Organization) -> x.ChatRooms))
                ]
        )

    let organizationDetailsDef =
        Define.Object<OrganizationForMember> (
            name = nameof OrganizationForMember,
            description = "An organization as seen by one of the organization's members",
            isTypeOf = (fun o -> o :? OrganizationForMember),
            fieldsFn =
                fun () -> [
                    Define.Field (
                        "id",
                        GuidType,
                        "the organization's ID",
                        fun _ (x : OrganizationForMember) ->
                            match x.Id with
                            | OrganizationId theId -> theId
                    )
                    Define.Field ("name", StringType, "the organization's name", (fun _ (x : OrganizationForMember) -> x.Name))
                    Define.Field (
                        "meAsAMember",
                        meAsAMemberDef,
                        "the member that queried the details",
                        fun _ (x : OrganizationForMember) -> x.MeAsAMember
                    )
                    Define.Field (
                        "otherMembers",
                        ListOf memberDef,
                        "members of this organization",
                        fun _ (x : OrganizationForMember) -> x.OtherMembers
                    )
                    Define.Field (
                        "chatRooms",
                        ListOf chatRoomStatsDef,
                        "chat rooms in this organization",
                        fun _ (x : OrganizationForMember) -> x.ChatRooms
                    )
                ]
        )

    let aChatRoomMessageDef description name =
        Define.Object<ChatRoomMessage> (
            name = name,
            description = description,
            isTypeOf = (fun o -> o :? ChatRoomMessage),
            fieldsFn =
                fun () -> [
                    Define.Field (
                        "id",
                        GuidType,
                        "the message's ID",
                        fun _ (x : ChatRoomMessage) ->
                            match x.Id with
                            | MessageId theId -> theId
                    )
                    Define.Field (
                        "chatRoomId",
                        GuidType,
                        "the ID of the chat room the message belongs to",
                        fun _ (x : ChatRoomMessage) ->
                            match x.ChatRoomId with
                            | ChatRoomId theId -> theId
                    )
                    Define.Field (
                        "date",
                        DateTimeOffsetType,
                        "the time the message was received at the server",
                        fun _ (x : ChatRoomMessage) -> DateTimeOffset (x.Date, TimeSpan.Zero)
                    )
                    Define.Field (
                        "authorId",
                        GuidType,
                        "the member ID of the message's author",
                        fun _ (x : ChatRoomMessage) ->
                            match x.AuthorId with
                            | MemberId theId -> theId
                    )
                    Define.Field ("text", StringType, "the message's text", (fun _ (x : ChatRoomMessage) -> x.Text))
                ]
        )

    let anEmptyChatRoomEvent description name =
        Define.Object<unit> (
            name = name,
            description = description,
            isTypeOf = (fun o -> o :? unit),
            fieldsFn =
                fun () -> [
                    Define.Field ("doNotUse", BooleanType, "this is just to satify the expected structure of this type", (fun _ _ -> true))
                ]
        )

    let aChatRoomEventForMessageId description name =
        Define.Object<MessageId> (
            name = name,
            description = description,
            isTypeOf = (fun o -> o :? MessageId),
            fieldsFn =
                (fun () -> [
                    Define.Field (
                        "messageId",
                        GuidType,
                        "this is the message ID",
                        fun _ (x : MessageId) ->
                            match x with
                            | MessageId theId -> theId
                    )
                ])
        )

    let aChatRoomEventForMemberIdAndName description name =
        Define.Object<MemberId * string> (
            name = name,
            description = description,
            isTypeOf = (fun o -> o :? (MemberId * string)),
            fieldsFn =
                (fun () -> [
                    Define.Field (
                        "memberId",
                        GuidType,
                        "this is the member's ID",
                        fun _ (mId : MemberId, _ : string) ->
                            match mId with
                            | MemberId theId -> theId
                    )
                    Define.Field ("memberName", StringType, "this is the member's name", (fun _ (_ : MemberId, name : string) -> name))
                ])
        )

    let newMessageDef =
        nameof NewMessage
        |> aChatRoomMessageDef "a new public message has been sent in the chat room"
    let editedMessageDef =
        nameof EditedMessage
        |> aChatRoomMessageDef "a public message of the chat room has been edited"
    let deletedMessageDef =
        nameof DeletedMessage
        |> aChatRoomEventForMessageId "a public message of the chat room has been deleted"
    let memberJoinedDef =
        nameof MemberJoined
        |> aChatRoomEventForMemberIdAndName "a member has joined the chat"
    let memberLeftDef =
        nameof MemberLeft
        |> aChatRoomEventForMemberIdAndName "a member has left the chat"

    let chatRoomSpecificEventDef =
        Define.Union (
            name = nameof ChatRoomSpecificEvent,
            options = [ newMessageDef; editedMessageDef; deletedMessageDef; memberJoinedDef; memberLeftDef ],
            resolveValue =
                (fun o ->
                    match o with
                    | NewMessage x -> box x
                    | EditedMessage x -> upcast x
                    | DeletedMessage x -> upcast x
                    | MemberJoined (mId, mName) -> upcast (mId, mName)
                    | MemberLeft (mId, mName) -> upcast (mId, mName)),
            resolveType =
                (fun o ->
                    match o with
                    | NewMessage _ -> newMessageDef
                    | EditedMessage _ -> editedMessageDef
                    | DeletedMessage _ -> deletedMessageDef
                    | MemberJoined _ -> memberJoinedDef
                    | MemberLeft _ -> memberLeftDef),
            description = "data which is specific to a certain type of event"
        )

    let chatRoomEventDef =
        Define.Object<ChatRoomEvent> (
            name = nameof ChatRoomEvent,
            description = "Something that happened in the chat room, like a new message sent",
            isTypeOf = (fun o -> o :? ChatRoomEvent),
            fieldsFn =
                (fun () -> [
                    Define.Field (
                        "chatRoomId",
                        GuidType,
                        "the ID of the chat room in which the event happened",
                        fun _ (x : ChatRoomEvent) ->
                            match x.ChatRoomId with
                            | ChatRoomId theId -> theId
                    )
                    Define.Field (
                        "time",
                        DateTimeOffsetType,
                        "the time the message was received at the server",
                        fun _ (x : ChatRoomEvent) -> DateTimeOffset (x.Time, TimeSpan.Zero)
                    )
                    Define.Field (
                        "specificData",
                        chatRoomSpecificEventDef,
                        "the event's specific data",
                        fun _ (x : ChatRoomEvent) -> x.SpecificData
                    )
                ])
        )

    let query =
        Define.Object<Root> (
            name = "Query",
            fields = [
                Define.Field (
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
        {
            ChatRoomId = chatRoomId
            Time = DateTime.UtcNow
            SpecificData = specificEvent
        }
        |> schemaConfig.SubscriptionProvider.Publish<ChatRoomEvent> chatRoomEvents_subscription_name

    let mutation =
        Define.Object<Root> (
            name = "Mutation",
            fields = [
                Define.Field (
                    "enterOrganization",
                    organizationDetailsDef,
                    "makes a new member enter an organization",
                    [
                        Define.Input ("organizationId", GuidType, description = "the ID of the organization")
                        Define.Input ("member", StringType, description = "the new member's name")
                    ],
                    fun ctx root ->
                        let organizationId = OrganizationId (ctx.Arg ("organizationId"))
                        let newMemberName : string = ctx.Arg ("member")
                        let maybeResult =
                            FakePersistence.Organizations
                            |> Map.tryFind organizationId
                            |> Option.map MapFrom.organizationInDb_To_Organization
                            |> Option.map (fun organization ->
                                if
                                    organization.Members
                                    |> List.exists (fun m -> m.Name = newMemberName)
                                then
                                    raise (
                                        newMemberName
                                        |> Exceptions.Member_With_This_Name_Already_Exists
                                    )
                                else
                                    let newMemberPrivId = MemberPrivateId (Guid.NewGuid ())
                                    let newMemberId = MemberId (Guid.NewGuid ())
                                    let newMember = {
                                        Member_In_Db.PrivId = newMemberPrivId
                                        Id = newMemberId
                                        Name = newMemberName
                                    }
                                    FakePersistence.Members <- FakePersistence.Members |> Map.add newMemberId newMember

                                    FakePersistence.Organizations <-
                                        FakePersistence.Organizations
                                        |> Map.change
                                            organizationId
                                            (Option.bind (fun organization ->
                                                Some { organization with Members = newMemberId :: organization.Members }))
                                    FakePersistence.Organizations
                                    |> Map.find organizationId
                                    |> MapFrom.organizationInDb_To_OrganizationForMember newMemberId)
                            |> Option.flatten
                        match maybeResult with
                        | None -> raise (GQLMessageException ("couldn't enter organization (maybe the ID is incorrect?)"))
                        | Some res -> res
                )
                Define.Field (
                    "createChatRoom",
                    chatRoomDetailsDef,
                    "creates a new chat room for a user",
                    [
                        Define.Input ("organizationId", GuidType, description = "the ID of the organization in which the chat room will be created")
                        Define.Input ("memberId", GuidType, description = "the member's private ID")
                        Define.Input ("name", StringType, description = "the chat room's name")
                    ],
                    fun ctx root ->
                        let organizationId = OrganizationId (ctx.Arg ("organizationId"))
                        let memberPrivId = MemberPrivateId (ctx.Arg ("memberId"))
                        let chatRoomName : string = ctx.Arg ("name")

                        memberPrivId
                        |> authenticateMemberInOrganization organizationId
                        |> Result.map (fun (organization, theMember) ->
                            let newChatRoomId = ChatRoomId (Guid.NewGuid ())
                            let newChatMember : ChatMember_In_Db = { ChatRoomId = newChatRoomId; MemberId = theMember.Id; Role = ChatAdmin }
                            let newChatRoom : ChatRoom_In_Db = { Id = newChatRoomId; Name = chatRoomName; Members = [ theMember.Id ] }
                            FakePersistence.ChatRooms <-
                                FakePersistence.ChatRooms
                                |> Map.add newChatRoomId newChatRoom
                            FakePersistence.ChatMembers <-
                                FakePersistence.ChatMembers
                                |> Map.add (newChatRoomId, theMember.Id) newChatMember
                            FakePersistence.Organizations <-
                                FakePersistence.Organizations
                                |> Map.change organizationId (Option.map (fun org -> { org with ChatRooms = newChatRoomId :: org.ChatRooms }))

                            MapFrom.chatRoomInDb_To_ChatRoomForMember
                                (FakePersistence.Members.Values
                                 |> Seq.filter (fun x -> organization.Members |> List.contains x.Id))
                                newChatMember
                                newChatRoom)
                        |> succeedOrRaiseGraphQLEx
                )
                Define.Field (
                    "enterChatRoom",
                    chatRoomDetailsDef,
                    "makes a member enter a chat room",
                    [
                        Define.Input ("organizationId", GuidType, description = "the ID of the organization the chat room and member are in")
                        Define.Input ("chatRoomId", GuidType, description = "the ID of the chat room")
                        Define.Input ("memberId", GuidType, description = "the member's private ID")
                    ],
                    fun ctx root ->
                        let organizationId = OrganizationId (ctx.Arg ("organizationId"))
                        let chatRoomId = ChatRoomId (ctx.Arg ("chatRoomId"))
                        let memberPrivId = MemberPrivateId (ctx.Arg ("memberId"))

                        memberPrivId
                        |> authenticateMemberInOrganization organizationId
                        |> Result.bind (fun (organization, theMember) ->
                            chatRoomId
                            |> validateChatRoomExistence organization
                            |> Result.map (fun chatRoom -> (organization, chatRoom, theMember)))
                        |> Result.map (fun (_, chatRoom, theMember) ->
                            let newChatMember : ChatMember_In_Db = { ChatRoomId = chatRoom.Id; MemberId = theMember.Id; Role = ChatGuest }
                            FakePersistence.ChatMembers <-
                                FakePersistence.ChatMembers
                                |> Map.add (newChatMember.ChatRoomId, newChatMember.MemberId) newChatMember
                            FakePersistence.ChatRooms <-
                                FakePersistence.ChatRooms
                                |> Map.change
                                    chatRoom.Id
                                    (Option.map (fun theChatRoom -> { theChatRoom with Members = newChatMember.MemberId :: theChatRoom.Members }))
                            let theChatRoom = FakePersistence.ChatRooms |> Map.find chatRoomId
                            let result =
                                MapFrom.chatRoomInDb_To_ChatRoomForMember
                                    (FakePersistence.Members.Values
                                     |> Seq.filter (fun x -> theChatRoom.Members |> List.contains x.Id))
                                    newChatMember
                                    theChatRoom

                            chatRoom.Id
                            |> publishChatRoomEvent (MemberJoined (theMember.Id, theMember.Name))

                            result)
                        |> succeedOrRaiseGraphQLEx
                )
                Define.Field (
                    "leaveChatRoom",
                    BooleanType,
                    "makes a member leave a chat room",
                    [
                        Define.Input ("organizationId", GuidType, description = "the ID of the organization the chat room and member are in")
                        Define.Input ("chatRoomId", GuidType, description = "the ID of the chat room")
                        Define.Input ("memberId", GuidType, description = "the member's private ID")
                    ],
                    fun ctx root ->
                        let organizationId = OrganizationId (ctx.Arg ("organizationId"))
                        let chatRoomId = ChatRoomId (ctx.Arg ("chatRoomId"))
                        let memberPrivId = MemberPrivateId (ctx.Arg ("memberId"))

                        memberPrivId
                        |> authenticateMemberInOrganization organizationId
                        |> Result.bind (fun (organization, theMember) ->
                            chatRoomId
                            |> validateChatRoomExistence organization
                            |> Result.map (fun chatRoom -> (organization, chatRoom, theMember)))
                        |> Result.map (fun (_, chatRoom, theMember) ->
                            FakePersistence.ChatMembers <-
                                FakePersistence.ChatMembers
                                |> Map.remove (chatRoom.Id, theMember.Id)
                            FakePersistence.ChatRooms <-
                                FakePersistence.ChatRooms
                                |> Map.change
                                    chatRoom.Id
                                    (Option.map (fun theChatRoom -> {
                                        theChatRoom with
                                            Members =
                                                theChatRoom.Members
                                                |> List.filter (fun mId -> mId <> theMember.Id)
                                    }))
                            true)
                        |> succeedOrRaiseGraphQLEx
                )
                Define.Field (
                    "sendChatMessage",
                    BooleanType,
                    [
                        Define.Input ("organizationId", GuidType, description = "the ID of the organization the chat room and member are in")
                        Define.Input ("chatRoomId", GuidType, description = "the chat room's ID")
                        Define.Input ("memberId", GuidType, description = "the member's private ID")
                        Define.Input ("text", StringType, description = "the chat message's contents")
                    ],
                    fun ctx _ ->
                        let organizationId = OrganizationId (ctx.Arg ("organizationId"))
                        let chatRoomId = ChatRoomId (ctx.Arg ("chatRoomId"))
                        let memberPrivId = MemberPrivateId (ctx.Arg ("memberId"))
                        let text : string = ctx.Arg ("text")

                        memberPrivId
                        |> authenticateMemberInOrganization organizationId
                        |> Result.bind (fun (organization, theMember) ->
                            chatRoomId
                            |> validateChatRoomExistence organization
                            |> Result.map (fun chatRoom -> (organization, chatRoom, theMember)))
                        |> Result.map (fun (_, chatRoom, theMember) ->
                            let newChatRoomMessage = {
                                Id = MessageId (Guid.NewGuid ())
                                ChatRoomId = chatRoom.Id
                                Date = DateTime.UtcNow
                                AuthorId = theMember.Id
                                Text = text
                            }
                            FakePersistence.ChatRoomMessages <-
                                FakePersistence.ChatRoomMessages
                                |> Map.add (chatRoom.Id, newChatRoomMessage.Id) newChatRoomMessage

                            chatRoom.Id
                            |> publishChatRoomEvent (NewMessage newChatRoomMessage)

                            true)
                        |> succeedOrRaiseGraphQLEx
                )
                Define.Field (
                    "editChatMessage",
                    BooleanType,
                    [
                        Define.Input ("organizationId", GuidType, description = "the ID of the organization the chat room and member are in")
                        Define.Input ("chatRoomId", GuidType, description = "the chat room's ID")
                        Define.Input ("memberId", GuidType, description = "the member's private ID")
                        Define.Input ("messageId", GuidType, description = "the existing message's ID")
                        Define.Input ("text", StringType, description = "the chat message's contents")
                    ],
                    fun ctx _ ->
                        let organizationId = OrganizationId (ctx.Arg ("organizationId"))
                        let chatRoomId = ChatRoomId (ctx.Arg ("chatRoomId"))
                        let memberPrivId = MemberPrivateId (ctx.Arg ("memberId"))
                        let messageId = MessageId (ctx.Arg ("messageId"))
                        let text : string = ctx.Arg ("text")

                        memberPrivId
                        |> authenticateMemberInOrganization organizationId
                        |> Result.bind (fun (organization, theMember) ->
                            chatRoomId
                            |> validateChatRoomExistence organization
                            |> Result.bind (fun chatRoom ->
                                messageId
                                |> validateMessageExistence chatRoom
                                |> Result.map (fun x -> (chatRoom, x)))
                            |> Result.map (fun (chatRoom, chatMessage) -> (organization, chatRoom, theMember, chatMessage)))
                        |> Result.map (fun (_, chatRoom, theMember, chatMessage) ->
                            let newChatRoomMessage = {
                                Id = chatMessage.Id
                                ChatRoomId = chatRoom.Id
                                Date = chatMessage.Date
                                AuthorId = theMember.Id
                                Text = text
                            }
                            FakePersistence.ChatRoomMessages <-
                                FakePersistence.ChatRoomMessages
                                |> Map.change (chatRoom.Id, newChatRoomMessage.Id) (Option.map (fun _ -> newChatRoomMessage))

                            chatRoom.Id
                            |> publishChatRoomEvent (EditedMessage newChatRoomMessage)

                            true)
                        |> succeedOrRaiseGraphQLEx
                )
                Define.Field (
                    "deleteChatMessage",
                    BooleanType,
                    [
                        Define.Input ("organizationId", GuidType, description = "the ID of the organization the chat room and member are in")
                        Define.Input ("chatRoomId", GuidType, description = "the chat room's ID")
                        Define.Input ("memberId", GuidType, description = "the member's private ID")
                        Define.Input ("messageId", GuidType, description = "the existing message's ID")
                    ],
                    fun ctx _ ->
                        let organizationId = OrganizationId (ctx.Arg ("organizationId"))
                        let chatRoomId = ChatRoomId (ctx.Arg ("chatRoomId"))
                        let memberPrivId = MemberPrivateId (ctx.Arg ("memberId"))
                        let messageId = MessageId (ctx.Arg ("messageId"))

                        memberPrivId
                        |> authenticateMemberInOrganization organizationId
                        |> Result.bind (fun (organization, theMember) ->
                            chatRoomId
                            |> validateChatRoomExistence organization
                            |> Result.bind (fun chatRoom ->
                                messageId
                                |> validateMessageExistence chatRoom
                                |> Result.map (fun x -> (chatRoom, x)))
                            |> Result.map (fun (chatRoom, chatMessage) -> (organization, chatRoom, theMember, chatMessage)))
                        |> Result.map (fun (_, chatRoom, theMember, chatMessage) ->
                            FakePersistence.ChatRoomMessages <-
                                FakePersistence.ChatRoomMessages
                                |> Map.remove (chatRoom.Id, chatMessage.Id)

                            chatRoom.Id
                            |> publishChatRoomEvent (DeletedMessage chatMessage.Id)

                            true)
                        |> succeedOrRaiseGraphQLEx
                )
            ]
        )

    let rootDef =
        Define.Object<Root> (
            name = "Root",
            description = "contains general request information",
            isTypeOf = (fun o -> o :? Root),
            fieldsFn =
                fun () -> [
                    Define.Field ("requestId", StringType, "The request's unique ID.", (fun _ (r : Root) -> r.RequestId))
                ]
        )

    let subscription =
        Define.SubscriptionObject<Root> (
            name = "Subscription",
            fields = [
                Define.SubscriptionField (
                    chatRoomEvents_subscription_name,
                    rootDef,
                    chatRoomEventDef,
                    "events related to a specific chat room",
                    [
                        Define.Input ("chatRoomId", GuidType, description = "the ID of the chat room to listen to events from")
                        Define.Input ("memberId", GuidType, description = "the member's private ID")
                    ],
                    (fun ctx _ (chatRoomEvent : ChatRoomEvent) ->
                        let chatRoomIdOfInterest = ChatRoomId (ctx.Arg ("chatRoomId"))
                        let memberId = MemberPrivateId (ctx.Arg ("memberId"))

                        if chatRoomEvent.ChatRoomId <> chatRoomIdOfInterest then
                            None
                        else
                            let chatRoom =
                                FakePersistence.ChatRooms
                                |> Map.find chatRoomEvent.ChatRoomId
                            let chatRoomMembersPrivIds =
                                FakePersistence.Members.Values
                                |> Seq.filter (fun m -> chatRoom.Members |> List.contains m.Id)
                                |> Seq.map (fun m -> m.PrivId)
                            if not (chatRoomMembersPrivIds |> Seq.contains memberId) then
                                None
                            else
                                Some chatRoomEvent)
                )
            ]
        )

    let schema : ISchema<Root> = Schema (query, mutation, subscription, schemaConfig)

    let executor = Executor (schema, [])
