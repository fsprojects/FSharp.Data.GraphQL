namespace FSharp.Data.GraphQL.Samples.ChatApp

open System

type FakePersistence () =
    static let mutable _members = Map.empty<MemberId, Member_In_Db>
    static let mutable _chatMembers = Map.empty<ChatRoomId * MemberId, ChatMember_In_Db>
    static let mutable _chatRoomMessages = Map.empty<ChatRoomId * MessageId, ChatRoomMessage>
    static let mutable _chatRooms = Map.empty<ChatRoomId, ChatRoom_In_Db>
    static let mutable _organizations =
        let newId = OrganizationId (Guid.Parse ("51f823ef-2294-41dc-9f39-a4b9a237317a"))
        (newId, { Organization_In_Db.Id = newId; Name = "Public"; Members = []; ChatRooms = [] })
        |> List.singleton
        |> Map.ofList

    static member Members
        with get () = _members
        and set (v) = _members <- v

    static member ChatMembers
        with get () = _chatMembers
        and set (v) = _chatMembers <- v

    static member ChatRoomMessages
        with get () = _chatRoomMessages
        and set (v) = _chatRoomMessages <- v

    static member ChatRooms
        with get () = _chatRooms
        and set (v) = _chatRooms <- v

    static member Organizations
        with get () = _organizations
        and set (v) = _organizations <- v
