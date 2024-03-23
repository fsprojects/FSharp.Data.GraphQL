namespace FSharp.Data.GraphQL.Samples.ChatApp

open System

type FakePersistence () =

    static let mutable members = Map.empty<MemberId, Member_In_Db>
    static let mutable chatMembers = Map.empty<ChatRoomId * MemberId, ChatMember_In_Db>
    static let mutable chatRoomMessages = Map.empty<ChatRoomId * MessageId, ChatRoomMessage>
    static let mutable chatRooms = Map.empty<ChatRoomId, ChatRoom_In_Db>
    static let mutable organizations =
        let newId = OrganizationId (Guid.Parse ("51f823ef-2294-41dc-9f39-a4b9a237317a"))
        (newId, { Organization_In_Db.Id = newId; Name = "Public"; Members = []; ChatRooms = [] })
        |> List.singleton
        |> Map.ofList

    static member Members
        with get () = members
        and set (v) = members <- v

    static member ChatMembers
        with get () = chatMembers
        and set (v) = chatMembers <- v

    static member ChatRoomMessages
        with get () = chatRoomMessages
        and set (v) = chatRoomMessages <- v

    static member ChatRooms
        with get () = chatRooms
        and set (v) = chatRooms <- v

    static member Organizations
        with get () = organizations
        and set (v) = organizations <- v
