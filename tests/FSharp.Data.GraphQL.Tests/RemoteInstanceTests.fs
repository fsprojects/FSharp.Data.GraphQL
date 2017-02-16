
module FSharp.Data.GraphQL.Tests.RemoteInstanceTests

open System
open Xunit
open FsCheck
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Relay
open FSharp.Data.GraphQL.RemoteSchema
open Newtonsoft.Json

type IPet =
    interface
        abstract Name : string
        abstract Weight: int
        abstract Friends: string[]
    end 

type Dog = 
    { Name: string; Woofs: bool; Weight: int; Friends: string[] }
    interface IPet with
        member x.Name = x.Name
        member x.Weight = x.Weight
        member x.Friends = x.Friends

type Cat = 
    { Name: string; Meows: bool; Weight: int; Friends: string[] }
    interface IPet with
        member x.Name = x.Name
        member x.Weight = x.Weight
        member x.Friends = x.Friends

type Human = { Name: string; }

type Pet =
    | DogCase of Dog
    | CatCase of Cat


type Friend = 
    {
        Name: string
        Weight: int
    }

let FriendType =Define.Object<Friend>(
        name = "Friend",
        isTypeOf = is<Friend>,
        fields = [
            Define.Field("name", String, fun _ d -> d.Name)
            Define.Field("weight", Int, fun _ d -> d.Weight)
        ])

let remoteSchema =
    Schema<obj>(
        query = Define.Object("Query", fun () ->
        [
            Define.Field("friends", ListOf FriendType, (fun _ _ -> [{ Name = "friend 1"; Weight=10; } :> Friend ; upcast { Name = "friend 2"; Weight=20; } ]))
        ]), 
        config = { SchemaConfig.Default with Types = [FriendType] })


type RemoteSchemaHandler() =
    interface IRemoteSchemaHandler with
        member x.Resolve resolveFieldCtx value = 
            // serizing needed information
            let astString = JsonConvert.SerializeObject(resolveFieldCtx.ExecutionInfo.Ast)
            let plan = resolveFieldCtx.Context.ExecutionPlan
            let operationType =
                match plan.Operation.OperationType with
                    | Query -> query 
                    | Mutation -> mutation


            // TODO : pass info 


            // get information on server:
            let ast = JsonConvert.DeserializeObject<Ast.Field>(astString)
            let oType = match operationType with
            | query -> Ast.Query
            | mutation -> Ast.Mutation

            let operationDefinition = 
                {
                    Ast.OperationDefinition.OperationType = oType
                    Ast.OperationDefinition.Name = Some "subquery"
                    Ast.OperationDefinition.VariableDefinitions = []
                    Ast.OperationDefinition.Directives = []
                    Ast.OperationDefinition.SelectionSet = [
                          Ast.Selection.Field (ast)
                    ]
                }

            let sp = Executor(remoteSchema)
            let doc = {
                Ast.Document.Definitions = [
                    Ast.Definition.OperationDefinition operationDefinition
                ]
            }

            let result = sync <| sp.AsyncExecute(doc)
            let errors = result.TryGetValue("errors")
            let resData = result.["data"] :?> Collections.Generic.IDictionary<string, obj>

            AsyncVal.wrap(resData.[ast.Name] :> obj)


[<Fact>]
let ``Execute subschema`` () = 
    let PetType = Define.Interface("Pet", fun () -> [ Define.Field("name", String); Define.Field("friends", Obj) ])
    let DogType =
      Define.Object<Dog>(
        name = "Dog", 
        isTypeOf = is<Dog>,
        interfaces = [ PetType ],
        fields = [
            Define.Field("name", String, fun _ d -> d.Name)
            Define.Field("woofs", Boolean, fun _ d -> d.Woofs)
            Define.Field("weight", Int, fun _ d -> d.Weight)
            Define.RemoteSchemaField("friends", RemoteSchemaHandler())
        ])
    let CatType =
      Define.Object<Cat>(
        name = "Cat", 
        isTypeOf = is<Cat>,
        interfaces = [ PetType ],
        fields = [
            Define.Field("name", String, fun _ c -> c.Name)
            Define.Field("meows", Boolean, fun _ c -> c.Meows)
            Define.Field("weight", Int, fun _ d -> d.Weight)
            Define.Field("friends", Obj, fun _ d -> d.Friends :> obj)
        ])
    let schema =
      Schema(
        query = Define.Object("Query", fun () ->
        [
            Define.Field("pets", ListOf PetType, (fun _ _ -> [ { Name = "Odie"; Woofs = true; Weight=10; Friends=[|"bb"|] } :> IPet ; upcast { Name = "Garfield"; Meows = false; Weight=20; Friends=[||] } ]))
        ]), 
        config = { SchemaConfig.Default with Types = [CatType; DogType] })
    let schemaProcessor = Executor(schema)
    let query = """{
      pets {
        name
        ... on Dog {
          woofs
        }
        ... on Cat {
          meows
        }
        friends {
            name
            weight
        }
      }
    }"""
    let result = sync <| schemaProcessor.AsyncExecute(parse query)
    let expected =
      NameValueLookup.ofList [
          "pets", upcast [
            NameValueLookup.ofList [
                "name", "Odie" :> obj
                "woofs", upcast true
                "friends", upcast [
                    NameValueLookup.ofList [
                        "name", upcast "friend 1"
                        "weight", upcast 10
                    ]
                    NameValueLookup.ofList [
                        "name", upcast "friend 2"
                        "weight", upcast 20
                    ]
                        ] ] :> obj
            upcast NameValueLookup.ofList [
                "name", "Garfield" :> obj
                "meows", upcast false
                "friends", upcast []]]]
    noErrors result
    result.["data"] |> equals (upcast expected)



//type Widget = 
//    { Id: string;
//      Name: string }

//type User = 
//    { Id: string;
//      Name: string;
//      Widgets: Widget list }

//[<Fact>]
//let ``Execute handles execution of abstract types: isTypeOf is used to resolve runtime type for Interface1111`` () = 
//    let rec Widget = Define.Object<Widget>(
//        name = "Widget",
//        description = "A shiny widget",
//        interfaces = [ Node ],
//        fields = [
//            Define.GlobalIdField(fun _ w -> w.Id)
//            Define.Field("name", String, fun _ w -> w.Name)])

//    and User = Define.Object<User>(
//        name = "User",
//        description = "A person who uses our app",
//        interfaces = [ Node ],
//        fields = [
//            Define.GlobalIdField(fun _ w -> w.Id)
//            Define.Field("name", String, fun _ w -> w.Name)
//            Define.Field("widgets", ConnectionOf Widget, "A person's collection of widgets", Connection.allArgs, fun ctx user -> 
//                let widgets = user.Widgets |> List.toArray
//                Connection.ofArray widgets )])

//    and Node = Define.Node<obj>(fun () -> [ User; Widget ])
//    ()

