module internal FSharp.Data.GraphQL.Server.Middlewares.ListField

open FSharp.Data.GraphQL.Types

let private sortInput = Define.Input("sort", ListOf String)
let private lastInput = Define.Input("last", Int)
let private firstInput = Define.Input("first", Int)
let private skipInput = Define.Input("skip", Int)
let private filterInput = Define.Input("filter", Obj)
let inputs = [ sortInput; lastInput; firstInput; skipInput; filterInput ]