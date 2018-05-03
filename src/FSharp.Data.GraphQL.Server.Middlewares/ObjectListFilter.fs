namespace FSharp.Data.GraphQL.Server.Middlewares

type FieldFilter<'Val> =
    { FieldName : string
      Value : 'Val }

type ObjectListFilter =
    | And of ObjectListFilter * ObjectListFilter
    | Or of ObjectListFilter * ObjectListFilter
    | Not of ObjectListFilter
    | Equals of FieldFilter<System.IComparable>
    | GreaterThan of FieldFilter<System.IComparable>
    | LessThan of FieldFilter<System.IComparable>
    | StartsWith of FieldFilter<string>
    | EndsWith of FieldFilter<string>
    | Contains of FieldFilter<string>
    | Field of FieldFilter<ObjectListFilter>

[<AutoOpen>]
module ObjectListFilterOperators =
    let ( &&& ) x y = And (x, y)
    let ( ||| ) x y = Or (x, y)
    let ( === ) fname value = Equals { FieldName = fname; Value = value }
    let ( ==> ) fname value = GreaterThan { FieldName = fname; Value = value }
    let ( <== ) fname value = LessThan { FieldName = fname; Value = value }
    let ( =@@ ) fname value = StartsWith { FieldName = fname; Value = value }
    let ( @@= ) fname value = EndsWith { FieldName = fname; Value = value }
    let ( @=@ ) fname value = Contains { FieldName = fname; Value = value }
    let ( --> ) fname filter = Field { FieldName = fname; Value = filter }
    let ( !!! ) filter = Not filter