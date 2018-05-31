namespace FSharp.Data.GraphQL.Server.Middlewares

/// A filter definition for a field value.
type FieldFilter<'Val> =
    { FieldName : string
      Value : 'Val }

/// A filter definition for an object list.
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
    | FilterField of FieldFilter<ObjectListFilter>

/// Contains tooling for working with ObjectListFilter.
module ObjectListFilter =
    /// Contains operators for building and comparing ObjectListFilter values.
    module Operators =
        /// Creates a new ObjectListFilter representing an AND operation between two existing ones.
        let ( &&& ) x y = And (x, y)

        /// Creates a new ObjectListFilter representing an OR operation between two existing ones.
        let ( ||| ) x y = Or (x, y)

        /// Creates a new ObjectListFilter representing an EQUALS operation between two comparable values.
        let ( === ) fname value = Equals { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing a GREATER THAN operation of a comparable value.
        let ( ==> ) fname value = GreaterThan { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing a LESS THAN operation of a comparable value.
        let ( <== ) fname value = LessThan { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing a STARTS WITH operation of a string value.
        let ( =@@ ) fname value = StartsWith { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing an ENDS WITH operation of a string value.
        let ( @@= ) fname value = EndsWith { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing a CONTAINS operation.
        let ( @=@ ) fname value = Contains { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing a field sub comparison.
        let ( --> ) fname filter = FilterField { FieldName = fname; Value = filter }

        /// Creates a new ObjectListFilter representing a NOT opreation for the existing one.
        let ( !!! ) filter = Not filter