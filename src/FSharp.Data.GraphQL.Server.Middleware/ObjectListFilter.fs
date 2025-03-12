namespace FSharp.Data.GraphQL.Server.Middleware

open System
open System.Linq
open System.Linq.Expressions
open System.Runtime.InteropServices
open Microsoft.FSharp.Quotations

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
    | OfTypes of FieldFilter<Type list>
    | FilterField of FieldFilter<ObjectListFilter>
    | NoFilter

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

//[<AutoOpen>]
//module ObjectListFilterExtensions =

//    type ObjectListFilter with

//        member filter.Apply<'T, 'D>(query : IQueryable<'T>,
//                                   compareDiscriminator : Expr<'T -> 'D -> 'D> | null,
//                                   getDiscriminatorValue : (Type -> 'D) | null) =
//            filter.Apply(query, compareDiscriminator, getDiscriminatorValue)

//        member filter.Apply<'T, 'D>(query : IQueryable<'T>,
//                                   [<Optional>] getDiscriminator : Expr<'T -> 'D> | null,
//                                   [<Optional>] getDiscriminatorValue : (Type -> 'D) | null) =
//            // Helper to create parameter expression for the lambda
//            let param = Expression.Parameter(typeof<'T>, "x")

//            // Helper to get property value
//            let getPropertyExpr fieldName =
//                Expression.PropertyOrField(param, fieldName)

//            // Helper to create lambda from body expression
//            let makeLambda (body: Expression) =
//                let delegateType = typedefof<Func<_,_>>.MakeGenericType([|typeof<'T>; body.Type|])
//                Expression.Lambda(delegateType, body, param)

//            // Helper to create Where expression
//            let whereExpr predicate =
//                let whereMethod =
//                    typeof<Queryable>.GetMethods()
//                    |> Seq.where (fun m -> m.Name = "Where")
//                    |> Seq.find (fun m ->
//                        let parameters = m.GetParameters()
//                        parameters.Length = 2
//                        && parameters[1].ParameterType.GetGenericTypeDefinition() = typedefof<Expression<Func<_,_>>>)
//                    |> fun m -> m.MakeGenericMethod([|typeof<'T>|])
//                Expression.Call(whereMethod, [|query.Expression; makeLambda predicate|])

//            // Helper for discriminator comparison
//            let buildTypeDiscriminatorCheck (t: Type) =
//                match getDiscriminator, getDiscriminatorValue with
//                | null, _ | _, null -> None
//                | discExpr, discValueFn ->
//                    let compiled = QuotationEvaluator.Eval(discExpr)
//                    let discriminatorValue = discValueFn t
//                    let discExpr = getPropertyExpr "__discriminator" // Assuming discriminator field name
//                    let valueExpr = Expression.Constant(discriminatorValue)
//                    Some(Expression.Equal(discExpr, valueExpr))

//            // Main filter logic
//            let rec buildFilterExpr filter =
//                match filter with
//                | NoFilter -> query.Expression
//                | And (f1, f2) ->
//                    let q1 = buildFilterExpr f1 |> Expression.Lambda<Func<IQueryable<'T>>>|> _.Compile().Invoke()
//                    buildFilterExpr f2 |> Expression.Lambda<Func<IQueryable<'T>>> |> _.Compile().Invoke(q1).Expression
//                | Or (f1, f2) ->
//                    let expr1 = buildFilterExpr f1
//                    let expr2 = buildFilterExpr f2
//                    let unionMethod =
//                        typeof<Queryable>.GetMethods()
//                        |> Array.find (fun m -> m.Name = "Union")
//                        |> fun m -> m.MakeGenericMethod([|typeof<'T>|])
//                    Expression.Call(unionMethod, [|expr1; expr2|])
//                | Not f ->
//                    let exceptMethod =
//                        typeof<Queryable>.GetMethods()
//                        |> Array.find (fun m -> m.Name = "Except")
//                        |> fun m -> m.MakeGenericMethod([|typeof<'T>|])
//                    Expression.Call(exceptMethod, [|query.Expression; buildFilterExpr f|])
//                | Equals f ->
//                    Expression.Equal(getPropertyExpr f.FieldName, Expression.Constant(f.Value)) |> whereExpr
//                | GreaterThan f ->
//                    Expression.GreaterThan(getPropertyExpr f.FieldName, Expression.Constant(f.Value)) |> whereExpr
//                | LessThan f ->
//                    Expression.LessThan(getPropertyExpr f.FieldName, Expression.Constant(f.Value)) |> whereExpr
//                | StartsWith f ->
//                    let methodInfo = typeof<string>.GetMethod("StartsWith", [|typeof<string>|])
//                    Expression.Call(getPropertyExpr f.FieldName, methodInfo, Expression.Constant(f.Value)) |> whereExpr
//                | EndsWith f ->
//                    let methodInfo = typeof<string>.GetMethod("EndsWith", [|typeof<string>|])
//                    Expression.Call(getPropertyExpr f.FieldName, methodInfo, Expression.Constant(f.Value)) |> whereExpr
//                | Contains f ->
//                    let methodInfo = typeof<string>.GetMethod("Contains", [|typeof<string>|])
//                    Expression.Call(getPropertyExpr f.FieldName, methodInfo, Expression.Constant(f.Value)) |> whereExpr
//                | OfTypes types ->
//                    match types.Value with
//                    | [] -> query.Expression // No types specified, return original query
//                    | types ->
//                        let typeChecks =
//                            types
//                            |> List.choose buildTypeDiscriminatorCheck
//                            |> List.fold (fun acc expr ->
//                                match acc with
//                                | None -> Some expr
//                                | Some prevExpr -> Some(Expression.OrElse(prevExpr, expr))) None

//                        match typeChecks with
//                        | None -> query.Expression
//                        | Some expr -> whereExpr expr
//                | FilterField f ->
//                    let propExpr = getPropertyExpr f.FieldName
//                    match propExpr.Type.GetInterfaces()
//                          |> Array.tryFind (fun t ->
//                              t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<IQueryable<_>>) with
//                    | Some queryableType ->
//                        let elementType = queryableType.GetGenericArguments().[0]
//                        let subFilter = f.Value
//                        let subQuery = Expression.Convert(propExpr, queryableType)
//                        Expression.Call(typeof<Queryable>, "Any", [|elementType|], subQuery) |> whereExpr
//                    | None -> query.Expression

//            // Create and execute the final expression
//            query.Provider.CreateQuery<'T>(buildFilterExpr filter)
