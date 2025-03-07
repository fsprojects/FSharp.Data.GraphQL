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

[<AutoOpen>]
module ObjectListFilterExtensions =

    //// discriminator custom condition
    //let a () =
    //    filter.Apply(
    //        queryable,
    //        <@ fun e d -> e.Discriminator.StartsWith d @>,
    //        function
    //        | t when Type.(=)(t, typeof<Complex>) -> ResidentialPropertiesConstants.Discriminators.Complex
    //        | t when Type.(=)(t, typeof<Building>) -> ResidentialPropertiesConstants.Discriminators.Building
    //    )
    //// discriminator equals
    //let b () =
    //    filter.Apply(
    //        queryable,
    //        <@ fun e -> e.Discriminator @>,
    //        function
    //        | t when Type.(=)(t, typeof<Complex>) -> ResidentialPropertiesConstants.Discriminators.Complex
    //        | t when Type.(=)(t, typeof<Building>) -> ResidentialPropertiesConstants.Discriminators.Building
    //    )

    // Helper to create parameter expression for the lambda
    let param<'T> = Expression.Parameter(typeof<'T>, "x")

    // Helper to get property value
    let getPropertyExpr<'T> (param: ParameterExpression) fieldName =
        Expression.PropertyOrField(param, fieldName)

    // Helper to create lambda from body expression
    let makeLambda<'T> (param: ParameterExpression) (body: Expression) =
        let delegateType = typedefof<Func<_,_>>.MakeGenericType([|typeof<'T>; body.Type|])
        Expression.Lambda(delegateType, body, param)

    // Helper to create Where expression
    let whereExpr<'T> (query : IQueryable<'T>) (param: ParameterExpression) predicate =
        let whereMethod =
            typeof<Queryable>.GetMethods()
            |> Seq.where (fun m -> m.Name = "Where")
            |> Seq.find (fun m ->
                let parameters = m.GetParameters()
                parameters.Length = 2
                && parameters[1].ParameterType.GetGenericTypeDefinition() = typedefof<Expression<Func<_,_>>>)
            |> fun m -> m.MakeGenericMethod([|typeof<'T>|])
        Expression.Call(whereMethod, [|query.Expression; makeLambda<'T> param predicate|])

    // Main filter logic
    let rec buildFilterExpr<'T> (param: ParameterExpression) buildTypeDiscriminatorCheck filter (query : IQueryable<'T>) =
        let buildFilterExpr = buildFilterExpr<'T> param buildTypeDiscriminatorCheck
        match filter with
        | NoFilter -> query.Expression
        | And (f1, f2) ->
            let q1 = buildFilterExpr f1 query |> Expression.Lambda<Func<IQueryable<'T>>> |> _.Compile().Invoke()
            let q2 = buildFilterExpr f2 q1 |> Expression.Lambda<Func<IQueryable<'T>>> |> _.Compile().Invoke()
            q2.Expression
        //| Or (f1, f2) ->
        //    let expr1 = buildFilterExpr f1 query
        //    let expr2 = buildFilterExpr f2 query
        //    Expression.OrElse(expr1, expr2) |> whereExpr<'T> query param :> Expression
        //| Not f ->
        //    let exceptMethod =
        //        typeof<Queryable>.GetMethods()
        //        |> Array.find (fun m -> m.Name = "Except")
        //        |> fun m -> m.MakeGenericMethod([|typeof<'T>|])
        //    Expression.Call(exceptMethod, [|query.Expression; buildFilterExpr f|])
        //| OfTypes types ->
        //    match types.Value with
        //    | [] -> query.Expression // No types specified, return original query
        //    | types ->
        //        let typeChecks =
        //            types
        //            |> List.vchoose buildTypeDiscriminatorCheck
        //            |> List.fold (fun acc expr ->
        //                match acc with
        //                | ValueNone -> ValueSome expr
        //                | ValueSome prevExpr -> ValueSome (Expression.OrElse(prevExpr, expr))) ValueNone

        //        match typeChecks with
        //        | ValueNone -> query.Expression
        //        | ValueSome expr -> whereExpr  query expr :> Expression
        | Equals f ->
            Expression.Equal(getPropertyExpr<'T> param f.FieldName, Expression.Constant(f.Value)) |> whereExpr<'T> query param :> Expression
        | GreaterThan f ->
            Expression.GreaterThan(getPropertyExpr<'T> param f.FieldName, Expression.Constant(f.Value)) |> whereExpr<'T> query param :> Expression
        | LessThan f ->
            Expression.LessThan(getPropertyExpr<'T> param f.FieldName, Expression.Constant(f.Value)) |> whereExpr<'T> query param :> Expression
        | StartsWith f ->
            let methodInfo = typeof<string>.GetMethod("StartsWith", [|typeof<string>|])
            Expression.Call(getPropertyExpr<'T> param f.FieldName, methodInfo, Expression.Constant(f.Value)) |> whereExpr<'T> query param :> Expression
        | EndsWith f ->
            let methodInfo = typeof<string>.GetMethod("EndsWith", [|typeof<string>|])
            Expression.Call(getPropertyExpr<'T> param f.FieldName, methodInfo, Expression.Constant(f.Value)) |> whereExpr<'T> query param :> Expression
        | Contains f ->
            let methodInfo = typeof<string>.GetMethod("Contains", [|typeof<string>|])
            Expression.Call(getPropertyExpr<'T> param f.FieldName, methodInfo, Expression.Constant(f.Value)) |> whereExpr<'T> query param :> Expression
        | FilterField f ->
            let propExpr = getPropertyExpr<'T> param f.FieldName
            match propExpr.Type.GetInterfaces()
                    |> Array.tryFind (fun t ->
                        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<IQueryable<_>>) with
            | Some queryableType ->
                let elementType = queryableType.GetGenericArguments().[0]
                let subFilter = f.Value
                let subQuery = Expression.Convert(propExpr, queryableType)
                Expression.Call(typeof<Queryable>, "Any", [|elementType|], subQuery) |> whereExpr<'T> query param :> Expression
            | None -> query.Expression

type ObjectListFilter with

    member filter.Apply<'T, 'D>(query : IQueryable<'T>,
                               compareDiscriminator : Expression<Func<'T, 'D, bool>>,
                               getDiscriminatorValue : (Type -> 'D)) =

        // Helper for discriminator comparison
        let buildTypeDiscriminatorCheck (t: Type) =
            match compareDiscriminator, getDiscriminatorValue with
            | null, discValueFn when obj.Equals(discValueFn, null) ->
                // use __typename from filter and do type.ToSting() for values
                ValueNone
            | discExpr, discValueFn when obj.Equals(discValueFn, null) ->
                // use discriminator and do type.ToSting() for values
                ValueNone
            | null, discValueFn ->
                // use __typename from filter and execute discValueFn for values
                ValueNone
            | discExpr, discValueFn ->
                // use discriminator and execute discValueFn for values

                let discriminatorValue = discValueFn t
                let param = Expression.Parameter(typeof<'T>, "x")
                let discExpr = getPropertyExpr<'T> param "__discriminator" // Assuming discriminator field name
                let valueExpr = Expression.Constant(discriminatorValue)
                ValueSome(Expression.Equal(discExpr, valueExpr))

        // Create and execute the final expression
        let param = Expression.Parameter(typeof<'T>, "x")
        query.Provider.CreateQuery<'T>(buildFilterExpr<'T> param buildTypeDiscriminatorCheck filter query)

    member filter.Apply<'T, 'D>(query : IQueryable<'T>,
                               [<Optional>] getDiscriminator : Expression<Func<'T, 'D>> | null,
                               [<Optional>] getDiscriminatorValue : Type -> 'D) =

        // Helper for discriminator comparison
        let buildTypeDiscriminatorCheck (t: Type) =
            match getDiscriminator, getDiscriminatorValue with
            | null, discValueFn when obj.Equals(discValueFn, null) ->
                // use __typename from filter and do type.ToSting() for values
                ValueNone
            | discExpr, discValueFn when obj.Equals(discValueFn, null) ->
                // use discriminator and do type.ToSting() for values
                ValueNone
            | null, discValueFn ->
                // use __typename from filter and execute discValueFn for values
                ValueNone
            | discExpr, discValueFn ->
                // use discriminator and execute discValueFn for values
                let discriminatorValue = discValueFn t
                let param = Expression.Parameter(typeof<'T>, "x")
                let discExpr = getPropertyExpr<'T> param "__discriminator" // Assuming discriminator field name
                let valueExpr = Expression.Constant(discriminatorValue)
                ValueSome(Expression.Equal(discExpr, valueExpr))

        // Create and execute the final expression
        let param = Expression.Parameter(typeof<'T>, "x")
        query.Provider.CreateQuery<'T>(buildFilterExpr<'T> param buildTypeDiscriminatorCheck filter query)
