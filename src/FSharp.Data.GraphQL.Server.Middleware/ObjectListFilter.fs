namespace FSharp.Data.GraphQL.Server.Middleware

open System

/// A filter definition for a field value.
type FieldFilter<'Val> = { FieldName : string; Value : 'Val }

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
    | OfTypes of Type list
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

open System.Linq
open System.Linq.Expressions
open System.Runtime.InteropServices
open System.Reflection

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

    let private genericWhereMethod =
        typeof<Queryable>.GetMethods ()
        |> Seq.where (fun m -> m.Name = "Where")
        |> Seq.find (fun m ->
            let parameters = m.GetParameters ()
            parameters.Length = 2
            && parameters[1].ParameterType.GetGenericTypeDefinition () = typedefof<Expression<Func<_, _>>>)

    // Helper to create Where expression
    let whereExpr<'T> (query : IQueryable<'T>) (param : ParameterExpression) predicate =
        let whereMethod = genericWhereMethod.MakeGenericMethod ([| typeof<'T> |])
        Expression.Call (whereMethod, [| query.Expression; Expression.Lambda<Func<'T, bool>> (predicate, param) |])

    let private StringStartsWithMethod = typeof<string>.GetMethod ("StartsWith", [| typeof<string> |])
    let private StringEndsWithMethod = typeof<string>.GetMethod ("EndsWith", [| typeof<string> |])
    let private StringContainsMethod = typeof<string>.GetMethod ("Contains", [| typeof<string> |])

    let getField (param : ParameterExpression) fieldName = Expression.PropertyOrField (param, fieldName)

    [<Struct>]
    type SourceExpression private (expression : Expression) =
        new (parameter : ParameterExpression) = SourceExpression (parameter :> Expression)
        new (``member`` : MemberExpression) = SourceExpression (``member`` :> Expression)
        member _.Value = expression
        static member op_Implicit (source : SourceExpression) = source.Value
        static member op_Implicit (parameter : ParameterExpression) = SourceExpression (parameter :> Expression)
        static member op_Implicit (``member`` : MemberExpression) = SourceExpression (``member`` :> Expression)

    let rec buildFilterExpr (param : SourceExpression) buildTypeDiscriminatorCheck filter : Expression =
        let build = buildFilterExpr param buildTypeDiscriminatorCheck
        match filter with
        | NoFilter -> Expression.Constant (true)
        | Not f -> f |> build |> Expression.Not :> Expression
        | And (f1, f2) -> Expression.AndAlso (build f1, build f2)
        | Or (f1, f2) -> Expression.OrElse (build f1, build f2)
        | Equals f -> Expression.Equal (Expression.PropertyOrField (param, f.FieldName), Expression.Constant (f.Value))
        | GreaterThan f -> Expression.GreaterThan (Expression.PropertyOrField (param, f.FieldName), Expression.Constant (f.Value))
        | LessThan f -> Expression.LessThan (Expression.PropertyOrField (param, f.FieldName), Expression.Constant (f.Value))
        | StartsWith f ->
            Expression.Call (Expression.PropertyOrField (param, f.FieldName), StringStartsWithMethod, Expression.Constant (f.Value))
        | EndsWith f ->
            Expression.Call (Expression.PropertyOrField (param, f.FieldName), StringEndsWithMethod, Expression.Constant (f.Value))
        | Contains f ->
            Expression.Call (Expression.PropertyOrField (param, f.FieldName), StringContainsMethod, Expression.Constant (f.Value))
        | OfTypes types ->
            types
            |> Seq.map (fun t -> buildTypeDiscriminatorCheck param t)
            |> Seq.reduce (fun acc expr -> Expression.Or (acc, expr))
        | FilterField f ->
            let paramExpr = Expression.PropertyOrField (param, f.FieldName)
            buildFilterExpr (SourceExpression paramExpr) buildTypeDiscriminatorCheck f.Value

    [<Struct>]
    type ObjectListFilterLinqOptions<'T, 'D> (
        [<Optional>] compareDiscriminator : Expression<Func<'T, 'D, bool>> | null,
        [<Optional>] getDiscriminatorValue : (Type -> 'D) | null,
        [<Optional>] serializeMemberName : (MemberInfo -> string) | null) =

        member _.CompareDiscriminator = compareDiscriminator |> ValueOption.ofObj
        member _.GetDiscriminatorValue = getDiscriminatorValue |> ValueOption.ofObj
        //member _.SerializeMemberName = serializeMemberName |> ValueOption.ofObj

        static member None = ObjectListFilterLinqOptions<'T, 'D> (null, null, null)

        static member GetCompareDiscriminator (getDiscriminatorValue : Expression<Func<'T, 'D>>) =
            let tParam = Expression.Parameter (typeof<'T>, "x")
            let dParam = Expression.Parameter (typeof<'D>, "d")
            let body = Expression.Equal(Expression.Invoke(getDiscriminatorValue, tParam), dParam)
            Expression.Lambda<Func<'T, 'D, bool>> (body, tParam, dParam)

        new (getDiscriminator : Expression<Func<'T, 'D>>) = ObjectListFilterLinqOptions<'T, 'D> (ObjectListFilterLinqOptions.GetCompareDiscriminator getDiscriminator, null, null)
        new (compareDiscriminator : Expression<Func<'T, 'D, bool>>) = ObjectListFilterLinqOptions<'T, 'D> (compareDiscriminator, null, null)
        new (getDiscriminatorValue : Type -> 'D) = ObjectListFilterLinqOptions<'T, 'D> (null, getDiscriminatorValue, null)
        //new (serializeMemberName : MemberInfo -> string) = ObjectListFilterLinqOptions<'T, 'D> (null, null, serializeMemberName)

        new (getDiscriminator : Expression<Func<'T, 'D>>, getDiscriminatorValue : Type -> 'D) = ObjectListFilterLinqOptions<'T, 'D> (ObjectListFilterLinqOptions.GetCompareDiscriminator getDiscriminator, getDiscriminatorValue, null)

        //new (getDiscriminator : Expression<Func<'T, 'D>>, serializeMemberName : MemberInfo -> string) = ObjectListFilterLinqOptions<'T, 'D> (ObjectListFilterLinqOptions.GetCompareDiscriminator getDiscriminator, null, serializeMemberName)
        //new (compareDiscriminator : Expression<Func<'T, 'D, bool>>, serializeMemberName : MemberInfo -> string) = ObjectListFilterLinqOptions<'T, 'D> (compareDiscriminator, null, serializeMemberName)
        //new (getDiscriminatorValue : Type -> 'D, serializeMemberName : MemberInfo -> string) = ObjectListFilterLinqOptions<'T, 'D> (null, getDiscriminatorValue, serializeMemberName)

type ObjectListFilter with

    member filter.Apply<'T, 'D> (query : IQueryable<'T>, [<Optional>] options : ObjectListFilterLinqOptions<'T, 'D>) =

        match filter with
        | NoFilter -> query
        | _ ->
            // Helper for discriminator comparison
            let buildTypeDiscriminatorCheck (param : SourceExpression) (t : Type) =
                match options.CompareDiscriminator, options.GetDiscriminatorValue with
                | ValueNone, ValueNone ->
                    // use __typename from filter and do type.ToSting() for values
                    let typename = t.FullName
                    Expression.Equal(Expression.PropertyOrField(param, "__typename"), Expression.Constant(typename)) :> Expression
                | ValueSome discExpr, ValueNone ->
                    // use discriminator and do type.ToSting() for values
                    let typename = t.FullName
                    Expression.Invoke(discExpr, param, Expression.Constant(typename)) :> Expression
                | ValueNone, ValueSome discValueFn ->
                    // use __typename from filter and execute discValueFn for values
                    let discriminatorValue = discValueFn t
                    Expression.Equal(Expression.PropertyOrField(param, "__typename"), Expression.Constant(discriminatorValue)) :> Expression
                | ValueSome discExpr, ValueSome discValueFn ->
                    // use discriminator and execute discValueFn for values
                    let discriminatorValue = discValueFn t
                    Expression.Invoke(discExpr, param, Expression.Constant (discriminatorValue))

            let queryExpr =
                let param = Expression.Parameter (typeof<'T>, "x")
                let body = buildFilterExpr (SourceExpression param) buildTypeDiscriminatorCheck filter
                whereExpr<'T> query param body
            // Create and execute the final expression
            query.Provider.CreateQuery<'T> (queryExpr)
