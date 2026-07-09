[<Xunit.Trait (Tests.TraitType.Category, Tests.TraitName.ObjectListFilter)>]
[<Xunit.Trait (Tests.TraitType.ObjectListFilterOperator, "in")>]
module FSharp.Data.GraphQL.Tests.ObjectListFilter.TypeCoercion.InOperatorTests

open System
open System.Linq
open System.Linq.Expressions
open Xunit
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Server.Middleware
open FSharp.Data.GraphQL.Tests.ObjectListFilter.TypeCoercion.Common

// ──────────────────────────────────────────────────────────────────────────────
// In operator test cases
// Covers all coercion types: CLR enum, DU-as-enum, Guid, single-case DU, and primitives
// ──────────────────────────────────────────────────────────────────────────────

/// Minimal entity model used to validate `In` translation for nullable fields.
type NullableEntity = {
    /// Primary identifier used in test assertions.
    Id: int
    /// Nullable scalar field used to assert typed `Contains<Nullable<int>>` expression generation.
    MaybeId: Nullable<int>
}

/// Query options for nullable-expression tests with JSON coercion enabled.
let private nullableOptions =
    ObjectListFilterLinqOptions<NullableEntity, obj> (Json.getSerializerOptions Seq.empty)

/// In-memory source for nullable-field `In` tests.
let private nullableData =
    [|
        { Id = 1; MaybeId = Nullable 1 }
        { Id = 2; MaybeId = Nullable() }
        { Id = 3; MaybeId = Nullable 3 }
    |]

/// Applies an `ObjectListFilter` to the nullable-field test dataset.
let private applyNullableFilter (filter : ObjectListFilter) =
    filter.ApplyTo (nullableData.AsQueryable (), nullableOptions) |> Seq.toList

/// Traverses an expression tree and returns the first `Enumerable.Contains` call node.
let private tryFindEnumerableContainsCall (expr : Expression) : MethodCallExpression option =
    let rec find (node : Expression) =
        match node with
        | :? MethodCallExpression as call
            when call.Method.Name = "Contains"
                 && call.Method.DeclaringType = typeof<Enumerable>
                 && call.Arguments.Count = 2 ->
            Some call
        | :? MethodCallExpression as call ->
            let fromObject =
                if isNull call.Object then None else find call.Object
            match fromObject with
            | Some found -> Some found
            | None -> call.Arguments |> Seq.tryPick find
        | :? UnaryExpression as unary -> find unary.Operand
        | :? LambdaExpression as lambda -> find lambda.Body
        | :? BinaryExpression as binary ->
            match find binary.Left with
            | Some found -> Some found
            | None -> find binary.Right
        | :? MemberExpression as memberExpr ->
            if isNull memberExpr.Expression then None else find memberExpr.Expression
        | _ -> None
    find expr

/// Builds a filtered query and extracts the generated `Enumerable.Contains` call from its expression tree.
let private findEnumerableContainsCall<'T, 'D> (options : ObjectListFilterLinqOptions<'T, 'D>) (filter : ObjectListFilter) =
    let query = Enumerable.Empty<'T>().AsQueryable()
    let result = query.Apply (filter, options)
    match tryFindEnumerableContainsCall result.Expression with
    | Some call -> call
    | None ->
        fail "Expected to find Enumerable.Contains call in generated expression tree"
        Unchecked.defaultof<MethodCallExpression>

/// Asserts that `In` is translated to a strongly typed `Enumerable.Contains<TField>` expression.
/// Validates method generic argument, typed values container, and non-boxed member argument.
let private assertTypedInExpression<'T, 'D>
    (options : ObjectListFilterLinqOptions<'T, 'D>)
    (filter : ObjectListFilter)
    (expectedElementType : Type)
    =
    let containsCall = findEnumerableContainsCall options filter
    containsCall.Method.GetGenericArguments().[0] |> equals expectedElementType

    let valuesArgType = containsCall.Arguments.[0].Type
    if valuesArgType = typeof<obj> || valuesArgType = typeof<obj list> then
        fail $"Expected a strongly typed values argument, but got {valuesArgType.FullName}"

    let actualElementType =
        if valuesArgType.IsArray then
            valuesArgType.GetElementType ()
        elif valuesArgType.IsGenericType then
            valuesArgType.GetGenericArguments().[0]
        else
            fail $"Expected array or generic collection values argument, got {valuesArgType.FullName}"
            Unchecked.defaultof<Type>

    actualElementType |> equals expectedElementType

    match containsCall.Arguments.[1] with
    | :? UnaryExpression as unary when unary.NodeType = ExpressionType.Convert && unary.Type = typeof<obj> ->
        fail "Expected In member argument to remain strongly typed without boxing to object"
    | _ -> ()

[<Fact>]
let ``In operator coerces string primitives`` () =
    let filter = In { FieldName = "name"; Value = [ box "Alice"; box "Bob" ] }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Bob" ]

[<Fact>]
let ``In operator coerces int primitives`` () =
    let filter = In { FieldName = "id"; Value = [ box 1; box 3 ] }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Id) |> List.sort |> equals [ 1; 3 ]

[<Fact>]
let ``In operator coerces CLR enum`` () =
    let filter = In { FieldName = "color"; Value = [ box "Red"; box "Blue" ] }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Charlie" ]

[<Fact>]
let ``In operator coerces DU-as-enum`` () =
    let filter = In { FieldName = "status"; Value = [ box "Active"; box "Pending" ] }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Charlie" ]

[<Fact>]
let ``In operator coerces Guid`` () =
    let filter =
        In {
            FieldName = "guidField"
            Value = [
                box "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
                box "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
            ]
        }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Bob" ]

[<Fact>]
let ``In operator coerces single-case DU wrapping string`` () =
    let filter = In { FieldName = "wrappedName"; Value = [ box "Alice"; box "Charlie" ] }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Charlie" ]

[<Fact>]
let ``In operator coerces single-case DU wrapping int`` () =
    let filter = In { FieldName = "wrappedScore"; Value = [ box 10; box 30 ] }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Charlie" ]

[<Fact>]
let ``In operator coerces single-case DU wrapping Guid`` () =
    let filter =
        In {
            FieldName = "wrappedGuid"
            Value = [ box "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"; box "cccccccc-cccc-cccc-cccc-cccccccccccc" ]
        }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Charlie" ]

[<Fact>]
let ``In operator coerces Nullable int primitives`` () =
    let filter = In { FieldName = "maybeId"; Value = [ box 1; box 3 ] }
    let result = applyNullableFilter filter
    result |> List.map (fun e -> e.Id) |> List.sort |> equals [ 1; 3 ]

[<Fact>]
let ``In operator expression uses typed contains for string primitive field`` () =
    let filter = In { FieldName = "name"; Value = [ box "Alice"; box "Bob" ] }
    assertTypedInExpression filterOptions filter typeof<string>

[<Fact>]
let ``In operator expression uses typed contains for int primitive field`` () =
    let filter = In { FieldName = "id"; Value = [ box 1; box 3 ] }
    assertTypedInExpression filterOptions filter typeof<int>

[<Fact>]
let ``In operator expression uses typed contains for CLR enum field`` () =
    let filter = In { FieldName = "color"; Value = [ box "Red"; box "Blue" ] }
    assertTypedInExpression filterOptions filter typeof<Color>

[<Fact>]
let ``In operator expression uses typed contains for fieldless DU field`` () =
    let filter = In { FieldName = "status"; Value = [ box "Active"; box "Pending" ] }
    assertTypedInExpression filterOptions filter typeof<Status>

[<Fact>]
let ``In operator expression uses typed contains for Guid field`` () =
    let filter = In { FieldName = "guidField"; Value = [ box "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" ] }
    assertTypedInExpression filterOptions filter typeof<Guid>

[<Fact>]
let ``In operator expression uses typed contains for single-case DU string field`` () =
    let filter = In { FieldName = "wrappedName"; Value = [ box "Alice"; box "Charlie" ] }
    assertTypedInExpression filterOptions filter typeof<WrappedString>

[<Fact>]
let ``In operator expression uses typed contains for single-case DU int field`` () =
    let filter = In { FieldName = "wrappedScore"; Value = [ box 10; box 30 ] }
    assertTypedInExpression filterOptions filter typeof<WrappedInt>

[<Fact>]
let ``In operator expression uses typed contains for single-case DU Guid field`` () =
    let filter = In { FieldName = "wrappedGuid"; Value = [ box "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" ] }
    assertTypedInExpression filterOptions filter typeof<WrappedGuid>

[<Fact>]
let ``In operator expression uses typed contains for option field`` () =
    let filter = In { FieldName = "optionName"; Value = [ box "Alice"; box "Charlie" ] }
    assertTypedInExpression filterOptions filter typeof<string option>

[<Fact>]
let ``In operator expression uses typed contains for voption field`` () =
    let filter = In { FieldName = "vOptionId"; Value = [ box 1; box 3 ] }
    assertTypedInExpression filterOptions filter typeof<int voption>

[<Fact>]
let ``In operator expression uses typed contains for Nullable field`` () =
    let filter = In { FieldName = "maybeId"; Value = [ box 1; box 3 ] }
    assertTypedInExpression nullableOptions filter typeof<Nullable<int>>

