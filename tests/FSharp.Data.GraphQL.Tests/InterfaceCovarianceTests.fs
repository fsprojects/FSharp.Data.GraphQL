// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.InterfaceCovarianceTests

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Validation
open FSharp.Data.GraphQL.Validation.Types
open FSharp.Data.GraphQL.Types
open Helpers
open Xunit

type IChildView =
    interface
        abstract Id : string
    end

type IParentView =
    interface
        abstract Child : IChildView
    end

type ChildAView = {
    Id : string
} with

    interface IChildView with
        member x.Id = x.Id

type ChildBView = {
    Id : string
} with

    interface IChildView with
        member x.Id = x.Id

type OtherView = { Name : string }

type ParentAInfoView = {
    Child : ChildAView
} with

    interface IParentView with
        member x.Child = x.Child :> IChildView

type ParentBInfoView = {
    Child : ChildBView
} with

    interface IParentView with
        member x.Child = x.Child :> IChildView

type ParentOtherInfoView = { Child : OtherView }

type ParentInterfaceChildView = {
    Child : IChildView
} with

    interface IParentView with
        member x.Child = x.Child

type ParentChildOptionView = { Child : ChildAView option }

type ParentChildVOptionView = { Child : ChildAView voption }

type ParentChildListView = { Child : ChildAView list }

type ParentChildOptionListView = { Child : ChildAView option list }

type ParentChildVOptionListView = { Child : ChildAView voption list }

let IChildInfo : InterfaceDef<IChildView> =
    Define.Interface<IChildView> (name = "IChildInfo", fields = [ Define.Field ("id", StringType) ])

let ChildAInfo : ObjectDef<ChildAView> =
    Define.Object<ChildAView> (
        name = "ChildAInfo",
        fields = [ Define.Field ("id", StringType, (fun _ (x : ChildAView) -> x.Id)) ],
        interfaces = [ IChildInfo ]
    )

let ChildBInfo : ObjectDef<ChildBView> =
    Define.Object<ChildBView> (
        name = "ChildBInfo",
        fields = [ Define.Field ("id", StringType, (fun _ (x : ChildBView) -> x.Id)) ],
        interfaces = [ IChildInfo ]
    )

let OtherInfo : ObjectDef<OtherView> =
    Define.Object<OtherView> (name = "OtherInfo", fields = [ Define.Field ("name", StringType, (fun _ x -> x.Name)) ])

let signatureMismatch objectName fieldName interfaceName =
    $"'{objectName}.{fieldName}' field signature does not match it's definition in interface '{interfaceName}'"

let hasValidationErrorContaining (text : string) result =
    match result with
    | ValidationError errors ->
        Assert.True (
            (errors : string list)
            |> List.exists (fun (x : string) -> x.Contains (text)),
            $"Expected validation error containing '{text}', but got {errors}"
        )
    | Success -> Assert.Fail ($"Expected validation error containing '{text}', but validation succeeded")

[<Fact>]
let ``Validation allows interface field covariance with ChildAInfo`` () =
    let parentInterface =
        Define.Interface<obj> (name = "IParentInfo", fields = [ Define.Field ("child", IChildInfo) ])

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [ Define.Field ("child", ChildAInfo, (fun _ _ -> Unchecked.defaultof<ChildAView>)) ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    equals Success result

[<Fact>]
let ``Validation allows interface field covariance with ChildBInfo`` () =
    let parentInterface =
        Define.Interface<obj> (name = "IParentInfo", fields = [ Define.Field ("child", IChildInfo) ])

    let parentObject =
        Define.Object<obj> (
            name = "ParentBInfo",
            fields = [ Define.Field ("child", ChildBInfo, (fun _ _ -> Unchecked.defaultof<ChildBView>)) ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    equals Success result

[<Fact>]
let ``Validation allows nullable interface field with non-null ChildAInfo`` () =
    let parentInterface =
        Define.Interface<obj> (name = "IParentInfo", fields = [ Define.Field ("child", Nullable IChildInfo) ])

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [ Define.Field ("child", ChildAInfo, (fun _ _ -> Unchecked.defaultof<ChildAView>)) ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    equals Success result

[<Fact>]
let ``Validation allows struct nullable interface field with non-null ChildAInfo`` () =
    let parentInterface =
        Define.Interface<obj> (name = "IParentInfo", fields = [ Define.Field ("child", StructNullable IChildInfo) ])

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [ Define.Field ("child", ChildAInfo, (fun _ _ -> Unchecked.defaultof<ChildAView>)) ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    equals Success result

[<Fact>]
let ``Validation allows exact non-null interface field type`` () =
    let parentInterface =
        Define.Interface<obj> (name = "IParentInfo", fields = [ Define.Field ("child", IChildInfo) ])

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [ Define.Field ("child", IChildInfo, (fun _ _ -> Unchecked.defaultof<IChildView>)) ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    equals Success result

[<Fact>]
let ``Validation allows list covariance from IChildInfo to ChildAInfo`` () =
    let parentInterface =
        Define.Interface<obj> (name = "IParentInfo", fields = [ Define.Field ("child", ListOf IChildInfo) ])

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [ Define.Field ("child", ListOf ChildAInfo, (fun _ _ -> Unchecked.defaultof<ChildAView list>)) ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    equals Success result

[<Fact>]
let ``Validation allows deep wrapper covariance from Nullable(List(Nullable(IChildInfo))) to List(ChildAInfo)`` () =
    let parentInterface =
        Define.Interface<obj> (name = "IParentInfo", fields = [ Define.Field ("child", Nullable (ListOf (Nullable IChildInfo))) ])

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [ Define.Field ("child", ListOf ChildAInfo, (fun _ _ -> Unchecked.defaultof<ChildAView list>)) ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    equals Success result

[<Fact>]
let ``Validation rejects unrelated object type for interface field`` () =
    let parentInterface =
        Define.Interface<obj> (name = "IParentInfo", fields = [ Define.Field ("child", IChildInfo) ])

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [ Define.Field ("child", OtherInfo, (fun _ _ -> Unchecked.defaultof<OtherView>)) ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    hasValidationErrorContaining (signatureMismatch "ParentAInfo" "child" "IParentInfo") result

[<Fact>]
let ``Validation rejects nullable object field when interface field is non-null`` () =
    let parentInterface =
        Define.Interface<obj> (name = "IParentInfo", fields = [ Define.Field ("child", IChildInfo) ])

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [ Define.Field ("child", Nullable ChildAInfo, (fun _ _ -> Unchecked.defaultof<ChildAView option>)) ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    hasValidationErrorContaining (signatureMismatch "ParentAInfo" "child" "IParentInfo") result

[<Fact>]
let ``Validation rejects struct nullable object field when interface field is non-null`` () =
    let parentInterface =
        Define.Interface<obj> (name = "IParentInfo", fields = [ Define.Field ("child", IChildInfo) ])

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [ Define.Field ("child", StructNullable ChildAInfo, (fun _ _ -> Unchecked.defaultof<ChildAView voption>)) ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    hasValidationErrorContaining (signatureMismatch "ParentAInfo" "child" "IParentInfo") result

[<Fact>]
let ``Validation rejects scalar type when interface expects list`` () =
    let parentInterface =
        Define.Interface<obj> (name = "IParentInfo", fields = [ Define.Field ("child", ListOf IChildInfo) ])

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [ Define.Field ("child", ChildAInfo, (fun _ _ -> Unchecked.defaultof<ChildAView>)) ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    hasValidationErrorContaining (signatureMismatch "ParentAInfo" "child" "IParentInfo") result

[<Fact>]
let ``Validation rejects list item nullability widening with Nullable`` () =
    let parentInterface =
        Define.Interface<obj> (name = "IParentInfo", fields = [ Define.Field ("child", ListOf IChildInfo) ])

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [ Define.Field ("child", ListOf (Nullable ChildAInfo), (fun _ _ -> Unchecked.defaultof<ChildAView option list>)) ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    hasValidationErrorContaining (signatureMismatch "ParentAInfo" "child" "IParentInfo") result

[<Fact>]
let ``Validation rejects list item nullability widening with StructNullable`` () =
    let parentInterface =
        Define.Interface<obj> (name = "IParentInfo", fields = [ Define.Field ("child", ListOf IChildInfo) ])

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [ Define.Field ("child", ListOf (StructNullable ChildAInfo), (fun _ _ -> Unchecked.defaultof<ChildAView voption list>)) ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    hasValidationErrorContaining (signatureMismatch "ParentAInfo" "child" "IParentInfo") result

[<Fact>]
let ``Validation rejects field arguments when required interface argument is missing`` () =
    let parentInterface =
        Define.Interface<obj> (
            name = "IParentInfo",
            fields = [
                Define.Field ("child", IChildInfo, "Child field", [ Define.Input ("id", IntType) ], (fun _ _ -> Unchecked.defaultof<IChildView>))
            ]
        )

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [ Define.Field ("child", ChildAInfo, "Child field", [], (fun _ _ -> Unchecked.defaultof<ChildAView>)) ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    hasValidationErrorContaining (signatureMismatch "ParentAInfo" "child" "IParentInfo") result

[<Fact>]
let ``Validation rejects field arguments when argument type differs`` () =
    let parentInterface =
        Define.Interface<obj> (
            name = "IParentInfo",
            fields = [
                Define.Field ("child", IChildInfo, "Child field", [ Define.Input ("id", IntType) ], (fun _ _ -> Unchecked.defaultof<IChildView>))
            ]
        )

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [
                Define.Field (
                    "child",
                    ChildAInfo,
                    "Child field",
                    [ Define.Input ("id", StringType) ],
                    (fun _ _ -> Unchecked.defaultof<ChildAView>)
                )
            ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    hasValidationErrorContaining (signatureMismatch "ParentAInfo" "child" "IParentInfo") result

[<Fact>]
let ``Validation rejects field arguments when object adds extra required argument`` () =
    let parentInterface =
        Define.Interface<obj> (
            name = "IParentInfo",
            fields = [
                Define.Field ("child", IChildInfo, "Child field", [ Define.Input ("id", IntType) ], (fun _ _ -> Unchecked.defaultof<IChildView>))
            ]
        )

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [
                Define.Field (
                    "child",
                    ChildAInfo,
                    "Child field",
                    [ Define.Input ("id", IntType); Define.Input ("extra", IntType) ],
                    (fun _ _ -> Unchecked.defaultof<ChildAView>)
                )
            ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    hasValidationErrorContaining (signatureMismatch "ParentAInfo" "child" "IParentInfo") result

[<Fact>]
let ``Validation allows field arguments when object adds extra optional argument`` () =
    let parentInterface =
        Define.Interface<obj> (
            name = "IParentInfo",
            fields = [
                Define.Field ("child", IChildInfo, "Child field", [ Define.Input ("id", IntType) ], (fun _ _ -> Unchecked.defaultof<IChildView>))
            ]
        )

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [
                Define.Field (
                    "child",
                    ChildAInfo,
                    "Child field",
                    [ Define.Input ("id", IntType); Define.Input ("extra", Nullable IntType) ],
                    (fun _ _ -> Unchecked.defaultof<ChildAView>)
                )
            ],
            interfaces = [ parentInterface ]
        )

    let result = validateImplements parentObject parentInterface
    equals Success result

[<Fact>]
let ``Validation type map allows covariance for concrete implementation of interface field`` () =
    let parentInterface =
        Define.Interface<obj> (name = "IParentInfo", fields = [ Define.Field ("child", IChildInfo) ])

    let parentObject =
        Define.Object<obj> (
            name = "ParentAInfo",
            fields = [ Define.Field ("child", ChildAInfo, (fun _ _ -> Unchecked.defaultof<ChildAView>)) ],
            interfaces = [ parentInterface ]
        )

    let typeMap = TypeMap ()
    typeMap.AddType (parentObject)

    let result = validateTypeMap typeMap
    equals Success result
