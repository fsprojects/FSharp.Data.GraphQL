// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Reflection
open FSharp.Reflection
open System.Collections.Generic
open System.Linq
open System.Runtime.CompilerServices
open FSharp.Data.GraphQL.Types
open FSharp.Quotations

type internal Methods =
    interface
        abstract member Type: Type
        abstract member Select : MethodInfo
        abstract member Where : MethodInfo
        abstract member Skip : MethodInfo
        abstract member Take : MethodInfo
        abstract member OrderBy : MethodInfo
        abstract member OrderByDesc : MethodInfo
    end

module internal Gen =

    [<return: Struct>]
    let (|List|_|) (t: Type) =
        let typeParam = t.GetGenericArguments().[0]
        let tList = typedefof<_ list>.MakeGenericType [| typeParam |]
        if t = tList then ValueSome typeParam
        else ValueNone

    [<return: Struct>]
    let (|Array|_|) (t: Type) =
        if t.IsArray then ValueSome (t.GetGenericArguments().[0])
        else ValueNone

    [<return: Struct>]
    let (|Set|_|) (t: Type) =
        let typeParam = t.GetGenericArguments().[0]
        let tArray = typedefof<Set<_>>.MakeGenericType [| typeParam |]
        if t = tArray then ValueSome typeParam
        else ValueNone

    let inline defaultArgOnNull a b = if isNull a |> not then a else b
    let private optionType = typedefof<option<_>>
    [<return: Struct>]
    let (|Option|_|) (t: Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = optionType
        then ValueSome (t.GetGenericArguments().[0])
        else ValueNone

    [<return: Struct>]
    let (|Enumerable|_|) t =
        if typeof<System.Collections.IEnumerable>.IsAssignableFrom t
        then
            let e = defaultArgOnNull (t.GetInterface("IEnumerable`1")) t
            ValueSome (e.GetGenericArguments().[0])
        else ValueNone

    [<return: Struct>]
    let (|Queryable|_|) t =
        if typeof<IQueryable>.IsAssignableFrom t
        then ValueSome (Queryable (t.GetInterface("IQueryable`1").GetGenericArguments().[0]))
        else ValueNone

    let genericType<'t> typeParams = typedefof<'t>.MakeGenericType typeParams

    let genericMethod<'t> methodName typeParams =
        let methods = typeof<'t>.GetMethods().Where(System.Func<MethodInfo,bool>(fun x -> x.Name = methodName))
        methods.First().MakeGenericMethod typeParams

    let private getCallInfo = function
        | Patterns.Call(_, info, _) -> info
        | _ -> failwith "Unexpected Quotation!"

    let listOfSeq =
        let info = getCallInfo <@ List.ofSeq<_> Seq.empty @>
        info.GetGenericMethodDefinition()

    let arrayOfSeq =
        let info = getCallInfo <@ Array.ofSeq<_> Seq.empty @>
        info.GetGenericMethodDefinition()

    let setOfSeq =
        let info = getCallInfo <@ Set.ofSeq<_> Seq.empty @>
        info.GetGenericMethodDefinition()

    let private em = typeof<Enumerable>.GetMethods()
    let private qm = typeof<Queryable>.GetMethods()

    let enumerableMethods = { new Methods with
        member _.Type = typedefof<IEnumerable<_>>
        member _.Select =
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Select"))
            methods.First()
        member _.Where =
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Where"))
            methods.First()
        member _.Skip =
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Skip"))
            methods.First()
        member _.Take =
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Take"))
            methods.First()
        member _.OrderBy =
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderBy"))
            methods.First()
        member _.OrderByDesc =
            let methods = em.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderByDescending"))
            methods.First()
        }

    let queryableMethods = { new Methods with
        member _.Type = typedefof<IQueryable<_>>
        member _.Select =
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Select"))
            methods.First()
        member _.Where =
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Where"))
            methods.First()
        member _.Skip =
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Skip"))
            methods.First()
        member _.Take =
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "Take"))
            methods.First()
        member _.OrderBy =
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderBy"))
            methods.First()
        member _.OrderByDesc =
            let methods = qm.Where(System.Func<MethodInfo,bool>(fun x -> x.Name = "OrderByDescending"))
            methods.First()
        }

module internal ReflectionHelper =

    let private genericEnumerableTypeDefinition = typedefof<IEnumerable<_>>

    let rec isTypeOptional (t: Type) =
        ReflectionHelper.isOptionType t
        || ReflectionHelper.isValueOptionType t
        || (ReflectionHelper.isSkippableType t && isTypeOptional (t.GetGenericArguments().[0]))

    let isParameterOptional (p: ParameterInfo) =
        p.IsOptional || isTypeOptional p.ParameterType

    let isPrameterMandatory = not << isParameterOptional

    let isParameterSkippable (p: ParameterInfo) = ReflectionHelper.isSkippableType p.ParameterType

    let unwrapOptions (ty : Type) =
        if ReflectionHelper.isOptionType ty || ReflectionHelper.isValueOptionType ty then
            ty.GetGenericArguments().[0]
        else ty

    /// <summary>
    /// The type of the items of a collection type: the element type of an array, or the type argument
    /// of the <see cref="T:System.Collections.Generic.IEnumerable`1"/> it is or implements.
    /// A string is a sequence of characters, but not a collection a GraphQL list builds.
    /// </summary>
    let tryGetItemType (collectionType : Type) : Type voption =
        if collectionType.IsArray then
            ValueSome (collectionType.GetElementType ())
        elif Type.(=) (collectionType, typeof<string>) then
            ValueNone
        elif ReflectionHelper.isConstructedFrom genericEnumerableTypeDefinition collectionType then
            ValueSome collectionType.GenericTypeArguments[0]
        else
            collectionType.GetInterfaces ()
            |> Array.vtryFind (ReflectionHelper.isConstructedFrom genericEnumerableTypeDefinition)
            |> ValueOption.map (fun enumerableType -> enumerableType.GenericTypeArguments[0])

    /// Builds a collection through the <c>CreateRange</c> method of the builder type that the
    /// <see cref="T:System.Runtime.CompilerServices.CollectionBuilderAttribute"/> of the collection type names,
    /// as the immutable collections have. The method the attribute names takes a <see cref="T:System.ReadOnlySpan`1"/>,
    /// which reflection cannot pass.
    let private tryCollectionBuilderFactory (collectionType : Type) (itemType : Type) (items : obj list -> obj) =
        match collectionType.GetCustomAttribute<CollectionBuilderAttribute> () with
        | null -> ValueNone
        | attribute ->
            attribute.BuilderType.GetMethods (BindingFlags.Public ||| BindingFlags.Static)
            |> Array.vtryFind (fun method ->
                String.Equals (method.Name, "CreateRange", StringComparison.Ordinal)
                && method.IsGenericMethodDefinition
                && method.GetGenericArguments().Length = 1
                && (let parameters = method.GetParameters ()
                    parameters.Length = 1
                    && ReflectionHelper.isConstructedFrom genericEnumerableTypeDefinition parameters[0].ParameterType))
            |> ValueOption.map (fun method -> method.MakeGenericMethod itemType)
            |> ValueOption.filter (fun method -> collectionType.IsAssignableFrom method.ReturnType)
            |> ValueOption.map (fun method -> fun values -> method.Invoke (null, [| items values |]))

    /// Builds a collection through its public constructor that takes the items as an <see cref="T:System.Collections.Generic.IEnumerable`1"/>.
    let private tryEnumerableConstructorFactory (collectionType : Type) (itemType : Type) (items : obj list -> obj) =
        match collectionType.GetConstructor [| genericEnumerableTypeDefinition.MakeGenericType itemType |] with
        | null -> ValueNone
        | constructor -> ValueSome (fun values -> constructor.Invoke [| items values |])

    /// Builds a collection the way a collection initializer does: through its public parameterless constructor and
    /// an <c>Add</c> method for every item. An <c>Add</c> that returns the collection type, as an immutable collection's does,
    /// leaves the collection unchanged, so only one that returns nothing or a <see cref="T:System.Boolean"/> counts.
    let private tryCollectionInitializerFactory (collectionType : Type) (itemType : Type) =
        match collectionType.GetConstructor Type.EmptyTypes, collectionType.GetMethod ("Add", [| itemType |]) with
        | null, _
        | _, null -> ValueNone
        | _, add when not (Type.(=) (add.ReturnType, typeof<Void>) || Type.(=) (add.ReturnType, typeof<bool>)) -> ValueNone
        | constructor, add ->
            ValueSome (fun (values : obj list) ->
                let collection = constructor.Invoke [||]
                for value in values do
                    add.Invoke (collection, [| value |]) |> ignore
                collection)

    /// <summary>
    /// Creates the function that builds a collection of the type from the coerced items of a GraphQL list.
    /// </summary>
    /// <remarks>
    /// It supports, in this order: arrays; the types an F# list is assignable to, such as
    /// <see cref="T:System.Collections.Generic.IReadOnlyList`1"/>, which get an F# list; interfaces that
    /// <see cref="T:System.Collections.Generic.List`1"/> or <see cref="T:System.Collections.Generic.HashSet`1"/> implement,
    /// such as <see cref="T:System.Collections.Generic.IList`1"/> or <see cref="T:System.Collections.Generic.ISet`1"/>,
    /// which get one of them; types with a collection builder, such as the immutable collections; types with a constructor
    /// that takes an <see cref="T:System.Collections.Generic.IEnumerable`1"/>, such as F# sets; and types with
    /// a collection initializer.
    /// </remarks>
    let tryCreateCollectionFactory (collectionType : Type) : (obj list -> obj) voption =
        match tryGetItemType collectionType with
        | ValueNone -> ValueNone
        | ValueSome itemType ->
            let toArray (values : obj list) = ReflectionHelper.arrayOfList itemType values
            if collectionType.IsArray then
                ValueSome toArray
            elif collectionType.IsAssignableFrom (typedefof<_ list>.MakeGenericType itemType) then
                let cons, nil = ReflectionHelper.listOfType itemType
                ValueSome (fun values -> List.foldBack cons values nil)
            else
                let concreteType =
                    if collectionType.IsInterface || collectionType.IsAbstract then
                        [ typedefof<List<_>>; typedefof<HashSet<_>> ]
                        |> List.map _.MakeGenericType(itemType)
                        |> List.tryFind collectionType.IsAssignableFrom
                        |> Option.defaultValue null
                    else
                        collectionType
                match concreteType with
                | null -> ValueNone
                | concreteType ->
                    tryCollectionBuilderFactory concreteType itemType toArray
                    |> ValueOption.orElseWith (fun () -> tryEnumerableConstructorFactory concreteType itemType toArray)
                    |> ValueOption.orElseWith (fun () -> tryCollectionInitializerFactory concreteType itemType)

    let rec isAssignableWithUnwrap (from: Type) (``to``: Type) =

        // A GraphQL list builds a collection of its own type, which input coercion then copies into
        // a constructor parameter of another collection type that it can build
        let checkCollections (from: Type) (``to``: Type) =
            match tryGetItemType from, tryGetItemType ``to`` with
            | ValueSome fromItemType, ValueSome toItemType ->
                fromItemType.IsAssignableTo toItemType
                && (tryCreateCollectionFactory ``to``).IsSome
            | _ -> false

        let actualFrom = unwrapOptions from
        let actualTo =
            if ReflectionHelper.isOptionType ``to`` ||
               ReflectionHelper.isValueOptionType ``to`` ||
               ReflectionHelper.isSkippableType ``to``
            then
                ``to``.GetGenericArguments()[0]
            else ``to``

        let result = actualFrom.IsAssignableTo actualTo || checkCollections actualFrom actualTo
        if result then true
        elif actualFrom <> from || actualTo <> ``to`` then isAssignableWithUnwrap actualFrom actualTo
        else false

    let matchConstructor (t: Type) (fields: string []) =
        if FSharpType.IsRecord(t, true) then FSharpValue.PreComputeRecordConstructorInfo(t, true)
        else
            let constructors = t.GetConstructors(BindingFlags.NonPublic|||BindingFlags.Public|||BindingFlags.Instance)
            let inputFieldNames =
                fields
                |> Set.ofArray

            let constructorsWithParameters =
                constructors
                |> Seq.map (fun ctor -> struct(ctor, ctor.GetParameters()))
                // start from most complete constructors
                |> Seq.sortBy (fun struct(_, parameters) -> -parameters.Length)

            let getMandatoryParammeters = Seq.where isPrameterMandatory

            let struct(ctor, _) =
                seq {
                    // match all constructors with all parameters
                    yield! constructorsWithParameters |> Seq.map (fun struct(ctor, parameters) -> struct(ctor, parameters |> Seq.map (fun p -> p.Name)))
                    // match all constructors with non optional parameters
                    yield! constructorsWithParameters |> Seq.map (fun struct(ctor, parameters) -> struct(ctor, parameters |> getMandatoryParammeters |> Seq.map (fun p -> p.Name)))
                }
                // try match field with params by name
                // at last, default constructor should be used if defined
                |> Seq.find (fun struct(_, ctorParamsNames) -> Set.isSubset (Set.ofSeq ctorParamsNames) inputFieldNames)
            ctor

    let parseUnion (t: Type) (u: string) =
        if t.IsEnum then Enum.Parse(t, u, ignoreCase = true)
        else
            try
                match FSharpType.GetUnionCases(t, (BindingFlags.NonPublic ||| BindingFlags.Public))|> Array.filter(fun case -> case.Name.ToLower() = u.ToLower()) with
                | [|case|] -> FSharpValue.MakeUnion(case, [||], (BindingFlags.NonPublic ||| BindingFlags.Public))
                | _ -> null
            with _ -> null

