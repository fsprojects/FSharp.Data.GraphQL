// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Validation

open System
open System.Collections.Generic
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Extensions
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Validation.ValidationResult
open FsToolkit.ErrorHandling

module Types =

    let private asOutputDef (tdef : TypeDef) =
        match tdef with
        | :? OutputDef as output -> ValueSome output
        | _ -> ValueNone

    let private isOptionalInputField (field : InputFieldDef) =
        match field.TypeDef with
        | Nullable _ -> true
        | _ -> field.DefaultValue.IsSome || field.IsSkippable

    let private areFieldArgumentsCompatible (objArgs : InputFieldDef[]) (ifaceArgs : InputFieldDef[]) =
        let objectArguments = objArgs |> Array.map (fun arg -> arg.Name, arg) |> Map.ofArray
        let interfaceArguments = ifaceArgs |> Array.map (fun arg -> arg.Name, arg) |> Map.ofArray

        let hasCompatibleInterfaceArguments =
            ifaceArgs
            |> Array.forall (fun ifaceArg ->
                match Map.tryFind ifaceArg.Name objectArguments with
                | Some objArg -> objArg.TypeDef = ifaceArg.TypeDef
                | None -> false)

        let hasOptionalExtraObjectArguments =
            objArgs
            |> Array.forall (fun objArg ->
                match Map.tryFind objArg.Name interfaceArguments with
                | Some ifaceArg -> objArg.TypeDef = ifaceArg.TypeDef
                | None -> isOptionalInputField objArg)

        hasCompatibleInterfaceArguments && hasOptionalExtraObjectArguments

    let rec private isOutputSubtype (objType : OutputDef) (ifaceType : OutputDef) =
        match objType, ifaceType with
        | Nullable objInner, Nullable ifaceInner ->
            match asOutputDef objInner, asOutputDef ifaceInner with
            | ValueSome objOutput, ValueSome ifaceOutput -> isOutputSubtype objOutput ifaceOutput
            | _ -> false
        | Nullable _, _ -> false
        | _, Nullable ifaceInner ->
            match asOutputDef ifaceInner with
            | ValueSome ifaceOutput -> isOutputSubtype objType ifaceOutput
            | _ -> false
        | List objInner, List ifaceInner ->
            match asOutputDef objInner, asOutputDef ifaceInner with
            | ValueSome objOutput, ValueSome ifaceOutput -> isOutputSubtype objOutput ifaceOutput
            | _ -> false
        | List _, _
        | _, List _ -> false
        | _ when objType = ifaceType -> true
        | (:? ObjectDef as objObject), (:? InterfaceDef as ifaceInterface) ->
            objObject.Implements |> Array.exists ((=) ifaceInterface)
        | (:? ObjectDef as objObject), (:? UnionDef as ifaceUnion) ->
            ifaceUnion.Options |> Array.exists ((=) objObject)
        | _ -> false

    let private isFieldImplementationCompatible (objField : FieldDef) (ifaceField : FieldDef) =
        objField.Name = ifaceField.Name
        && areFieldArgumentsCompatible objField.Args ifaceField.Args
        && isOutputSubtype objField.TypeDef ifaceField.TypeDef

    let validateImplements (objdef : ObjectDef) (idef : InterfaceDef) =
        let objectFields = objdef.Fields
        let errors =
            idef.Fields
            |> Array.fold
                (fun acc f ->
                    match Map.tryFind f.Name objectFields with
                    | None ->
                        $"'%s{f.Name}' field is defined by interface '%s{idef.Name}', but not implemented in object '%s{objdef.Name}'"
                        :: acc
                    | Some objf when isFieldImplementationCompatible objf f -> acc
                    | Some _ ->
                        $"'%s{objdef.Name}.%s{f.Name}' field signature does not match it's definition in interface '%s{idef.Name}'"
                        :: acc)
                []
        match errors with
        | [] -> Success
        | err -> ValidationError err

    let validateType typedef =
        match typedef with
        | Scalar _ -> Success
        | Object objdef ->
            let nonEmptyResult =
                if objdef.Fields.Count > 0 then
                    Success
                else
                    ValidationError [ $"'%s{objdef.Name}' must have at least one field defined" ]
            let implementsResult =
                objdef.Implements
                |> ValidationResult.collect (validateImplements objdef)
            nonEmptyResult @@ implementsResult
        | InputObject indef ->
            let nonEmptyResult =
                if indef.Fields.Length > 0 then
                    Success
                else
                    ValidationError [ $"'%s{indef.Name}' must have at least one field defined" ]
            nonEmptyResult
        | Union uniondef ->
            let nonEmptyResult =
                if uniondef.Options.Length > 0 then
                    Success
                else
                    ValidationError [
                        $"'%s{uniondef.Name}' must have at least one type definition option"
                    ]
            nonEmptyResult
        | Enum enumdef ->
            let nonEmptyResult =
                if enumdef.Options.Length > 0 then
                    Success
                else
                    ValidationError [ $"'%s{enumdef.Name}' must have at least one enum value defined" ]
            nonEmptyResult
        | Interface idef ->
            let nonEmptyResult =
                if idef.Fields.Length > 0 then
                    Success
                else
                    ValidationError [ $"'%s{idef.Name}' must have at least one field defined" ]
            nonEmptyResult
        | InputCustom _ -> Success
        | _ -> failwithf "Unexpected value of typedef: %O" typedef

    let validateTypeMap (namedTypes : TypeMap) : ValidationResult<string> =
        namedTypes.ToSeq ()
        |> Seq.fold (fun acc (_, namedDef) -> acc @@ validateType namedDef) Success

module Ast =

    type MetaTypeFieldInfo = { Name : string; ArgumentNames : string[] }

    let private metaTypeFields =
        seq {
            { Name = "__type"; ArgumentNames = [| "name" |] }
            { Name = "__schema"; ArgumentNames = [||] }
            { Name = "__typename"; ArgumentNames = [||] }
        }
        |> Seq.map (fun x -> x.Name, x)
        |> Map.ofSeq

    let rec private tryGetSchemaTypeByRef (schemaTypes : Map<string, IntrospectionType>) (tref : IntrospectionTypeRef) =
        match tref.Kind with
        | TypeKind.NON_NULL
        | TypeKind.LIST when tref.OfType.IsSome -> tryGetSchemaTypeByRef schemaTypes tref.OfType.Value
        | _ ->
            tref.Name
            |> ValueOption.bind (schemaTypes.TryFind >> ValueOption.ofOption)

    type SchemaInfo = {
        SchemaTypes : Map<string, IntrospectionType>
        QueryType : IntrospectionType voption
        SubscriptionType : IntrospectionType voption
        MutationType : IntrospectionType voption
        Directives : IntrospectionDirective[]
    } with

        member x.TryGetTypeByRef (tref : IntrospectionTypeRef) = tryGetSchemaTypeByRef x.SchemaTypes tref
        static member FromIntrospectionSchema (schema : IntrospectionSchema) =
            let schemaTypes = schema.Types |> Seq.map (fun x -> x.Name, x) |> Map.ofSeq
            {
                SchemaTypes = schema.Types |> Seq.map (fun x -> x.Name, x) |> Map.ofSeq
                QueryType = tryGetSchemaTypeByRef schemaTypes schema.QueryType
                MutationType =
                    schema.MutationType
                    |> ValueOption.bind (tryGetSchemaTypeByRef schemaTypes)
                SubscriptionType =
                    schema.SubscriptionType
                    |> ValueOption.bind (tryGetSchemaTypeByRef schemaTypes)
                Directives = schema.Directives
            }
        member x.TryGetOperationType (ot : OperationType) =
            match ot with
            | Query -> x.QueryType
            | Mutation -> x.MutationType
            | Subscription -> x.SubscriptionType
        member x.TryGetTypeByName (name : string) = x.SchemaTypes.TryFind (name)
        member x.TryGetInputType (input : InputType) =
            match input with
            | NamedType name ->
                x.TryGetTypeByName (name)
                |> Option.bind (fun x ->
                    match x.Kind with
                    | TypeKind.INPUT_OBJECT
                    | TypeKind.SCALAR
                    | TypeKind.ENUM -> Some x
                    | _ -> None)
                |> Option.map IntrospectionTypeRef.Named
            | ListType inner ->
                x.TryGetInputType (inner)
                |> Option.map IntrospectionTypeRef.List
            | NonNullType inner ->
                x.TryGetInputType (inner)
                |> Option.map IntrospectionTypeRef.NonNull

    /// Contains information about the selection in a field or a fragment, with related GraphQL schema type information.
    and SelectionInfo = {
        /// Contains the information about the field inside the selection set of the parent type.
        Field : Field
        /// Contains the reference to tye field type in the schema.
        FieldType : IntrospectionTypeRef voption
        /// Contains the reference to the parent type of the current field in the schema.
        ParentType : IntrospectionType
        /// If the field is inside a fragment selection, gets the reference to the fragment type of the current field in the schema.
        FragmentType : IntrospectionType voption
        /// In case this field is part of a selection of a fragment spread, gets the name of the fragment spread.
        FragmentSpreadName : string voption
        /// Contains the selection info of this field, if it is an Object, Interface or Union type.
        SelectionSet : SelectionInfo list
        /// In case the schema definition fo this field has input values, gets information about the input values of the field in the schema.
        InputValues : IntrospectionInputVal[]
        /// Contains the path of the field in the document.
        Path : FieldPath
    } with

        /// If the field has an alias, return its alias. Otherwise, returns its name.
        member x.AliasOrName = x.Field.AliasOrName
        /// If the field is inside a selection of a fragment definition, returns the fragment type containing the field.
        /// Otherwise, return the parent type in the schema definition.
        member x.FragmentOrParentType = x.FragmentType |> ValueOption.defaultValue x.ParentType

    /// Contains information about an operation definition in the document, with related GraphQL schema type information.
    type OperationDefinitionInfo = {
        /// Returns the definition from the parsed document.
        Definition : OperationDefinition
        /// Returns the selection information about the operation, with related GraphQL schema type information.
        SelectionSet : SelectionInfo list
    } with

        /// Returns the name of the operation definition, if it does have a name.
        member x.Name = x.Definition.Name

    /// Contains information about a fragment definition in the document, with related GraphQL schema type information.
    type FragmentDefinitionInfo = {
        /// Returns the definition from the parsed document.
        Definition : FragmentDefinition
        /// Returns the selection information about the fragment, with related GraphQL schema type information.
        SelectionSet : SelectionInfo list
    } with

        /// Returns the name of the fragment definition, if it does have a name.
        member x.Name = x.Definition.Name

    /// Contains information about a definition in the document, with related GraphQL schema type information.
    type DefinitionInfo =
        | OperationDefinitionInfo of OperationDefinitionInfo
        | FragmentDefinitionInfo of FragmentDefinitionInfo

        /// Returns the definition from the parsed definition in the document.
        member x.Definition =
            match x with
            | OperationDefinitionInfo x -> OperationDefinition x.Definition
            | FragmentDefinitionInfo x -> FragmentDefinition x.Definition
        /// Returns the name of the definition, if it does have a name.
        member x.Name = x.Definition.Name
        /// Returns the selection information about the definition, with related GraphQL schema type information.
        member x.SelectionSet =
            match x with
            | OperationDefinitionInfo x -> x.SelectionSet
            | FragmentDefinitionInfo x -> x.SelectionSet
        /// Returns the directives from the parsed definition in the document.
        member x.Directives =
            match x with
            | OperationDefinitionInfo x -> x.Definition.Directives
            | FragmentDefinitionInfo x -> x.Definition.Directives

    /// The validation  context used to run validations against a parsed document.
    /// It should have the schema type information, the original document and the definition information about the original document.
    type ValidationContext = {
        /// Gets information about all definitions in the original document, including related GraphQL schema type information for them.
        Definitions : DefinitionInfo list
        /// Gets information about the schema which the document is validated against.
        Schema : SchemaInfo
        /// Gets the document that is being validated.
        Document : Document
    } with

        /// Gets the information about all the operations in the original document, including related GraphQL schema type information for them.
        member x.OperationDefinitions =
            x.Definitions
            |> List.vchoose (function
                | OperationDefinitionInfo x -> ValueSome x
                | _ -> ValueNone)
        /// Gets the information about all the fragments in the original document, including related GraphQL schema type information for them.
        member x.FragmentDefinitions =
            x.Definitions
            |> List.vchoose (function
                | FragmentDefinitionInfo x -> ValueSome x
                | _ -> ValueNone)

    /// Contains information about a fragment type and its related GraphQL schema type.
    type FragmentTypeInfo =
        | Inline of typeCondition : IntrospectionType
        | Spread of name : string * directives : Directive list * typeCondition : IntrospectionType

        /// Gets the schema type related to the type condition of this fragment.
        member x.TypeCondition =
            match x with
            | Inline fragType -> fragType
            | Spread (_, _, fragType) -> fragType
        /// In case this fragment is a fragment spread, get its name.
        member x.Name =
            match x with
            | Inline _ -> ValueNone
            | Spread (name, _, _) -> ValueSome name

    type SelectionInfoContext = {
        Schema : SchemaInfo
        /// The fragments whose spreads are inlined, by name
        Fragments : IReadOnlyDictionary<string, FragmentDefinition>
        ParentType : IntrospectionType
        FragmentType : FragmentTypeInfo voption
        Path : FieldPath
        SelectionSet : Selection list
    } with

        member x.FragmentOrParentType =
            x.FragmentType
            |> ValueOption.map _.TypeCondition
            |> ValueOption.defaultValue x.ParentType

    let private tryFindInArrayOption (finder : 'T -> bool) = ValueOption.bind (Array.vtryFind finder)

    let private onAllSelections (ctx : ValidationContext) (onSelection : SelectionInfo -> ValidationResult<GQLProblemDetails>) =
        let rec traverseSelections selection =
            (onSelection selection)
            @@ (selection.SelectionSet
                |> ValidationResult.collect traverseSelections)
        ctx.Definitions
        |> ValidationResult.collect (fun def ->
            def.SelectionSet
            |> ValidationResult.collect traverseSelections)

    let rec private getFragSelectionSetInfo
        (visitedFragments : string list)
        (fragmentTypeInfo : FragmentTypeInfo)
        (fragmentSelectionSet : Selection list)
        (parentCtx : SelectionInfoContext)
        =
        match fragmentTypeInfo with
        | Inline fragType ->
            let fragCtx = {
                parentCtx with
                    ParentType = parentCtx.FragmentOrParentType
                    FragmentType = ValueSome (Inline fragType)
                    SelectionSet = fragmentSelectionSet
            }
            getSelectionSetInfo visitedFragments fragCtx
        | Spread (fragName, _, _) when List.contains fragName visitedFragments -> []
        | Spread (fragName, directives, fragType) ->
            let fragCtx = {
                parentCtx with
                    ParentType = parentCtx.FragmentOrParentType
                    FragmentType = ValueSome (Spread (fragName, directives, fragType))
                    SelectionSet = fragmentSelectionSet
            }
            getSelectionSetInfo (fragName :: visitedFragments) fragCtx

    and private getSelectionSetInfo (visitedFragments : string list) (ctx : SelectionInfoContext) : SelectionInfo list =
        // When building the selection info, we should not raise any error when a type referred by the document
        // is not found in the schema. Not found types are validated in another validation, without the need of the
        // selection info to do it. Info types are helpers to validate against their schema types when the match is found.
        // Because of that, whenever a field or fragment type does not have a matching type in the schema, we skip the selection production.
        ctx.SelectionSet
        |> List.collect (function
            | Field field ->
                let introspectionField =
                    ctx.FragmentOrParentType.Fields
                    |> tryFindInArrayOption (fun f -> f.Name = field.Name)
                let inputValues = introspectionField |> ValueOption.map (fun f -> f.Args)
                let fieldTypeRef = introspectionField |> ValueOption.map (fun f -> f.Type)
                let fieldPath = box field.AliasOrName :: ctx.Path
                let fieldSelectionSet =
                    voption {
                        let! fieldTypeRef = fieldTypeRef
                        let! fieldType = ctx.Schema.TryGetTypeByRef fieldTypeRef
                        let fieldCtx = {
                            ctx with
                                ParentType = fieldType
                                FragmentType = ValueNone
                                Path = fieldPath
                                SelectionSet = field.SelectionSet
                        }
                        return getSelectionSetInfo visitedFragments fieldCtx
                    }
                    |> ValueOption.defaultValue []
                {
                    Field = field
                    SelectionSet = fieldSelectionSet
                    FieldType = fieldTypeRef
                    ParentType = ctx.ParentType
                    FragmentType = ctx.FragmentType |> ValueOption.map _.TypeCondition
                    FragmentSpreadName = ctx.FragmentType |> ValueOption.bind _.Name
                    InputValues = inputValues |> ValueOption.defaultValue [||]
                    Path = fieldPath
                }
                |> List.singleton
            | InlineFragment inlineFrag ->
                // An inline fragment without a type condition applies to its parent type
                let fragType =
                    match inlineFrag.TypeCondition with
                    | ValueSome typeCondition -> ctx.Schema.TryGetTypeByName typeCondition |> ValueOption.ofOption
                    | ValueNone -> ValueSome ctx.FragmentOrParentType
                fragType
                |> ValueOption.map (fun fragType -> getFragSelectionSetInfo visitedFragments (Inline fragType) inlineFrag.SelectionSet ctx)
                |> ValueOption.defaultValue List.empty
            | FragmentSpread fragSpread ->
                voption {
                    let! fragDef =
                        match ctx.Fragments.TryGetValue fragSpread.Name with
                        | true, fragDef -> ValueSome fragDef
                        | false, _ -> ValueNone
                    let! typeCondition = fragDef.TypeCondition
                    let! fragType = ctx.Schema.TryGetTypeByName typeCondition
                    let fragType = Spread (fragSpread.Name, fragSpread.Directives, fragType)
                    return getFragSelectionSetInfo visitedFragments fragType fragDef.SelectionSet ctx
                }
                |> ValueOption.defaultValue List.empty)

    let private getOperationDefinitions (ast : Document) =
        ast.Definitions
        |> List.vchoose (function
            | OperationDefinition x -> ValueSome x
            | _ -> ValueNone)

    let private getFragmentDefinitions (ast : Document) =
        ast.Definitions
        |> List.vchoose (function
            | FragmentDefinition x when x.Name.IsSome -> ValueSome x
            | _ -> ValueNone)

    /// The named fragments of the document by name. As in fragment spread resolution, the first definition of a name wins.
    let private getFragmentsByName (fragmentDefinitions : FragmentDefinition list) =
        let fragments = Dictionary<string, FragmentDefinition> (StringComparer.Ordinal)
        for fragment in fragmentDefinitions do
            if not (fragments.ContainsKey fragment.Name.Value) then
                fragments.Add (fragment.Name.Value, fragment)
        fragments

    /// The size of a selection set before its fragment spreads are inlined.
    [<Struct>]
    type private SelectionSetShape = {
        /// The number of fields, inline fragments and fragment spreads, nested ones included.
        Selections : int64
        /// The deepest nesting level; the selection set itself is level 1, and each field selection set, inline fragment and fragment spread adds one.
        Depth : int
        /// The fragment spreads with the level of the selection set that contains each of them.
        Spreads : struct (string * int)[]
    }

    // Iterative, so a deeply nested document cannot overflow the stack here
    let private getSelectionSetShape (root : Selection list) =
        let mutable selections = 0L
        let mutable depth = 0
        let spreads = ResizeArray<struct (string * int)> ()
        let pending = Stack<struct (Selection list * int)> ()
        pending.Push (struct (root, 1))
        while pending.Count > 0 do
            let struct (selectionSet, level) = pending.Pop ()
            if not selectionSet.IsEmpty && level > depth then
                depth <- level
            for selection in selectionSet do
                selections <- selections + 1L
                match selection with
                | Field field when not field.SelectionSet.IsEmpty -> pending.Push (struct (field.SelectionSet, level + 1))
                | Field _ -> ()
                | InlineFragment fragment -> pending.Push (struct (fragment.SelectionSet, level + 1))
                | FragmentSpread spread -> spreads.Add (struct (spread.Name, level))
        { Selections = selections; Depth = depth; Spreads = spreads.ToArray () }

    let private getFragmentShapes (fragments : Dictionary<string, FragmentDefinition>) =
        let shapes = Dictionary<string, SelectionSetShape> (fragments.Count, StringComparer.Ordinal)
        for KeyValue (name, fragment) in fragments do
            shapes.Add (name, getSelectionSetShape fragment.SelectionSet)
        shapes

    /// <summary>
    /// The names of the fragments that are part of a fragment spread cycle, including fragments that spread themselves.
    /// </summary>
    /// <remarks>
    /// Iterative Tarjan's strongly connected components algorithm, linear in the number of fragment spreads.
    /// </remarks>
    let private findCyclicFragments (shapes : Dictionary<string, SelectionSetShape>) =
        let targets = Dictionary<string, string[]> (shapes.Count, StringComparer.Ordinal)
        for KeyValue (name, shape) in shapes do
            targets.Add (
                name,
                shape.Spreads
                |> Seq.map (fun struct (target, _) -> target)
                |> Seq.filter shapes.ContainsKey
                |> Seq.distinct
                |> Seq.toArray
            )
        let indexes = Dictionary<string, int> (shapes.Count, StringComparer.Ordinal)
        let lowLinks = Dictionary<string, int> (shapes.Count, StringComparer.Ordinal)
        let onStack = HashSet<string> (StringComparer.Ordinal)
        let componentStack = Stack<string> ()
        let cyclic = HashSet<string> (StringComparer.Ordinal)
        let mutable nextIndex = 0
        let visit (name : string) =
            indexes.Add (name, nextIndex)
            lowLinks.Add (name, nextIndex)
            nextIndex <- nextIndex + 1
            componentStack.Push name
            onStack.Add name |> ignore
        for root in shapes.Keys do
            if not (indexes.ContainsKey root) then
                // Each entry is a fragment and the position of the next spread target to explore
                let work = Stack<struct (string * int)> ()
                visit root
                work.Push (struct (root, 0))
                while work.Count > 0 do
                    let struct (name, position) = work.Pop ()
                    let nameTargets = targets[name]
                    if position < nameTargets.Length then
                        work.Push (struct (name, position + 1))
                        let target = nameTargets[position]
                        if not (indexes.ContainsKey target) then
                            visit target
                            work.Push (struct (target, 0))
                        elif onStack.Contains target then
                            lowLinks[name] <- min (lowLinks[name]) (indexes[target])
                    else
                        if work.Count > 0 then
                            let struct (parent, _) = work.Peek ()
                            lowLinks[parent] <- min (lowLinks[parent]) (lowLinks[name])
                        if lowLinks[name] = indexes[name] then
                            // The fragment is the root of a strongly connected component: pop the whole component
                            let componentNames = ResizeArray<string> ()
                            let mutable isRoot = false
                            while not isRoot do
                                let componentName = componentStack.Pop ()
                                onStack.Remove componentName |> ignore
                                componentNames.Add componentName
                                isRoot <- String.Equals (componentName, name, StringComparison.Ordinal)
                            if componentNames.Count > 1 || Array.contains name nameTargets then
                                for componentName in componentNames do
                                    cyclic.Add componentName |> ignore
        cyclic

    /// <summary>
    /// The named fragments whose spreads are inlined while the document is validated: all of them except the ones that form a cycle.
    /// </summary>
    /// <remarks>
    /// Following the spreads of a cycle inlines every simple path through it, which grows factorially with the size of the cycle.
    /// The cycles themselves are reported by <see cref="validateFragmentsMustNotFormCycles"/>.
    /// </remarks>
    let private getInlinableFragmentDefinitions (fragmentDefinitions : FragmentDefinition list) =
        let cyclic =
            fragmentDefinitions
            |> getFragmentsByName
            |> getFragmentShapes
            |> findCyclicFragments
        if cyclic.Count = 0 then
            fragmentDefinitions
        else
            fragmentDefinitions
            |> List.filter (fun fragment -> not (cyclic.Contains fragment.Name.Value))

    /// <summary>
    /// The number of selections and the nesting depth that validating the document inlines, saturated at <paramref name="selectionCap"/>.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Every operation and fragment definition is counted with its fragment spreads inlined, as
    /// <see cref="getValidationContext"/> builds them. Spreads of fragments that form a cycle are not inlined.
    /// </para>
    /// <para>
    /// The count of each fragment is computed once (as in Apollo Server's <c>RecursiveSelectionsLimit</c> rule),
    /// and the traversal is iterative, so neither an exponential fragment bomb nor a long chain of fragments can stall
    /// or overflow it.
    /// </para>
    /// </remarks>
    let internal measureDocument (selectionCap : int64) (ast : Document) : struct (int64 * int) =
        let shapes =
            getFragmentDefinitions ast
            |> getFragmentsByName
            |> getFragmentShapes
        let cyclic = findCyclicFragments shapes
        let isInlined name = shapes.ContainsKey name && not (cyclic.Contains name)
        let inlined = Dictionary<string, struct (int64 * int)> (shapes.Count, StringComparer.Ordinal)
        // Requires the inlined size of every inlined spread target to be known already
        let inlineSpreads (shape : SelectionSetShape) =
            let mutable selections = shape.Selections
            let mutable depth = shape.Depth
            for spread in shape.Spreads do
                let struct (target, level) = spread
                if isInlined target then
                    let struct (targetSelections, targetDepth) = inlined[target]
                    selections <- min selectionCap (selections + targetSelections)
                    depth <- max depth (level + targetDepth)
            struct (selections, depth)
        let pushMissingTargets (pending : Stack<string>) (shape : SelectionSetShape) =
            let mutable pushed = false
            for spread in shape.Spreads do
                let struct (target, _) = spread
                if isInlined target && not (inlined.ContainsKey target) then
                    pending.Push target
                    pushed <- true
            pushed
        let measure (shape : SelectionSetShape) =
            // Post-order over the spread targets, which form no cycle, so this terminates
            let pending = Stack<string> ()
            pushMissingTargets pending shape |> ignore
            while pending.Count > 0 do
                let name = pending.Peek ()
                if inlined.ContainsKey name then
                    pending.Pop () |> ignore
                else
                    let nameShape = shapes[name]
                    if not (pushMissingTargets pending nameShape) then
                        pending.Pop () |> ignore
                        inlined.Add (name, inlineSpreads nameShape)
            inlineSpreads shape
        let mutable selections = 0L
        let mutable depth = 0
        for definition in ast.Definitions do
            let struct (definitionSelections, definitionDepth) = measure (getSelectionSetShape definition.SelectionSet)
            selections <- min selectionCap (selections + definitionSelections)
            depth <- max depth definitionDepth
        struct (selections, depth)

    /// <summary>
    /// Rejects a document whose validation would inline more selections or nest deeper than allowed,
    /// before any validation context is built for it.
    /// </summary>
    let internal checkDocumentLimits (maxRecursiveSelections : int) (maxNestingDepth : int) (ast : Document) =
        let struct (selections, depth) = measureDocument (int64 maxRecursiveSelections + 1L) ast
        if selections > int64 maxRecursiveSelections then
            AstError.AsResult $"The document recursively requests too many selections (more than %i{maxRecursiveSelections})."
        elif depth > maxNestingDepth then
            AstError.AsResult $"The document is nested too deeply once fragment spreads are inlined (more than %i{maxNestingDepth} levels)."
        else
            Success

    /// Prepare a ValidationContext for the given Document and SchemaInfo to make validation operations easier.
    let internal getValidationContext (schemaInfo : SchemaInfo) (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        let inlinableFragments =
            fragmentDefinitions
            |> getInlinableFragmentDefinitions
            |> getFragmentsByName
        let fragmentInfos =
            fragmentDefinitions
            |> List.vchoose (fun def -> voption {
                let! typeCondition = def.TypeCondition
                let! fragType = schemaInfo.TryGetTypeByName typeCondition
                let fragCtx = {
                    Schema = schemaInfo
                    Fragments = inlinableFragments
                    ParentType = fragType
                    FragmentType = ValueSome (Spread (def.Name.Value, def.Directives, fragType))
                    Path = [ def.Name.Value ]
                    SelectionSet = def.SelectionSet
                }
                return FragmentDefinitionInfo { Definition = def; SelectionSet = getSelectionSetInfo [] fragCtx }
            })
        let operationInfos =
            getOperationDefinitions ast
            |> List.vchoose (fun def -> voption {
                let! parentType = schemaInfo.TryGetOperationType def.OperationType
                let path = def.Name |> ValueOption.map box |> ValueOption.toList
                let opCtx = {
                    Schema = schemaInfo
                    Fragments = inlinableFragments
                    ParentType = parentType
                    FragmentType = ValueNone
                    Path = path
                    SelectionSet = def.SelectionSet
                }
                return OperationDefinitionInfo { Definition = def; SelectionSet = getSelectionSetInfo [] opCtx }
            })
        {
            Definitions = fragmentInfos @ operationInfos
            Schema = schemaInfo
            Document = ast
        }

    /// Reports, in the order of their first definition, the operation names defined more than once.
    let internal validateOperationNameUniqueness (ctx : ValidationContext) =
        // Counted in one pass: counting every name against all definitions is quadratic in the number of operations
        let counts = Dictionary<string, int> (StringComparer.Ordinal)
        let names = ResizeArray<string> ()
        for operation in getOperationDefinitions ctx.Document do
            match operation.Name with
            | ValueSome name ->
                match counts.TryGetValue name with
                | true, count -> counts[name] <- count + 1
                | false, _ ->
                    counts.Add (name, 1)
                    names.Add name
            | ValueNone -> ()
        names
        |> ValidationResult.collect (fun name ->
            let count = counts[name]
            if count <= 1 then
                Success
            else
                AstError.AsResult $"Operation '%s{name}' has %i{count} definitions. Each operation name must be unique.")

    let internal validateLoneAnonymousOperation (ctx : ValidationContext) =
        let operations = ctx.OperationDefinitions |> List.map _.Definition
        let unamed = operations |> List.filter _.Name.IsNone
        if unamed.Length = 0 then
            Success
        elif unamed.Length = 1 && operations.Length = 1 then
            Success
        else
            AstError.AsResult
                "An anonymous operation must be the only operation in a document. This document has at least one anonymous operation and more than one operation."

    let internal validateSubscriptionSingleRootField (ctx : ValidationContext) =
        let fragments = getFragmentDefinitions ctx.Document |> getFragmentsByName
        ctx.Document.Definitions
        |> ValidationResult.collect (function
            | OperationDefinition def when def.OperationType = Subscription ->
                // As in CollectFields, each fragment is collected once per operation, those that form a cycle included.
                // The selections are walked depth first with a stack: following the spreads by recursion would nest once
                // for every fragment of a chain
                let visitedFragments = HashSet<string> (StringComparer.Ordinal)
                let namesInWalkOrder = ResizeArray<string> ()
                let pending = Stack<Selection list> ()
                pending.Push def.SelectionSet
                while pending.Count > 0 do
                    match pending.Pop () with
                    | [] -> ()
                    | selection :: rest ->
                        pending.Push rest
                        match selection with
                        | Field field -> namesInWalkOrder.Add field.AliasOrName
                        | InlineFragment frag -> pending.Push frag.SelectionSet
                        | FragmentSpread spread when visitedFragments.Add spread.Name ->
                            match fragments.TryGetValue spread.Name with
                            | true, frag -> pending.Push frag.SelectionSet
                            | false, _ -> ()
                        | FragmentSpread _ -> ()
                // The message lists the names last found first
                let fieldNames = namesInWalkOrder |> Seq.rev |> List.ofSeq
                if fieldNames.Length <= 1 then
                    Success
                else
                    let fieldNamesAsString = System.String.Join (", ", fieldNames)
                    match def.Name with
                    | ValueSome operationName ->
                        AstError.AsResult
                            $"Subscription operations should have only one root field. Operation '%s{operationName}' has %i{fieldNames.Length} fields (%s{fieldNamesAsString})."
                    | ValueNone ->
                        AstError.AsResult
                            $"Subscription operations should have only one root field. Operation has %i{fieldNames.Length} fields (%s{fieldNamesAsString})."
            | _ -> Success)

    let internal validateSelectionFieldTypes (ctx : ValidationContext) =
        onAllSelections ctx (fun selection ->
            if metaTypeFields.ContainsKey (selection.Field.Name) then
                Success
            else
                let exists =
                    selection.FragmentOrParentType.Fields
                    |> ValueOption.map (Array.exists (fun f -> f.Name = selection.Field.Name))
                    |> ValueOption.defaultValue false
                if not exists then
                    AstError.AsResult (
                        $"Field '%s{selection.Field.Name}' is not defined in schema type '%s{selection.FragmentOrParentType.Name}'.",
                        selection.Path
                    )
                else
                    Success)

    let private typesAreApplicable (parentType : IntrospectionType, fragmentType : IntrospectionType) =
        let parentPossibleTypes =
            parentType.PossibleTypes
            |> ValueOption.defaultValue [||]
            |> Seq.vchoose _.Name
            |> Seq.append (Seq.singleton parentType.Name)
            |> Set.ofSeq
        let fragmentPossibleTypes =
            fragmentType.PossibleTypes
            |> ValueOption.defaultValue [||]
            |> Seq.vchoose _.Name
            |> Seq.append (Seq.singleton fragmentType.Name)
            |> Set.ofSeq
        let applicableTypes = Set.intersect parentPossibleTypes fragmentPossibleTypes
        applicableTypes.Count > 0

    /// <summary>
    /// Compares unordered pairs of selections by reference: two selections with equal contents at different places are
    /// different selections, and the pair of <c>a</c> and <c>b</c> is the same pair whichever of them comes first.
    /// </summary>
    let private selectionPairComparer =
        { new IEqualityComparer<struct (SelectionInfo * SelectionInfo)> with
            member _.Equals (x, y) =
                let struct (xA, xB) = x
                let struct (yA, yB) = y
                (obj.ReferenceEquals (xA, yA) && obj.ReferenceEquals (xB, yB))
                || (obj.ReferenceEquals (xA, yB) && obj.ReferenceEquals (xB, yA))
            member _.GetHashCode pair =
                let struct (a, b) = pair
                // Symmetric, so that both orders of a pair hash alike
                LanguagePrimitives.PhysicalHash a ^^^ LanguagePrimitives.PhysicalHash b
        }

    /// <summary>
    /// The state of the field selection merging rule for one document.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Merging the selections of two fields and comparing the merged selections again reaches the same pairs of
    /// selections over and over, which grows exponentially with the nesting of fields that share a response name.
    /// Like graphql-js's <c>PairSet</c>, the rule therefore compares each pair once, whichever of its two selections
    /// comes first, which also reports each conflict once.
    /// </para>
    /// <para>
    /// The rule also stops once it has found more errors than validation reports.
    /// </para>
    /// </remarks>
    type private FieldMergingState
        /// <param name="maxErrors">The number of errors validation reports; the rule stops once it has found more.</param>
        (maxErrors : int) =
        member val ComparedShapes = HashSet<struct (SelectionInfo * SelectionInfo)> (selectionPairComparer)
        member val ComparedFields = HashSet<struct (SelectionInfo * SelectionInfo)> (selectionPairComparer)
        member val ErrorCount = 0 with get, set
        member this.IsOverBudget = this.ErrorCount > maxErrors
        member this.Report (message : string, path : FieldPath) =
            this.ErrorCount <- this.ErrorCount + 1
            AstError.AsResult (message, path)

    let private isSameType (typeA : IntrospectionType) (typeB : IntrospectionType) =
        // Types are unique by name in a schema; comparing the records structurally compares the whole type
        String.Equals (typeA.Name, typeB.Name, StringComparison.Ordinal)

    let rec private sameResponseShape (state : FieldMergingState) (fieldA : SelectionInfo, fieldB : SelectionInfo) =
        if state.IsOverBudget || not (state.ComparedShapes.Add (struct (fieldA, fieldB))) then
            Success
        elif fieldA.FieldType = fieldB.FieldType then
            let fieldsForName = Dictionary<string, SelectionInfo list> (StringComparer.Ordinal)
            fieldA.SelectionSet
            |> List.iter (fun selection -> Dictionary.addWith (List.append) selection.AliasOrName [ selection ] fieldsForName)
            fieldB.SelectionSet
            |> List.iter (fun selection -> Dictionary.addWith (List.append) selection.AliasOrName [ selection ] fieldsForName)
            fieldsForName
            |> ValidationResult.collect (fun (KeyValue (_, selectionSet)) ->
                if selectionSet.Length < 2 then
                    Success
                else
                    List.pairwise selectionSet
                    |> ValidationResult.collect (sameResponseShape state))
        else
            state.Report (
                $"Field name or alias '%s{fieldA.AliasOrName}' appears two times, but they do not have the same return types in the scope of the parent type.",
                fieldA.Path
            )

    let rec private fieldsInSetCanMerge (state : FieldMergingState) (set : SelectionInfo list) =
        let fieldsForName = set |> List.groupBy _.AliasOrName
        fieldsForName
        |> ValidationResult.collect (fun (aliasOrName, selectionSet) ->
            if selectionSet.Length < 2 then
                Success
            else
                List.pairwise selectionSet
                |> ValidationResult.collect (fun (fieldA, fieldB) ->
                    if state.IsOverBudget || not (state.ComparedFields.Add (struct (fieldA, fieldB))) then
                        Success
                    else
                        let hasSameShape = sameResponseShape state (fieldA, fieldB)
                        if
                            isSameType fieldA.FragmentOrParentType fieldB.FragmentOrParentType
                            || fieldA.FragmentOrParentType.Kind <> TypeKind.OBJECT
                            || fieldB.FragmentOrParentType.Kind <> TypeKind.OBJECT
                        then
                            if fieldA.Field.Name <> fieldB.Field.Name then
                                hasSameShape
                                @@ state.Report (
                                    $"Field name or alias '%s{aliasOrName}' is referring to fields '%s{fieldA.Field.Name}' and '%s{fieldB.Field.Name}', but they are different fields in the scope of the parent type.",
                                    fieldA.Path
                                )
                            else if fieldA.Field.Arguments <> fieldB.Field.Arguments then
                                hasSameShape
                                @@ state.Report (
                                    $"Field name or alias '%s{aliasOrName}' refers to field '%s{fieldA.Field.Name}' two times, but each reference has different argument sets.",
                                    fieldA.Path
                                )
                            else
                                let mergedSet = fieldA.SelectionSet @ fieldB.SelectionSet
                                hasSameShape @@ (fieldsInSetCanMerge state mergedSet)
                        else
                            hasSameShape))

    /// The field selection merging rule, stopping once it has found more than the given number of errors.
    let internal validateFieldSelectionMergingWithin (maxErrors : int) (ctx : ValidationContext) =
        let state = FieldMergingState maxErrors
        ctx.Definitions
        |> ValidationResult.collect (fun def -> fieldsInSetCanMerge state def.SelectionSet)

    let internal validateFieldSelectionMerging (ctx : ValidationContext) =
        validateFieldSelectionMergingWithin DocumentLimitsDefaults.MaxValidationErrors ctx

    let rec private checkLeafFieldSelection (selection : SelectionInfo) =
        let rec validateByKind (fieldType : IntrospectionTypeRef) (selectionSetLength : int) =
            match fieldType.Kind with
            | TypeKind.NON_NULL
            | TypeKind.LIST when fieldType.OfType.IsSome -> validateByKind fieldType.OfType.Value selectionSetLength
            | TypeKind.SCALAR
            | TypeKind.ENUM when selectionSetLength > 0 ->
                AstError.AsResult (
                    $"Field '%s{selection.Field.Name}' of '%s{selection.FragmentOrParentType.Name}' type is of type kind %s{fieldType.Kind.ToString ()}, and therefore should not contain inner fields in its selection.",
                    selection.Path
                )
            | TypeKind.INTERFACE
            | TypeKind.UNION
            | TypeKind.OBJECT when selectionSetLength = 0 ->
                AstError.AsResult (
                    $"Field '%s{selection.Field.Name}' of '%s{selection.FragmentOrParentType.Name}' type is of type kind %s{fieldType.Kind.ToString ()}, and therefore should have inner fields in its selection.",
                    selection.Path
                )
            | _ -> Success
        match selection.FieldType with
        | ValueSome fieldType -> validateByKind fieldType selection.SelectionSet.Length
        | ValueNone -> Success

    let internal validateLeafFieldSelections (ctx : ValidationContext) = onAllSelections ctx checkLeafFieldSelection

    let private checkFieldArgumentNames (schemaInfo : SchemaInfo) (selection : SelectionInfo) =
        let argumentsValid =
            selection.Field.Arguments
            |> ValidationResult.collect (fun arg ->
                let schemaArgumentNames =
                    metaTypeFields.TryFind (selection.Field.Name)
                    |> ValueOption.ofOption
                    |> ValueOption.map _.ArgumentNames
                    |> ValueOption.defaultWith (fun () -> selection.InputValues |> Array.map _.Name)
                match schemaArgumentNames |> Array.vtryFind (fun x -> x = arg.Name) with
                | ValueSome _ -> Success
                | ValueNone ->
                    AstError.AsResult (
                        $"Field '%s{selection.Field.Name}' of type '%s{selection.FragmentOrParentType.Name}' does not have an input named '%s{arg.Name}' in its definition.",
                        selection.Path
                    ))
        let directivesValid =
            selection.Field.Directives
            |> ValidationResult.collect (fun directive ->
                match
                    schemaInfo.Directives
                    |> Array.vtryFind (fun d -> d.Name = directive.Name)
                with
                | ValueSome directiveType ->
                    directive.Arguments
                    |> ValidationResult.collect (fun arg ->
                        match
                            directiveType.Args
                            |> Array.vtryFind (fun argt -> argt.Name = arg.Name)
                        with
                        | ValueSome _ -> Success
                        | ValueNone ->
                            AstError.AsResult (
                                $"Directive '%s{directiveType.Name}' of field '%s{selection.Field.Name}' of type '%s{selection.FragmentOrParentType.Name}' does not have an argument named '%s{arg.Name}' in its definition.",
                                selection.Path
                            ))
                | ValueNone -> Success)
        argumentsValid @@ directivesValid

    let internal validateArgumentNames (ctx : ValidationContext) = onAllSelections ctx (checkFieldArgumentNames ctx.Schema)

    let rec private validateArgumentUniquenessInSelection (selection : SelectionInfo) =
        let validateArgs (fieldOrDirective : string) (path : FieldPath) (args : Argument list) =
            args
            |> List.countBy _.Name
            |> ValidationResult.collect (fun (name, length) ->
                if length > 1 then
                    AstError.AsResult (
                        $"There are %i{length} arguments with name '%s{name}' defined in %s{fieldOrDirective}. Field arguments must be unique.",
                        path
                    )
                else
                    Success)
        let argsValid =
            validateArgs $"alias or field '%s{selection.AliasOrName}'" selection.Path selection.Field.Arguments
        let directiveArgsValid =
            selection.Field.Directives
            |> ValidationResult.collect (fun directive -> validateArgs $"directive '%s{directive.Name}'" selection.Path directive.Arguments)
        argsValid @@ directiveArgsValid

    let internal validateArgumentUniqueness (ctx : ValidationContext) = onAllSelections ctx validateArgumentUniquenessInSelection

    let private checkRequiredArguments (schemaInfo : SchemaInfo) (selection : SelectionInfo) =
        let inputsValid =
            selection.InputValues
            |> ValidationResult.collect (fun argDef ->
                match argDef.Type.Kind with
                | TypeKind.NON_NULL when argDef.DefaultValue.IsNone ->
                    match
                        selection.Field.Arguments
                        |> List.vtryFind (fun arg -> arg.Name = argDef.Name)
                    with
                    | ValueSome arg when arg.Value <> NullValue -> Success
                    | _ ->
                        AstError.AsResult (
                            $"Argument '%s{argDef.Name}' of field '%s{selection.Field.Name}' of type '%s{selection.FragmentOrParentType.Name}' is required and does not have a default value.",
                            selection.Path
                        )
                | _ -> Success)
        let directivesValid =
            selection.Field.Directives
            |> ValidationResult.collect (fun directive ->
                match
                    schemaInfo.Directives
                    |> Array.vtryFind (fun d -> d.Name = directive.Name)
                with
                | ValueSome directiveType ->
                    directiveType.Args
                    |> ValidationResult.collect (fun argDef ->
                        match argDef.Type.Kind with
                        | TypeKind.NON_NULL when argDef.DefaultValue.IsNone ->
                            match
                                directive.Arguments
                                |> List.vtryFind (fun arg -> arg.Name = argDef.Name)
                            with
                            | ValueSome arg when arg.Value <> NullValue -> Success
                            | _ ->
                                AstError.AsResult (
                                    $"Argument '%s{argDef.Name}' of directive '%s{directiveType.Name}' of field '%s{selection.Field.Name}' of type '%s{selection.FragmentOrParentType.Name}' is required and does not have a default value.",
                                    selection.Path
                                )
                        | _ -> Success)
                | ValueNone -> Success)
        inputsValid @@ directivesValid

    let internal validateRequiredArguments (ctx : ValidationContext) = onAllSelections ctx (checkRequiredArguments ctx.Schema)

    let internal validateFragmentNameUniqueness (ctx : ValidationContext) =
        let counts = Dictionary<string, int> ()
        ctx.FragmentDefinitions
        |> List.iter (fun frag ->
            frag.Definition.Name
            |> ValueOption.iter (fun name -> Dictionary.addWith (+) name 1 counts))
        counts
        |> ValidationResult.collect (fun (KeyValue (name, length)) ->
            if length > 1 then
                AstError.AsResult
                    $"There are %i{length} fragments with name '%s{name}' in the document. Fragment definitions must have unique names."
            else
                Success)

    let rec private checkFragmentTypeExistence
        (fragmentDefinitions : FragmentDefinition list)
        (schemaInfo : SchemaInfo)
        (path : FieldPath)
        (frag : FragmentDefinition)
        =
        let typeConditionsValid =
            match frag.TypeCondition with
            // An inline fragment without a type condition applies to its parent type
            | ValueNone -> Success
            | ValueSome typeCondition ->
                match schemaInfo.TryGetTypeByName typeCondition with
                | Some _ -> Success
                | None ->
                    match frag.Name with
                    | ValueSome name ->
                        AstError.AsResult
                            $"Fragment '%s{name}' has type condition '%s{typeCondition}', but that type does not exist in the schema."
                    | ValueNone ->
                        AstError.AsResult (
                            $"Inline fragment has type condition '%s{typeCondition}', but that type does not exist in the schema.",
                            path
                        )
        typeConditionsValid
        @@ (frag.SelectionSet
            |> ValidationResult.collect (checkFragmentTypeExistenceInSelection fragmentDefinitions schemaInfo path))

    and private checkFragmentTypeExistenceInSelection (fragmentDefinitions : FragmentDefinition list) (schemaInfo : SchemaInfo) (path : FieldPath) =
        function
        | Field field ->
            let path = box field.AliasOrName :: path
            field.SelectionSet
            |> ValidationResult.collect (checkFragmentTypeExistenceInSelection fragmentDefinitions schemaInfo path)
        | InlineFragment frag -> checkFragmentTypeExistence fragmentDefinitions schemaInfo path frag
        | _ -> Success

    let internal validateFragmentTypeExistence (ctx : ValidationContext) =
        let fragmentDefinitions = getFragmentDefinitions ctx.Document
        ctx.Document.Definitions
        |> ValidationResult.collect (function
            | FragmentDefinition frag ->
                let path = frag.Name |> ValueOption.map box |> ValueOption.toList
                checkFragmentTypeExistence fragmentDefinitions ctx.Schema path frag
            | OperationDefinition odef ->
                let path = odef.Name |> ValueOption.map box |> ValueOption.toList
                odef.SelectionSet
                |> ValidationResult.collect (checkFragmentTypeExistenceInSelection fragmentDefinitions ctx.Schema path))

    let rec private checkFragmentOnCompositeType (selection : SelectionInfo) =
        let fragmentTypeValid =
            match selection.FragmentType with
            | ValueSome fragType ->
                match fragType.Kind with
                | TypeKind.UNION
                | TypeKind.OBJECT
                | TypeKind.INTERFACE -> Success
                | _ when selection.FragmentSpreadName.IsSome ->
                    AstError.AsResult (
                        $"Fragment '%s{selection.FragmentSpreadName.Value}' has type kind %s{fragType.Kind.ToString ()}, but fragments can only be defined in UNION, OBJECT or INTERFACE types.",
                        selection.Path
                    )
                | _ ->
                    AstError.AsResult (
                        $"Inline fragment has type kind %s{fragType.Kind.ToString ()}, but fragments can only be defined in UNION, OBJECT or INTERFACE types.",
                        selection.Path
                    )
            | ValueNone -> Success
        fragmentTypeValid
        @@ (selection.SelectionSet
            |> ValidationResult.collect checkFragmentOnCompositeType)

    let internal validateFragmentsOnCompositeTypes (ctx : ValidationContext) = onAllSelections ctx checkFragmentOnCompositeType

    let internal validateFragmentsMustBeUsed (ctx : ValidationContext) =
        let rec getSpreadNames (acc : Set<string>) =
            function
            | Field field ->
                field.SelectionSet
                |> Set.ofList
                |> Set.collect (getSpreadNames acc)
            | InlineFragment frag ->
                frag.SelectionSet
                |> Set.ofList
                |> Set.collect (getSpreadNames acc)
            | FragmentSpread spread -> acc.Add spread.Name
        let fragmentSpreadNames =
            Set.ofList ctx.Document.Definitions
            |> Set.collect (fun def ->
                Set.ofList def.SelectionSet
                |> Set.collect (getSpreadNames Set.empty))
        getFragmentDefinitions ctx.Document
        |> ValidationResult.collect (fun def ->
            if
                def.Name.IsSome
                && Set.contains def.Name.Value fragmentSpreadNames
            then
                Success
            else
                AstError.AsResult
                    $"Fragment '%s{def.Name.Value}' is not used in any operation in the document. Fragments must be used in at least one operation.")

    let rec private fragmentSpreadTargetDefinedInSelection (fragmentDefinitionNames : HashSet<string>) (path : FieldPath) =
        function
        | Field field ->
            let path = box field.AliasOrName :: path
            field.SelectionSet
            |> ValidationResult.collect (fragmentSpreadTargetDefinedInSelection fragmentDefinitionNames path)
        | InlineFragment frag ->
            frag.SelectionSet
            |> ValidationResult.collect (fragmentSpreadTargetDefinedInSelection fragmentDefinitionNames path)
        | FragmentSpread spread ->
            if fragmentDefinitionNames.Contains spread.Name then
                Success
            else
                AstError.AsResult ($"Fragment spread '%s{spread.Name}' refers to a non-existent fragment definition in the document.", path)

    let internal validateFragmentSpreadTargetDefined (ctx : ValidationContext) =
        let fragmentDefinitionNames = HashSet<string> (ctx.FragmentDefinitions |> Seq.vchoose _.Name, StringComparer.Ordinal)
        ctx.Document.Definitions
        |> ValidationResult.collect (function
            | FragmentDefinition frag ->
                let path = frag.Name |> ValueOption.map box |> ValueOption.toList
                frag.SelectionSet
                |> ValidationResult.collect (fragmentSpreadTargetDefinedInSelection fragmentDefinitionNames path)
            | OperationDefinition odef ->
                let path = odef.Name |> ValueOption.map box |> ValueOption.toList
                odef.SelectionSet
                |> ValidationResult.collect (fragmentSpreadTargetDefinedInSelection fragmentDefinitionNames path))

    /// Reports, in definition order, every fragment that is part of a fragment spread cycle, including fragments that spread themselves.
    let internal validateFragmentsMustNotFormCycles (ctx : ValidationContext) =
        let fragmentDefinitions = getFragmentDefinitions ctx.Document
        let cyclic =
            fragmentDefinitions
            |> getFragmentsByName
            |> getFragmentShapes
            |> findCyclicFragments
        let reported = HashSet<string> (StringComparer.Ordinal)
        fragmentDefinitions
        |> ValidationResult.collect (fun fragment ->
            let name = fragment.Name.Value
            if cyclic.Contains name && reported.Add name then
                AstError.AsResult $"Fragment '%s{name}' is making a cyclic reference."
            else
                Success)

    let private checkFragmentSpreadIsPossibleInSelection (path : FieldPath, parentType : IntrospectionType, fragmentType : IntrospectionType) =
        if not (typesAreApplicable (parentType, fragmentType)) then
            AstError.AsResult (
                $"Fragment type condition '%s{fragmentType.Name}' is not applicable to the parent type of the field '%s{parentType.Name}'.",
                path
            )
        else
            Success

    let rec private getFragmentAndParentTypes (set : SelectionInfo list) =
        ([], set)
        ||> List.fold (fun acc selection ->
            match selection.FragmentType with
            | ValueSome fragType when fragType.Name <> selection.ParentType.Name -> (selection.Path, selection.ParentType, fragType) :: acc
            | _ -> acc)

    let internal validateFragmentSpreadIsPossible (ctx : ValidationContext) =
        ctx.Definitions
        |> ValidationResult.collect (fun def ->
            def.SelectionSet
            |> getFragmentAndParentTypes
            |> ValidationResult.collect (checkFragmentSpreadIsPossibleInSelection))

    /// The scalar types a string literal cannot be coerced to
    let private nonStringScalars = [| "Int"; "Float"; "Boolean" |]

    /// Compares the value of an argument by reference, so that it is looked up without being walked, and its type structurally.
    let private valueTypeComparer =
        { new IEqualityComparer<struct (InputValue * IntrospectionTypeRef)> with
            member _.Equals (x, y) =
                let struct (xValue, xType) = x
                let struct (yValue, yType) = y
                obj.ReferenceEquals (xValue, yValue) && xType = yType
            member _.GetHashCode key =
                let struct (value, typeRef) = key
                (LanguagePrimitives.PhysicalHash value * 397) ^^^ typeRef.GetHashCode ()
        }

    /// <summary>
    /// The state of the input value rule for one document: the values already found coercible, and the number of errors
    /// found so far.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Every operation checks the argument values of the fragments it spreads, so the values of a fragment spread by
    /// thousands of operations would be checked thousands of times. Whether a value can be coerced does not depend on
    /// the operation, except through the default values of the variables used in it, so each value is checked once,
    /// and each operation checks only the variables used in it.
    /// </para>
    /// <para>
    /// The rule also stops once it has found more errors than validation reports.
    /// </para>
    /// </remarks>
    [<Sealed>]
    type private InputValueState
        /// <param name="schemaInfo">The schema the document is validated against.</param>
        /// <param name="maxErrors">The number of errors validation reports; the rule stops once it has found more.</param>
        (schemaInfo : SchemaInfo, maxErrors : int) =

        let variableUsages =
            Dictionary<struct (InputValue * IntrospectionTypeRef), struct (string * string) list voption> (valueTypeComparer)

        // Whether the value can be coerced to the type apart from the variables in it, which are added to the usages
        // with the name of the argument or input field each is used in, as the errors of checkInputValue name them
        let rec isCoercible (typeRef : IntrospectionTypeRef) (name : string) (value : InputValue) (usages : ResizeArray<struct (string * string)>) =
            match value with
            | NullValue -> typeRef.Kind <> TypeKind.NON_NULL
            | _ when typeRef.Kind = TypeKind.NON_NULL -> isCoercible typeRef.OfType.Value name value usages
            | IntValue _ ->
                match typeRef.Name, typeRef.Kind with
                | ValueSome ("ID" | "Int" | "Long" | "Float"), TypeKind.SCALAR -> true
                | _ -> false
            | FloatValue _ -> typeRef.Kind = TypeKind.SCALAR && typeRef.Name = ValueSome "Float"
            | BooleanValue _ -> typeRef.Kind = TypeKind.SCALAR && typeRef.Name = ValueSome "Boolean"
            | StringValue _ ->
                match typeRef.Name, typeRef.Kind with
                | ValueSome typeName, TypeKind.SCALAR -> not (Array.contains typeName nonStringScalars)
                | ValueSome typeName, TypeKind.INPUT_OBJECT -> typeName = FileType.Name
                | _ -> false
            | EnumValue _ -> typeRef.Kind = TypeKind.ENUM
            | ListValue values ->
                match typeRef.Kind with
                | TypeKind.LIST when typeRef.OfType.IsSome ->
                    values
                    |> List.forall (fun item -> isCoercible typeRef.OfType.Value name item usages)
                | _ -> false
            | ObjectValue props ->
                match typeRef.Kind with
                | TypeKind.OBJECT
                | TypeKind.INTERFACE
                | TypeKind.UNION
                | TypeKind.INPUT_OBJECT when typeRef.Name.IsSome ->
                    match schemaInfo.TryGetTypeByRef typeRef with
                    | ValueSome inputType ->
                        let fields = inputType.InputFields |> ValueOption.defaultValue [||]
                        fields
                        |> Array.forall (fun field -> field.Type.Kind <> TypeKind.NON_NULL || props.ContainsKey field.Name)
                        && props
                           |> Seq.forall (fun (KeyValue (fieldName, fieldValue)) ->
                               match fields |> Array.tryFind (fun field -> field.Name = fieldName) with
                               | Some field -> isCoercible field.Type fieldName fieldValue usages
                               | None -> false)
                    | ValueNone -> false
                | _ -> false
            | VariableName variableName ->
                usages.Add (struct (variableName, name))
                true

        member _.Schema = schemaInfo

        member val ErrorCount = 0 with get, set

        member this.IsOverBudget = this.ErrorCount > maxErrors

        /// The variables used in a value, each with the name of the argument or input field it is used in, when the value
        /// can be coerced to the type apart from them, and nothing when it cannot.
        member _.TryGetVariableUsages (typeRef : IntrospectionTypeRef, name : string, value : InputValue) =
            let key = struct (value, typeRef)
            match variableUsages.TryGetValue key with
            | true, found -> found
            | false, _ ->
                let usages = ResizeArray<struct (string * string)> ()
                let found = if isCoercible typeRef name value usages then ValueSome (List.ofSeq usages) else ValueNone
                variableUsages.Add (key, found)
                found

    /// Whether the default value of every variable used in a value can be coerced to the type of the variable, apart from
    /// the variables in it
    let private defaultValuesAreCoercible
        (state : InputValueState)
        (variables : IReadOnlyDictionary<string, VariableDefinition>)
        (usages : struct (string * string) list)
        =
        usages
        |> List.forall (fun struct (variableName, name) ->
            match variables.TryGetValue variableName with
            | true, definition when definition.DefaultValue.IsSome ->
                match state.Schema.TryGetInputType definition.Type with
                // A default value must be a constant: the variables in it are reported by
                // validateVariableDefaultValuesAreConstant and never followed, so only the rest of the value is checked
                | Some variableType ->
                    let usages = state.TryGetVariableUsages (variableType, name, definition.DefaultValue.Value)
                    usages.IsSome
                | None -> false
            | _ -> true)

    let private checkInputValue
        (state : InputValueState)
        (variables : IReadOnlyDictionary<string, VariableDefinition>)
        (selection : SelectionInfo)
        =
        let schemaInfo = state.Schema
        // Reports every error of the value in order: run only once the value or the default value of a variable used in
        // it is known not to coerce, so that a valid value is walked once per document instead of once per operation
        let rec checkIsCoercible (inDefaultValue : bool) (tref : IntrospectionTypeRef) (argName : string) (value : InputValue) =
            let canNotCoerce () =
                AstError.AsResult (
                    $"Argument field or value named '%s{argName}' can not be coerced. It does not match a valid literal representation for the type.",
                    selection.Path
                )
            match value with
            | NullValue when tref.Kind = TypeKind.NON_NULL ->
                AstError.AsResult (
                    $"Argument '%s{argName}' value can not be coerced. It's type is non-nullable but the argument has a null value.",
                    selection.Path
                )
            | NullValue -> Success
            | _ when tref.Kind = TypeKind.NON_NULL -> checkIsCoercible inDefaultValue tref.OfType.Value argName value
            | IntValue _ ->
                match tref.Name, tref.Kind with
                | ValueSome ("ID" | "Int" | "Long" | "Float"), TypeKind.SCALAR -> Success
                | _ -> canNotCoerce ()
            | FloatValue _ ->
                match tref.Name, tref.Kind with
                | ValueSome "Float", TypeKind.SCALAR -> Success
                | _ -> canNotCoerce ()
            | BooleanValue _ ->
                match tref.Name, tref.Kind with
                | ValueSome "Boolean", TypeKind.SCALAR -> Success
                | _ -> canNotCoerce ()
            | StringValue _ ->
                match tref.Name, tref.Kind with
                | (ValueSome x, TypeKind.SCALAR) when not (Array.contains x nonStringScalars) -> Success
                | (ValueSome x, TypeKind.INPUT_OBJECT) when x = FileType.Name -> Success
                | _ -> canNotCoerce ()
            | EnumValue _ ->
                match tref.Kind with
                | TypeKind.ENUM -> Success
                | _ -> canNotCoerce ()
            | ListValue values ->
                match tref.Kind with
                | TypeKind.LIST when tref.OfType.IsSome ->
                    values
                    |> ValidationResult.collect (checkIsCoercible inDefaultValue tref.OfType.Value argName)
                | _ -> canNotCoerce ()
            | ObjectValue props ->
                match tref.Kind with
                | TypeKind.OBJECT
                | TypeKind.INTERFACE
                | TypeKind.UNION
                | TypeKind.INPUT_OBJECT when tref.Name.IsSome ->
                    match schemaInfo.TryGetTypeByRef (tref) with
                    | ValueSome itype ->
                        let fieldMap =
                            itype.InputFields
                            |> ValueOption.defaultValue [||]
                            |> Array.fold (fun acc inputVal -> Map.add inputVal.Name inputVal.Type acc) Map.empty
                        let canCoerceFields =
                            fieldMap
                            |> ValidationResult.collect (fun kvp ->
                                if
                                    kvp.Value.Kind = TypeKind.NON_NULL
                                    && not (props.ContainsKey (kvp.Key))
                                then
                                    AstError.AsResult (
                                        $"Can not coerce argument '%s{argName}'. Argument definition '%s{tref.Name.Value}' have a required field '%s{kvp.Key}', but that field does not exist in the literal value for the argument.",
                                        selection.Path
                                    )
                                else
                                    Success)
                        let canCoerceProps =
                            props
                            |> ValidationResult.collect (fun kvp ->
                                match Map.tryFind kvp.Key fieldMap with
                                | Some fieldTypeRef -> checkIsCoercible inDefaultValue fieldTypeRef kvp.Key kvp.Value
                                | None ->
                                    AstError.AsResult (
                                        $"Can not coerce argument '%s{argName}'. The field '%s{kvp.Key}' is not a valid field in the argument definition.",
                                        selection.Path
                                    ))
                        canCoerceFields @@ canCoerceProps
                    | ValueNone -> canNotCoerce ()
                | _ -> canNotCoerce ()
            // A default value must be a constant, so a variable in it is reported by validateVariableDefaultValuesAreConstant
            // instead of followed: following it could loop forever, as in query ($a: Int = $a)
            | VariableName _ when inDefaultValue -> Success
            | VariableName varName ->
                match variables.TryGetValue varName with
                | true, vdef when vdef.DefaultValue.IsSome ->
                    match schemaInfo.TryGetInputType vdef.Type with
                    | Some vtype -> checkIsCoercible true vtype argName vdef.DefaultValue.Value
                    | None -> canNotCoerce ()
                | _ -> Success
        selection.Field.Arguments
        |> ValidationResult.collect (fun arg ->
            let argumentTypeRef =
                selection.InputValues
                |> Array.tryPick (fun x -> if x.Name = arg.Name then Some x.Type else None)
            match argumentTypeRef with
            | Some _ when state.IsOverBudget -> Success
            | Some argumentTypeRef ->
                match state.TryGetVariableUsages (argumentTypeRef, arg.Name, arg.Value) with
                | ValueSome usages when defaultValuesAreCoercible state variables usages -> Success
                | _ ->
                    match checkIsCoercible false argumentTypeRef arg.Name arg.Value with
                    | Success -> Success
                    | ValidationError errors as result ->
                        state.ErrorCount <- state.ErrorCount + errors.Length
                        result
            | None -> Success)

    /// The variables of an operation by name, the first definition of a name winning as in the rest of validation
    let private variablesByName (definitions : VariableDefinition list) : IReadOnlyDictionary<string, VariableDefinition> =
        let variables = Dictionary<string, VariableDefinition> (StringComparer.Ordinal)
        for definition in definitions do
            if not (variables.ContainsKey definition.VariableName) then
                variables.Add (definition.VariableName, definition)
        variables

    /// The input value rule, stopping once it has found more than the given number of errors.
    let internal validateInputValuesWithin (maxErrors : int) (ctx : ValidationContext) =
        let state = InputValueState (ctx.Schema, maxErrors)
        let noVariables = variablesByName []
        ctx.Definitions
        |> ValidationResult.collect (fun def ->
            let struct (variables, selectionSet) =
                match def with
                | OperationDefinitionInfo odef -> struct (variablesByName odef.Definition.VariableDefinitions, odef.SelectionSet)
                | FragmentDefinitionInfo fdef -> struct (noVariables, fdef.SelectionSet)
            selectionSet
            |> ValidationResult.collect (checkInputValue state variables))

    let internal validateInputValues (ctx : ValidationContext) =
        validateInputValuesWithin DocumentLimitsDefaults.MaxValidationErrors ctx

    let rec private getDistinctDirectiveNamesInSelection (path : FieldPath) (selection : Selection) : (FieldPath * Set<string>) list =
        match selection with
        | Field field ->
            let path = box field.AliasOrName :: path
            let fieldDirectives = [ path, field.Directives |> Seq.map _.Name |> Set.ofSeq ]
            let selectionSetDirectives =
                field.SelectionSet
                |> List.collect (getDistinctDirectiveNamesInSelection path)
            fieldDirectives |> List.append selectionSetDirectives
        | InlineFragment frag -> getDistinctDirectiveNamesInDefinition path (FragmentDefinition frag)
        | FragmentSpread spread -> [
            path,
            spread.Directives
            |> Seq.map _.Name
            |> Set.ofSeq
          ]

    and private getDistinctDirectiveNamesInDefinition (path : FieldPath) (frag : Definition) : (FieldPath * Set<string>) list =
        let fragDirectives = [ path, frag.Directives |> Seq.map _.Name |> Set.ofSeq ]
        let selectionSetDirectives =
            frag.SelectionSet
            |> List.collect (getDistinctDirectiveNamesInSelection path)
        fragDirectives @ selectionSetDirectives

    let internal validateDirectivesDefined (ctx : ValidationContext) =
        ctx.Definitions
        |> List.collect (fun def ->
            let path =
                match def.Name with
                | ValueSome name -> [ box name ]
                | ValueNone -> []
            getDistinctDirectiveNamesInDefinition path def.Definition)
        |> ValidationResult.collect (fun (path, names) ->
            names
            |> ValidationResult.collect (fun name ->
                if
                    ctx.Schema.Directives
                    |> Array.exists (fun x -> x.Name = name)
                then
                    Success
                else
                    AstError.AsResult ($"Directive '%s{name}' is not defined in the schema.", path)))

    let private validateDirective
        (schemaInfo : SchemaInfo)
        (path : FieldPath)
        (location : DirectiveLocation)
        (onError : Directive -> string)
        (directive : Directive)
        =
        schemaInfo.Directives
        |> ValidationResult.collect (fun d ->
            if d.Name = directive.Name then
                if d.Locations |> Array.contains location then
                    Success
                else
                    AstError.AsResult (onError directive, path)
            else
                Success)

    type private InlineFragmentContext = {
        Schema : SchemaInfo
        FragmentDefinitions : FragmentDefinition list
        Path : FieldPath
        Directives : Directive list
        SelectionSet : Selection list
    }

    let rec private checkDirectivesInValidLocationOnInlineFragment (ctx : InlineFragmentContext) =
        let directivesValid =
            ctx.Directives
            |> ValidationResult.collect (
                validateDirective ctx.Schema ctx.Path DirectiveLocation.INLINE_FRAGMENT (fun d ->
                    $"An inline fragment has a directive '%s{d.Name}', but this directive location is not supported by the schema definition.")
            )
        let directivesValidInSelectionSet =
            ctx.SelectionSet
            |> ValidationResult.collect (checkDirectivesInValidLocationOnSelection ctx.Schema ctx.FragmentDefinitions ctx.Path)
        directivesValid @@ directivesValidInSelectionSet

    and private checkDirectivesInValidLocationOnSelection
        (schemaInfo : SchemaInfo)
        (fragmentDefinitions : FragmentDefinition list)
        (path : FieldPath)
        =
        function
        | Field field ->
            let path = box field.AliasOrName :: path
            let directivesValid =
                field.Directives
                |> ValidationResult.collect (
                    validateDirective schemaInfo path DirectiveLocation.FIELD (fun directiveDef ->
                        $"Field or alias '%s{field.AliasOrName}' has a directive '%s{directiveDef.Name}', but this directive location is not supported by the schema definition.")
                )
            let directivesValidInSelectionSet =
                field.SelectionSet
                |> ValidationResult.collect (checkDirectivesInValidLocationOnSelection schemaInfo fragmentDefinitions path)
            directivesValid @@ directivesValidInSelectionSet
        | InlineFragment frag ->
            let fragCtx = {
                Schema = schemaInfo
                FragmentDefinitions = fragmentDefinitions
                Path = path
                Directives = frag.Directives
                SelectionSet = frag.SelectionSet
            }
            checkDirectivesInValidLocationOnInlineFragment fragCtx
        | _ -> Success // We don't validate spreads here, they are being validated in another function

    type private FragmentSpreadContext = {
        Schema : SchemaInfo
        FragmentDefinitions : FragmentDefinition list
        Path : FieldPath
        FragmentName : string
        Directives : Directive list
        SelectionSet : Selection list
    }

    let rec private checkDirectivesInValidLocationOnFragmentSpread (ctx : FragmentSpreadContext) =
        let directivesValid =
            ctx.Directives
            |> ValidationResult.collect (
                validateDirective ctx.Schema ctx.Path DirectiveLocation.FRAGMENT_SPREAD (fun d ->
                    $"Fragment '%s{ctx.FragmentName}' has a directive '%s{d.Name}', but this directive location is not supported by the schema definition.")
            )
        let directivesValidInSelectionSet =
            ctx.SelectionSet
            |> ValidationResult.collect (checkDirectivesInValidLocationOnSelection ctx.Schema ctx.FragmentDefinitions ctx.Path)
        directivesValid @@ directivesValidInSelectionSet

    let private checkDirectivesInOperation
        (schemaInfo : SchemaInfo)
        (fragmentDefinitions : FragmentDefinition list)
        (path : FieldPath)
        (operation : OperationDefinition)
        =
        let expectedLocation =
            match operation.OperationType with
            | Query -> DirectiveLocation.QUERY
            | Mutation -> DirectiveLocation.MUTATION
            | Subscription -> DirectiveLocation.SUBSCRIPTION
        let directivesValid =
            operation.Directives
            |> ValidationResult.collect (
                validateDirective schemaInfo path expectedLocation (fun directiveDef ->
                    match operation.Name with
                    | ValueSome operationName ->
                        $"%s{operation.OperationType.ToString ()} operation '%s{operationName}' has a directive '%s{directiveDef.Name}', but this directive location is not supported by the schema definition."
                    | ValueNone ->
                        $"This %s{operation.OperationType.ToString ()} operation has a directive '%s{directiveDef.Name}', but this directive location is not supported by the schema definition.")
            )
        let directivesValidInSelectionSet =
            operation.SelectionSet
            |> ValidationResult.collect (checkDirectivesInValidLocationOnSelection schemaInfo fragmentDefinitions path)
        directivesValid @@ directivesValidInSelectionSet

    let internal validateDirectivesAreInValidLocations (ctx : ValidationContext) =
        let fragmentDefinitions = ctx.FragmentDefinitions |> List.map _.Definition
        ctx.Document.Definitions
        |> ValidationResult.collect (fun def ->
            let path = def.Name |> ValueOption.map box |> ValueOption.toList
            match def with
            | OperationDefinition odef -> checkDirectivesInOperation ctx.Schema fragmentDefinitions path odef
            | FragmentDefinition frag when frag.Name.IsSome ->
                let fragCtx = {
                    Schema = ctx.Schema
                    FragmentDefinitions = fragmentDefinitions
                    Path = path
                    FragmentName = frag.Name.Value
                    Directives = frag.Directives
                    SelectionSet = frag.SelectionSet
                }
                checkDirectivesInValidLocationOnFragmentSpread fragCtx
            | _ -> Success)

    let rec private getDirectiveNamesInSelection (path : FieldPath) (selection : Selection) : (FieldPath * string list) list =
        match selection with
        | Field field ->
            let path = box field.AliasOrName :: path
            let fieldDirectives = [ path, field.Directives |> List.map _.Name ]
            let selectionSetDirectives =
                field.SelectionSet
                |> List.collect (getDirectiveNamesInSelection path)
            fieldDirectives |> List.append selectionSetDirectives
        | InlineFragment frag -> getDirectiveNamesInDefinition path (FragmentDefinition frag)
        | FragmentSpread spread -> [ path, spread.Directives |> List.map _.Name ]

    and private getDirectiveNamesInDefinition (path : FieldPath) (frag : Definition) : (FieldPath * string list) list =
        let fragDirectives = [ path, frag.Directives |> List.map _.Name ]
        let selectionSetDirectives =
            frag.SelectionSet
            |> List.collect (getDirectiveNamesInSelection path)
        fragDirectives |> List.append selectionSetDirectives

    let internal validateUniqueDirectivesPerLocation (ctx : ValidationContext) =
        ctx.Definitions
        |> List.collect (fun def ->
            let path =
                match def.Name with
                | ValueSome name -> [ box name ]
                | ValueNone -> []
            let defDirectives = path, def.Directives |> List.map _.Name
            let selectionSetDirectives =
                def.Definition.SelectionSet
                |> List.collect (getDirectiveNamesInSelection path)
            defDirectives :: selectionSetDirectives)
        |> ValidationResult.collect (fun (path, directives) ->
            directives
            |> Seq.countBy id
            |> ValidationResult.collect (fun (name, count) ->
                if count <= 1 then
                    Success
                else
                    AstError.AsResult (
                        $"Directive '%s{name}' appears %i{count} times in the location it is used. Directives must be unique in their locations.",
                        path
                    )))

    let internal validateVariableUniqueness (ctx : ValidationContext) =
        ctx.Document.Definitions
        |> ValidationResult.collect (function
            | OperationDefinition def ->
                def.VariableDefinitions
                |> List.countBy id
                |> ValidationResult.collect (fun (var, count) ->
                    match def.Name with
                    | _ when count < 2 -> Success
                    | ValueSome operationName ->
                        AstError.AsResult
                            $"A variable '$%s{var.VariableName}' in operation '%s{operationName}' is declared %i{count} times. Variables must be unique in their operations."
                    | ValueNone ->
                        AstError.AsResult
                            $"A variable '$%s{var.VariableName}' is declared %i{count} times in the operation. Variables must be unique in their operations.")
            | _ -> Success)

    let internal validateVariablesAsInputTypes (ctx : ValidationContext) =
        ctx.Document.Definitions
        |> ValidationResult.collect (function
            | OperationDefinition def ->
                def.VariableDefinitions
                |> ValidationResult.collect (fun var ->
                    match def.Name, ctx.Schema.TryGetInputType (var.Type) with
                    | ValueSome operationName, None ->
                        AstError.AsResult (
                            $"A variable '$%s{var.VariableName}' in operation '%s{operationName}' has a type that is not an input type defined by the schema (%s{var.Type.ToString ()})."
                        )
                    | ValueNone, None ->
                        AstError.AsResult (
                            $"A variable '$%s{var.VariableName}' has a type is not an input type defined by the schema (%s{var.Type.ToString ()})."
                        )
                    | _ -> Success)
            | _ -> Success)

    /// Whether the value holds a variable, walked without recursion
    let private holdsVariable (value : InputValue) =
        let pending = Stack<InputValue> ()
        pending.Push value
        let mutable found = false
        while not found && pending.Count > 0 do
            match pending.Pop () with
            | VariableName _ -> found <- true
            | ObjectValue fields ->
                for KeyValue (_, field) in fields do
                    pending.Push field
            | ListValue items ->
                for item in items do
                    pending.Push item
            | _ -> ()
        found

    /// <summary>
    /// Reports the variables whose default value uses a variable.
    /// </summary>
    /// <remarks>
    /// The grammar of GraphQL only allows constants as default values, but the parser accepts variables in them. The other
    /// rules never follow such a variable: following it could loop forever, as in <c>query ($a: Int = $a)</c>.
    /// </remarks>
    let internal validateVariableDefaultValuesAreConstant (ctx : ValidationContext) =
        ctx.Document.Definitions
        |> ValidationResult.collect (function
            | OperationDefinition def ->
                def.VariableDefinitions
                |> ValidationResult.collect (fun var ->
                    match var.DefaultValue, def.Name with
                    | Some value, ValueSome operationName when holdsVariable value ->
                        AstError.AsResult
                            $"The default value of variable '$%s{var.VariableName}' in operation '%s{operationName}' uses a variable. Default values must be constants."
                    | Some value, ValueNone when holdsVariable value ->
                        AstError.AsResult
                            $"The default value of variable '$%s{var.VariableName}' uses a variable. Default values must be constants."
                    | _ -> Success)
            | _ -> Success)

    let private checkVariablesDefinedInDirective (variableDefinitions : Set<string>) (path : FieldPath) (directive : Directive) =
        directive.Arguments
        |> ValidationResult.collect (fun arg ->
            match arg.Value with
            | VariableName varName ->
                if variableDefinitions |> Set.contains varName then
                    Success
                else
                    AstError.AsResult (
                        $"A variable '%s{varName}' is referenced in an argument '%s{arg.Name}' of directive '%s{directive.Name}' of field with alias or name '%O{path.Head}', but that variable is not defined in the operation.",
                        path
                    )
            | _ -> Success)

    let rec private checkVariablesDefinedInSelection
        (fragmentDefinitions : FragmentDefinition list)
        (variableDefinitions : Set<string>)
        (path : FieldPath)
        =
        function
        | Field field ->
            let path = box field.AliasOrName :: path
            let variablesValid =
                field.Arguments
                |> ValidationResult.collect (fun arg ->
                    match arg.Value with
                    | VariableName varName ->
                        if variableDefinitions |> Set.contains varName then
                            Success
                        else
                            AstError.AsResult (
                                $"A variable '$%s{varName}' is referenced in argument '%s{arg.Name}' of field with alias or name '%s{field.AliasOrName}', but that variable is not defined in the operation."
                            )
                    | _ -> Success)
            variablesValid
            @@ (field.SelectionSet
                |> ValidationResult.collect (checkVariablesDefinedInSelection fragmentDefinitions variableDefinitions path))
            @@ (field.Directives
                |> ValidationResult.collect (checkVariablesDefinedInDirective variableDefinitions path))
        | InlineFragment frag ->
            let variablesValid =
                frag.SelectionSet
                |> ValidationResult.collect (checkVariablesDefinedInSelection fragmentDefinitions variableDefinitions path)
            variablesValid
            @@ (frag.Directives
                |> ValidationResult.collect (checkVariablesDefinedInDirective variableDefinitions path))
        | _ -> Success // Spreads can't have variable definitions

    let internal validateVariablesUsesDefined (ctx : ValidationContext) =
        let fragmentDefinitions = getFragmentDefinitions ctx.Document
        ctx.Document.Definitions
        |> ValidationResult.collect (function
            | OperationDefinition def ->
                let path = def.Name |> ValueOption.map box |> ValueOption.toList
                let varNames =
                    def.VariableDefinitions
                    |> Seq.map _.VariableName
                    |> Set.ofSeq
                def.SelectionSet
                |> ValidationResult.collect (checkVariablesDefinedInSelection fragmentDefinitions varNames path)
            | _ -> Success)

    /// <summary>
    /// The names of the variables a selection set uses itself, in the arguments and directives of its selections and in
    /// the given directives, and the names of the fragments it spreads, whose variables are not followed.
    /// </summary>
    /// <remarks>
    /// One iterative pass, which does not depend on the number of variables: searching for each variable separately is
    /// quadratic in the size of the document.
    /// </remarks>
    let private getOwnVariableUsages (directives : Directive list) (selectionSet : Selection list) =
        let used = HashSet<string> (StringComparer.Ordinal)
        let spreads = ResizeArray<string> ()
        let pendingValues = Stack<InputValue> ()
        let pendingSelectionSets = Stack<Selection list> ()
        let addDirectives (directives : Directive list) =
            for directive in directives do
                for argument in directive.Arguments do
                    pendingValues.Push argument.Value
        addDirectives directives
        pendingSelectionSets.Push selectionSet
        while pendingSelectionSets.Count > 0 do
            for selection in pendingSelectionSets.Pop () do
                match selection with
                | Field field ->
                    for argument in field.Arguments do
                        pendingValues.Push argument.Value
                    addDirectives field.Directives
                    pendingSelectionSets.Push field.SelectionSet
                | InlineFragment fragment ->
                    addDirectives fragment.Directives
                    pendingSelectionSets.Push fragment.SelectionSet
                | FragmentSpread spread ->
                    addDirectives spread.Directives
                    spreads.Add spread.Name
            while pendingValues.Count > 0 do
                match pendingValues.Pop () with
                | VariableName name -> used.Add name |> ignore
                | ObjectValue fields ->
                    for KeyValue (_, value) in fields do
                        pendingValues.Push value
                | ListValue values ->
                    for value in values do
                        pendingValues.Push value
                | _ -> ()
        struct (used, spreads)

    /// <summary>
    /// The names of the variables used in the selection set of an operation, following its fragment spreads.
    /// </summary>
    /// <remarks>
    /// Each fragment is followed once, even when it is part of a cycle, without recursion. The variables a fragment uses
    /// itself are found once per document: walking the values of a fragment for every operation that spreads it is
    /// multiplicative in the number of operations and the size of the fragment.
    /// </remarks>
    let private getUsedVariables
        (fragmentUsages : string -> struct (HashSet<string> * ResizeArray<string>) voption)
        (selectionSet : Selection list)
        =
        let struct (used, spreads) = getOwnVariableUsages [] selectionSet
        let followedFragments = HashSet<string> (StringComparer.Ordinal)
        let pendingFragments = Stack<string> (spreads)
        while pendingFragments.Count > 0 do
            let name = pendingFragments.Pop ()
            if followedFragments.Add name then
                match fragmentUsages name with
                | ValueSome (struct (fragmentUsed, fragmentSpreads)) ->
                    used.UnionWith fragmentUsed
                    for spread in fragmentSpreads do
                        pendingFragments.Push spread
                | ValueNone -> ()
        used

    let internal validateAllVariablesUsed (ctx : ValidationContext) =
        let fragments =
            getFragmentDefinitions ctx.Document
            |> getFragmentsByName
        let ownUsagesOfFragments = Dictionary<string, struct (HashSet<string> * ResizeArray<string>)> (StringComparer.Ordinal)
        let fragmentUsages (name : string) =
            match ownUsagesOfFragments.TryGetValue name with
            | true, usages -> ValueSome usages
            | false, _ ->
                match fragments.TryGetValue name with
                | true, fragment ->
                    let usages = getOwnVariableUsages fragment.Directives fragment.SelectionSet
                    ownUsagesOfFragments.Add (name, usages)
                    ValueSome usages
                | false, _ -> ValueNone
        ctx.Document.Definitions
        |> ValidationResult.collect (function
            | OperationDefinition def ->
                let usedVariables = getUsedVariables fragmentUsages def.SelectionSet
                def.VariableDefinitions
                |> ValidationResult.collect (fun varDef ->
                    let isUsed = usedVariables.Contains varDef.VariableName
                    match def.Name, isUsed with
                    | _, true -> Success
                    | ValueSome operationName, _ ->
                        AstError.AsResult
                            $"A variable '$%s{varDef.VariableName}' is not used in operation '%s{operationName}'. Every variable must be used."
                    | ValueNone, _ ->
                        AstError.AsResult $"A variable '$%s{varDef.VariableName}' is not used in operation. Every variable must be used.")
            | _ -> Success)

    let rec private areTypesCompatible (variableTypeRef : IntrospectionTypeRef) (locationTypeRef : IntrospectionTypeRef) =
        if
            locationTypeRef.Kind = TypeKind.NON_NULL
            && locationTypeRef.OfType.IsSome
        then
            if variableTypeRef.Kind <> TypeKind.NON_NULL then
                false
            elif variableTypeRef.OfType.IsSome then
                areTypesCompatible variableTypeRef.OfType.Value locationTypeRef.OfType.Value
            else
                false
        elif
            variableTypeRef.Kind = TypeKind.NON_NULL
            && variableTypeRef.OfType.IsSome
        then
            areTypesCompatible variableTypeRef.OfType.Value locationTypeRef
        elif
            locationTypeRef.Kind = TypeKind.LIST
            && locationTypeRef.OfType.IsSome
        then
            if variableTypeRef.Kind <> TypeKind.LIST then
                false
            elif variableTypeRef.OfType.IsSome then
                areTypesCompatible variableTypeRef.OfType.Value locationTypeRef.OfType.Value
            else
                false
        elif variableTypeRef.Kind = TypeKind.LIST then
            false
        else
            variableTypeRef.Name = locationTypeRef.Name
            && variableTypeRef.Kind = locationTypeRef.Kind

    let private checkVariableUsageAllowedOnArguments
        (inputs : IntrospectionInputVal[])
        (varNamesAndTypeRefs : Map<string, VariableDefinition * IntrospectionTypeRef>)
        (path : FieldPath)
        (args : Argument list)
        =
        args
        |> ValidationResult.collect (fun arg ->
            match arg.Value with
            | VariableName varName ->
                match varNamesAndTypeRefs.TryFind (varName) with
                | Some (varDef, variableTypeRef) ->
                    let err =
                        AstError.AsResult (
                            $"A variable '$%s{varName}' can not be used in its reference. The type of the variable definition is not compatible with the type of its reference.",
                            path
                        )
                    match inputs |> Array.vtryFind (fun x -> x.Name = arg.Name) with
                    | ValueSome input ->
                        let locationTypeRef = input.Type
                        if
                            locationTypeRef.Kind = TypeKind.NON_NULL
                            && locationTypeRef.OfType.IsSome
                            && variableTypeRef.Kind <> TypeKind.NON_NULL
                        then
                            let hasNonNullVariableDefaultValue = varDef.DefaultValue.IsSome
                            let hasLocationDefaultValue = input.DefaultValue.IsSome
                            if
                                not hasNonNullVariableDefaultValue
                                && not hasLocationDefaultValue
                            then
                                err
                            else
                                let nullableLocationType = locationTypeRef.OfType.Value
                                if not (areTypesCompatible variableTypeRef nullableLocationType) then
                                    err
                                else
                                    Success
                        elif not (areTypesCompatible variableTypeRef locationTypeRef) then
                            err
                        else
                            Success
                    | ValueNone -> Success
                | None -> Success
            | _ -> Success)

    let rec private checkVariableUsageAllowedOnSelection
        (varNamesAndTypeRefs : Map<string, VariableDefinition * IntrospectionTypeRef>)
        (visitedFragments : string list)
        (selection : SelectionInfo)
        =
        match selection.FragmentSpreadName with
        | ValueSome spreadName when List.contains spreadName visitedFragments -> Success
        | _ ->
            let visitedFragments =
                match selection.FragmentSpreadName with
                | ValueSome _ -> selection.FragmentSpreadName.Value :: visitedFragments
                | ValueNone -> visitedFragments
            match selection.FieldType with
            | ValueSome _ ->
                let argumentsValid =
                    selection.Field.Arguments
                    |> checkVariableUsageAllowedOnArguments selection.InputValues varNamesAndTypeRefs selection.Path
                let selectionValid =
                    selection.SelectionSet
                    |> ValidationResult.collect (checkVariableUsageAllowedOnSelection varNamesAndTypeRefs visitedFragments)
                let directivesValid =
                    selection.Field.Directives
                    |> ValidationResult.collect (fun directive ->
                        directive.Arguments
                        |> checkVariableUsageAllowedOnArguments selection.InputValues varNamesAndTypeRefs selection.Path)
                argumentsValid @@ selectionValid @@ directivesValid
            | ValueNone -> Success

    let internal validateVariableUsagesAllowed (ctx : ValidationContext) =
        ctx.OperationDefinitions
        |> ValidationResult.collect (fun def ->
            let varNamesAndTypeRefs =
                def.Definition.VariableDefinitions
                |> List.vchoose (fun varDef -> voption {
                    let! t = ctx.Schema.TryGetInputType (varDef.Type)
                    return varDef.VariableName, (varDef, t)
                })
                |> Map.ofList
            def.SelectionSet
            |> ValidationResult.collect (checkVariableUsageAllowedOnSelection varNamesAndTypeRefs []))

    let private isIncrementalDirective (directive : Directive) = directive.Name = "defer" || directive.Name = "stream"

    /// <summary>
    /// An <c>@defer</c> or <c>@stream</c> disabled with a literal <c>if: false</c> is allowed anywhere, since it never applies.
    /// </summary>
    let private isDisabledIncrementalDirective (directive : Directive) =
        directive.Arguments
        |> List.exists (fun argument -> argument.Name = "if" && argument.Value = BooleanValue false)

    /// <summary>
    /// The <c>@defer</c> and <c>@stream</c> directives used in the selection set, each with the (reversed) path of the
    /// selection carrying it; fragment spreads are followed only when asked to, so a fragment definition validated on
    /// its own is not counted twice.
    /// </summary>
    let rec private incrementalDirectiveUsages
        (fragments : Dictionary<string, FragmentDefinition>)
        (followSpreads : bool)
        (visitedFragments : string list)
        (path : FieldPath)
        (selectionSet : Selection list)
        : (FieldPath * Directive) list =
        let usagesOf (directives : Directive list) (path : FieldPath) =
            directives
            |> List.filter isIncrementalDirective
            |> List.map (fun directive -> path, directive)
        selectionSet
        |> List.collect (function
            | Field field ->
                let fieldPath = box field.AliasOrName :: path
                usagesOf field.Directives fieldPath
                @ incrementalDirectiveUsages fragments followSpreads visitedFragments fieldPath field.SelectionSet
            | InlineFragment fragment ->
                usagesOf fragment.Directives path
                @ incrementalDirectiveUsages fragments followSpreads visitedFragments path fragment.SelectionSet
            | FragmentSpread spread ->
                let own = usagesOf spread.Directives path
                if followSpreads && not (visitedFragments |> List.contains spread.Name) then
                    match fragments.TryGetValue spread.Name with
                    | true, fragment ->
                        own
                        @ incrementalDirectiveUsages fragments followSpreads (spread.Name :: visitedFragments) path fragment.SelectionSet
                    | false, _ -> own
                else
                    own)

    /// <summary>
    /// The <c>@defer</c> and <c>@stream</c> directives applied to the root fields of the selection set, through the
    /// fragments spread at its root.
    /// </summary>
    let rec private rootIncrementalDirectiveUsages
        (fragments : Dictionary<string, FragmentDefinition>)
        (visitedFragments : string list)
        (selectionSet : Selection list)
        : (FieldPath * Directive) list =
        selectionSet
        |> List.collect (function
            | Field field ->
                field.Directives
                |> List.filter isIncrementalDirective
                |> List.map (fun directive -> [ box field.AliasOrName ], directive)
            | InlineFragment fragment -> rootIncrementalDirectiveUsages fragments visitedFragments fragment.SelectionSet
            | FragmentSpread spread when not (visitedFragments |> List.contains spread.Name) ->
                match fragments.TryGetValue spread.Name with
                | true, fragment -> rootIncrementalDirectiveUsages fragments (spread.Name :: visitedFragments) fragment.SelectionSet
                | false, _ -> []
            | FragmentSpread _ -> [])

    /// <summary>
    /// The <c>@stream</c> directive may only be applied to list fields
    /// (<see href="https://github.com/graphql/graphql-spec/pull/1110">Stream Directives Are Used On List Fields</see>).
    /// </summary>
    let internal validateStreamDirectiveOnListFields (ctx : ValidationContext) =
        let rec isList (typeRef : IntrospectionTypeRef) =
            match typeRef.Kind with
            | TypeKind.LIST -> true
            | TypeKind.NON_NULL -> typeRef.OfType |> ValueOption.exists isList
            | _ -> false
        onAllSelections ctx (fun selection ->
            if selection.Field.Directives |> List.exists (fun directive -> directive.Name = "stream") then
                match selection.FieldType with
                | ValueSome fieldType when isList fieldType -> Success
                | _ ->
                    AstError.AsResult (
                        $"Directive 'stream' on field '%s{selection.Field.Name}' of type '%s{selection.FragmentOrParentType.Name}' must be applied to a list field.",
                        selection.Path
                    )
            else
                Success)

    /// <summary>
    /// <c>@defer</c> and <c>@stream</c> are not allowed in subscription operations, unless disabled with <c>if: false</c>
    /// (<see href="https://github.com/graphql/graphql-spec/pull/1110">Defer And Stream Directives Are Used On Valid Operations</see>).
    /// </summary>
    let internal validateDeferStreamDirectivesOnValidOperations (ctx : ValidationContext) =
        let fragments =
            getFragmentDefinitions ctx.Document
            |> getInlinableFragmentDefinitions
            |> getFragmentsByName
        ctx.Document.Definitions
        |> ValidationResult.collect (function
            | OperationDefinition def when def.OperationType = Subscription ->
                incrementalDirectiveUsages fragments true [] [] def.SelectionSet
                |> List.filter (fun (_, directive) -> not (isDisabledIncrementalDirective directive))
                |> ValidationResult.collect (fun (path, directive) ->
                    AstError.AsResult (
                        $"Directive '%s{directive.Name}' is not allowed in a subscription operation. Disable it with `if: false` instead.",
                        path
                    ))
            | _ -> Success)

    /// <summary>
    /// <c>@defer</c> and <c>@stream</c> cannot be applied to the root fields of a mutation, which are executed serially
    /// (<see href="https://github.com/graphql/graphql-spec/pull/1110">Defer And Stream Directives Are Used On Valid Root Field</see>).
    /// </summary>
    let internal validateDeferStreamDirectivesOnRootFields (ctx : ValidationContext) =
        let fragments =
            getFragmentDefinitions ctx.Document
            |> getInlinableFragmentDefinitions
            |> getFragmentsByName
        let mutationTypeName =
            ctx.Schema.MutationType
            |> ValueOption.map _.Name
            |> ValueOption.defaultValue "Mutation"
        ctx.Document.Definitions
        |> ValidationResult.collect (function
            | OperationDefinition def when def.OperationType = Mutation ->
                rootIncrementalDirectiveUsages fragments [] def.SelectionSet
                |> List.filter (fun (_, directive) -> not (isDisabledIncrementalDirective directive))
                |> ValidationResult.collect (fun (path, directive) ->
                    AstError.AsResult (
                        $"Directive '%s{directive.Name}' cannot be applied to a root field of the mutation type '%s{mutationTypeName}'.",
                        path
                    ))
            | _ -> Success)

    /// <summary>
    /// The <c>label</c> of <c>@defer</c> and <c>@stream</c> must be a string literal, and unique within each operation,
    /// counting the fragments the operation spreads
    /// (<see href="https://github.com/graphql/graphql-spec/pull/1110">Defer And Stream Directive Labels Are Unique</see>).
    /// </summary>
    let internal validateDeferStreamDirectiveLabels (ctx : ValidationContext) =
        let fragments =
            getFragmentDefinitions ctx.Document
            |> getInlinableFragmentDefinitions
            |> getFragmentsByName
        let labelOf (directive : Directive) =
            directive.Arguments
            |> List.vtryFind (fun argument -> argument.Name = "label")
            |> ValueOption.map _.Value
        // A variable label is rejected wherever it is written, a fragment definition included
        let literalErrors =
            ctx.Document.Definitions
            |> List.collect (fun def -> incrementalDirectiveUsages fragments false [] [] def.SelectionSet)
            |> ValidationResult.collect (fun (path, directive) ->
                match labelOf directive with
                | ValueSome (VariableName _) ->
                    AstError.AsResult ($"Argument 'label' of directive '%s{directive.Name}' must be a string literal, not a variable.", path)
                | _ -> Success)
        // Labels are unique per operation, over the fragments the operation reaches: two operations may reuse a label
        let uniquenessErrors =
            ctx.Document.Definitions
            |> ValidationResult.collect (function
                | OperationDefinition def ->
                    let seenLabels = HashSet<string> ()
                    incrementalDirectiveUsages fragments true [] [] def.SelectionSet
                    |> ValidationResult.collect (fun (path, directive) ->
                        match labelOf directive with
                        | ValueSome (StringValue label) when not (seenLabels.Add label) ->
                            AstError.AsResult (
                                $"Label '%s{label}' of directive '%s{directive.Name}' is used more than once. Defer and stream labels must be unique in an operation.",
                                path
                            )
                        | _ -> Success)
                | _ -> Success)
        literalErrors @@ uniquenessErrors

    /// The rules, in the order they run; the rules that can find more errors than the size of the document stop once
    /// they have found more than <c>maxErrors</c>.
    let private allValidations (maxErrors : int) = [
        validateFragmentsMustNotFormCycles
        validateOperationNameUniqueness
        validateLoneAnonymousOperation
        validateSubscriptionSingleRootField
        validateSelectionFieldTypes
        validateFieldSelectionMergingWithin maxErrors
        validateLeafFieldSelections
        validateArgumentNames
        validateArgumentUniqueness
        validateRequiredArguments
        validateFragmentNameUniqueness
        validateFragmentTypeExistence
        validateFragmentsOnCompositeTypes
        validateFragmentsMustBeUsed
        validateFragmentSpreadTargetDefined
        validateFragmentSpreadIsPossible
        validateInputValuesWithin maxErrors
        validateDirectivesDefined
        validateDirectivesAreInValidLocations
        validateUniqueDirectivesPerLocation
        validateStreamDirectiveOnListFields
        validateDeferStreamDirectivesOnValidOperations
        validateDeferStreamDirectivesOnRootFields
        validateDeferStreamDirectiveLabels
        validateVariableUniqueness
        validateVariablesAsInputTypes
        validateVariableDefaultValuesAreConstant
        validateVariablesUsesDefined
        validateAllVariablesUsed
        validateVariableUsagesAllowed
    ]

    /// <summary>
    /// Runs all validations against the document, unless the document exceeds the size limits,
    /// and stops once more than <paramref name="maxErrors"/> errors are found.
    /// </summary>
    /// <remarks>
    /// Like graphql-js, the result then holds the first <paramref name="maxErrors"/> errors followed by an error saying
    /// that validation was aborted.
    /// </remarks>
    let internal validateDocumentWithLimits
        (maxRecursiveSelections : int)
        (maxNestingDepth : int)
        (maxErrors : int)
        (schema : IntrospectionSchema)
        (ast : Document)
        =
        match checkDocumentLimits maxRecursiveSelections maxNestingDepth ast with
        | ValidationError _ as limitExceeded -> limitExceeded
        | Success ->
            let schemaInfo = SchemaInfo.FromIntrospectionSchema (schema)
            let context = getValidationContext schemaInfo ast
            let errors = ResizeArray<GQLProblemDetails> ()
            let mutable failed = false
            let mutable validations = allValidations maxErrors
            while not validations.IsEmpty && errors.Count <= maxErrors do
                match validations.Head context with
                | Success -> ()
                | ValidationError validationErrors ->
                    failed <- true
                    errors.AddRange validationErrors
                validations <- validations.Tail
            if not failed then
                Success
            elif errors.Count > maxErrors then
                ValidationError [
                    yield! Seq.take maxErrors errors
                    AstError.Create "Too many validation errors, error limit reached. Validation aborted."
                ]
            else
                ValidationError (List.ofSeq errors)

    /// Run all available Ast validations against the given Document and IntrospectionSchema
    let validateDocument (schema : IntrospectionSchema) (ast : Document) =
        validateDocumentWithLimits
            DocumentLimitsDefaults.MaxRecursiveSelections
            DocumentLimitsDefaults.MaxNestingDepth
            DocumentLimitsDefaults.MaxValidationErrors
            schema
            ast
