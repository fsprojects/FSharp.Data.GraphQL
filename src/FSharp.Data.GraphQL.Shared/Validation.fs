// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Validation

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

    let validateImplements (objdef : ObjectDef) (idef : InterfaceDef) =
        let objectFields = objdef.Fields
        let errors =
            idef.Fields
            |> Array.fold
                (fun acc f ->
                    match Map.tryFind f.Name objectFields with
                    | None ->
                        $"'%s{f.Name}' field is defined by interface %s{idef.Name}, but not implemented in object %s{objdef.Name}"
                        :: acc
                    | Some objf when objf = f -> acc
                    | Some _ ->
                        $"'%s{objdef.Name}.%s{f.Name}' field signature does not match it's definition in interface %s{idef.Name}"
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
                    ValidationError [ objdef.Name + " must have at least one field defined" ]
            let implementsResult =
                objdef.Implements
                |> ValidationResult.collect (validateImplements objdef)
            nonEmptyResult @@ implementsResult
        | InputObject indef ->
            let nonEmptyResult =
                if indef.Fields.Length > 0 then
                    Success
                else
                    ValidationError [ indef.Name + " must have at least one field defined" ]
            nonEmptyResult
        | Union uniondef ->
            let nonEmptyResult =
                if uniondef.Options.Length > 0 then
                    Success
                else
                    ValidationError [
                        uniondef.Name
                        + " must have at least one type definition option"
                    ]
            nonEmptyResult
        | Enum enumdef ->
            let nonEmptyResult =
                if enumdef.Options.Length > 0 then
                    Success
                else
                    ValidationError [ enumdef.Name + " must have at least one enum value defined" ]
            nonEmptyResult
        | Interface idef ->
            let nonEmptyResult =
                if idef.Fields.Length > 0 then
                    Success
                else
                    ValidationError [ idef.Name + " must have at least one field defined" ]
            nonEmptyResult
        | _ -> failwithf "Unexpected value of typedef: %O" typedef

    let validateTypeMap (namedTypes : TypeMap) : ValidationResult<string> =
        namedTypes.ToSeq ()
        |> Seq.fold (fun acc (_, namedDef) -> acc @@ validateType namedDef) Success

module Ast =

    type MetaTypeFieldInfo = { Name : string; ArgumentNames : string[] }

    let private metaTypeFields =
        [|
            { Name = "__type"; ArgumentNames = [| "name" |] }
            { Name = "__schema"; ArgumentNames = [||] }
            { Name = "__typename"; ArgumentNames = [||] }
        |]
        |> Array.map (fun x -> x.Name, x)
        |> Map.ofArray

    let rec private tryGetSchemaTypeByRef (schemaTypes : Map<string, IntrospectionType>) (tref : IntrospectionTypeRef) =
        match tref.Kind with
        | TypeKind.NON_NULL
        | TypeKind.LIST when tref.OfType.IsSome -> tryGetSchemaTypeByRef schemaTypes tref.OfType.Value
        | _ -> tref.Name |> Option.bind schemaTypes.TryFind

    type SchemaInfo = {
        SchemaTypes : Map<string, IntrospectionType>
        QueryType : IntrospectionType option
        SubscriptionType : IntrospectionType option
        MutationType : IntrospectionType option
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
                    |> Option.bind (tryGetSchemaTypeByRef schemaTypes)
                SubscriptionType =
                    schema.SubscriptionType
                    |> Option.bind (tryGetSchemaTypeByRef schemaTypes)
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
        FragmentDefinitions : FragmentDefinition list
        ParentType : IntrospectionType
        FragmentType : FragmentTypeInfo voption
        Path : FieldPath
        SelectionSet : Selection list
    } with

        member x.FragmentOrParentType =
            x.FragmentType
            |> ValueOption.map _.TypeCondition
            |> ValueOption.defaultValue x.ParentType

    let private tryFindInArrayOption (finder : 'T -> bool) =
        ValueOption.ofOption
        >> ValueOption.bind (Array.tryFind finder >> ValueOption.ofOption)

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
                voption {
                    let! typeCondition = inlineFrag.TypeCondition
                    let! fragType = ctx.Schema.TryGetTypeByName typeCondition
                    let fragType = Inline fragType
                    return getFragSelectionSetInfo visitedFragments fragType inlineFrag.SelectionSet ctx
                }
                |> ValueOption.defaultValue List.empty
            | FragmentSpread fragSpread ->
                voption {
                    let! fragDef =
                        ctx.FragmentDefinitions
                        |> List.tryFind (fun def -> def.Name.IsSome && def.Name.Value = fragSpread.Name)
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

    /// Prepare a ValidationContext for the given Document and SchemaInfo to make validation operations easier.
    let internal getValidationContext (schemaInfo : SchemaInfo) (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        let fragmentInfos =
            fragmentDefinitions
            |> List.vchoose (fun def -> voption {
                let! typeCondition = def.TypeCondition
                let! fragType = schemaInfo.TryGetTypeByName typeCondition
                let fragCtx = {
                    Schema = schemaInfo
                    FragmentDefinitions = fragmentDefinitions
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
                    FragmentDefinitions = fragmentDefinitions
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

    let internal validateOperationNameUniqueness (ctx : ValidationContext) =
        let names = ctx.Document.Definitions |> Seq.vchoose _.Name
        names
        |> Seq.map (fun name -> name, names |> Seq.filter (fun x -> x = name) |> Seq.length)
        |> Seq.distinctBy fst
        |> ValidationResult.collect (fun (name, count) ->
            if count <= 1 then
                Success
            else
                AstError.AsResult $"Operation '%s{name}' has %i{count} definitions. Each operation name must be unique.")

    let internal validateLoneAnonymousOperation (ctx : ValidationContext) =
        let operations = ctx.OperationDefinitions |> List.map (fun x -> x.Definition)
        let unamed = operations |> List.filter (fun x -> x.Name.IsNone)
        if unamed.Length = 0 then
            Success
        elif unamed.Length = 1 && operations.Length = 1 then
            Success
        else
            AstError.AsResult
                "An anonymous operation must be the only operation in a document. This document has at least one anonymous operation and more than one operation."

    let internal validateSubscriptionSingleRootField (ctx : ValidationContext) =
        let fragmentDefinitions = getFragmentDefinitions ctx.Document
        let rec getFieldNames (selectionSet : Selection list) =
            ([], selectionSet)
            ||> List.fold (fun acc ->
                function
                | Field field -> field.AliasOrName :: acc
                | InlineFragment frag -> List.append (getFieldNames frag.SelectionSet) acc
                | FragmentSpread spread ->
                    fragmentDefinitions
                    |> List.tryFind (fun x -> x.Name.IsSome && x.Name.Value = spread.Name)
                    |> Option.unwrap acc (fun frag -> getFieldNames frag.SelectionSet))
        ctx.Document.Definitions
        |> ValidationResult.collect (function
            | OperationDefinition def when def.OperationType = Subscription ->
                let fieldNames = getFieldNames def.SelectionSet
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
                    |> Option.map (Array.exists (fun f -> f.Name = selection.Field.Name))
                    |> Option.defaultValue false
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
            |> Option.defaultValue [||]
            |> Seq.choose _.Name
            |> Seq.append (Seq.singleton parentType.Name)
            |> Set.ofSeq
        let fragmentPossibleTypes =
            fragmentType.PossibleTypes
            |> Option.defaultValue [||]
            |> Seq.choose _.Name
            |> Seq.append (Seq.singleton fragmentType.Name)
            |> Set.ofSeq
        let applicableTypes = Set.intersect parentPossibleTypes fragmentPossibleTypes
        applicableTypes.Count > 0

    let rec private sameResponseShape (fieldA : SelectionInfo, fieldB : SelectionInfo) =
        if fieldA.FieldType = fieldB.FieldType then
            let fieldsForName = Dictionary<string, SelectionInfo list> ()
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
                    |> ValidationResult.collect sameResponseShape)
        else
            AstError.AsResult (
                $"Field name or alias '%s{fieldA.AliasOrName}' appears two times, but they do not have the same return types in the scope of the parent type.",
                fieldA.Path
            )

    let rec private fieldsInSetCanMerge (set : SelectionInfo list) =
        let fieldsForName = set |> List.groupBy (fun x -> x.AliasOrName)
        fieldsForName
        |> ValidationResult.collect (fun (aliasOrName, selectionSet) ->
            if selectionSet.Length < 2 then
                Success
            else
                List.pairwise selectionSet
                |> ValidationResult.collect (fun (fieldA, fieldB) ->
                    let hasSameShape = sameResponseShape (fieldA, fieldB)
                    if
                        fieldA.FragmentOrParentType = fieldB.FragmentOrParentType
                        || fieldA.FragmentOrParentType.Kind <> TypeKind.OBJECT
                        || fieldB.FragmentOrParentType.Kind <> TypeKind.OBJECT
                    then
                        if fieldA.Field.Name <> fieldB.Field.Name then
                            hasSameShape
                            @@ AstError.AsResult (
                                $"Field name or alias '%s{aliasOrName}' is referring to fields '%s{fieldA.Field.Name}' and '%s{fieldB.Field.Name}', but they are different fields in the scope of the parent type.",
                                fieldA.Path
                            )
                        else if fieldA.Field.Arguments <> fieldB.Field.Arguments then
                            hasSameShape
                            @@ AstError.AsResult (
                                $"Field name or alias '%s{aliasOrName}' refers to field '%s{fieldA.Field.Name}' two times, but each reference has different argument sets.",
                                fieldA.Path
                            )
                        else
                            let mergedSet = fieldA.SelectionSet @ fieldB.SelectionSet
                            hasSameShape @@ (fieldsInSetCanMerge mergedSet)
                    else
                        hasSameShape))

    let internal validateFieldSelectionMerging (ctx : ValidationContext) =
        ctx.Definitions
        |> ValidationResult.collect (fun def -> fieldsInSetCanMerge def.SelectionSet)

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
                    |> Option.map (fun x -> x.ArgumentNames)
                    |> Option.defaultValue (selection.InputValues |> Array.map (fun x -> x.Name))
                match schemaArgumentNames |> Array.tryFind (fun x -> x = arg.Name) with
                | Some _ -> Success
                | None ->
                    AstError.AsResult (
                        $"Field '%s{selection.Field.Name}' of type '%s{selection.FragmentOrParentType.Name}' does not have an input named '%s{arg.Name}' in its definition.",
                        selection.Path
                    ))
        let directivesValid =
            selection.Field.Directives
            |> ValidationResult.collect (fun directive ->
                match
                    schemaInfo.Directives
                    |> Array.tryFind (fun d -> d.Name = directive.Name)
                with
                | Some directiveType ->
                    directive.Arguments
                    |> ValidationResult.collect (fun arg ->
                        match
                            directiveType.Args
                            |> Array.tryFind (fun argt -> argt.Name = arg.Name)
                        with
                        | Some _ -> Success
                        | _ ->
                            AstError.AsResult (
                                $"Directive '%s{directiveType.Name}' of field '%s{selection.Field.Name}' of type '%s{selection.FragmentOrParentType.Name}' does not have an argument named '%s{arg.Name}' in its definition.",
                                selection.Path
                            ))
                | None -> Success)
        argumentsValid @@ directivesValid

    let internal validateArgumentNames (ctx : ValidationContext) = onAllSelections ctx (checkFieldArgumentNames ctx.Schema)

    let rec private validateArgumentUniquenessInSelection (selection : SelectionInfo) =
        let validateArgs (fieldOrDirective : string) (path : FieldPath) (args : Argument list) =
            args
            |> List.countBy (fun x -> x.Name)
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
                        |> List.tryFind (fun arg -> arg.Name = argDef.Name)
                    with
                    | Some arg when arg.Value <> NullValue -> Success
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
                    |> Array.tryFind (fun d -> d.Name = directive.Name)
                with
                | Some directiveType ->
                    directiveType.Args
                    |> ValidationResult.collect (fun argDef ->
                        match argDef.Type.Kind with
                        | TypeKind.NON_NULL when argDef.DefaultValue.IsNone ->
                            match
                                directive.Arguments
                                |> List.tryFind (fun arg -> arg.Name = argDef.Name)
                            with
                            | Some arg when arg.Value <> NullValue -> Success
                            | _ ->
                                AstError.AsResult (
                                    $"Argument '%s{argDef.Name}' of directive '%s{directiveType.Name}' of field '%s{selection.Field.Name}' of type '%s{selection.FragmentOrParentType.Name}' is required and does not have a default value.",
                                    selection.Path
                                )
                        | _ -> Success)
                | None -> Success)
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
            let fragType = voption {
                let! typeCondition = frag.TypeCondition
                return! schemaInfo.TryGetTypeByName typeCondition
            }
            match fragType with
            | ValueSome _ -> Success
            | ValueNone when frag.Name.IsSome ->
                AstError.AsResult
                    $"Fragment '%s{frag.Name.Value}' has type condition '%s{frag.TypeCondition.Value}', but that type does not exist in the schema."
            | ValueNone ->
                AstError.AsResult (
                    $"Inline fragment has type condition '%s{frag.TypeCondition.Value}', but that type does not exist in the schema.",
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

    let rec private fragmentSpreadTargetDefinedInSelection (fragmentDefinitionNames : string list) (path : FieldPath) =
        function
        | Field field ->
            let path = box field.AliasOrName :: path
            field.SelectionSet
            |> ValidationResult.collect (fragmentSpreadTargetDefinedInSelection fragmentDefinitionNames path)
        | InlineFragment frag ->
            frag.SelectionSet
            |> ValidationResult.collect (fragmentSpreadTargetDefinedInSelection fragmentDefinitionNames path)
        | FragmentSpread spread ->
            if List.contains spread.Name fragmentDefinitionNames then
                Success
            else
                AstError.AsResult ($"Fragment spread '%s{spread.Name}' refers to a non-existent fragment definition in the document.", path)

    let internal validateFragmentSpreadTargetDefined (ctx : ValidationContext) =
        let fragmentDefinitionNames = ctx.FragmentDefinitions |> List.vchoose _.Name
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

    let rec private checkFragmentMustNotHaveCycles
        (fragmentDefinitions : FragmentDefinition list)
        (visited : string list)
        (fragName : string)
        (fragSelectionSet : Selection list)
        =
        let visitCount =
            visited
            |> List.filter (fun x -> x = fragName)
            |> List.length
        if visitCount > 1 then
            AstError.AsResult $"Fragment '%s{fragName}' is making a cyclic reference."
        else
            fragSelectionSet
            |> ValidationResult.collect (checkFragmentsMustNotHaveCyclesInSelection fragmentDefinitions (fragName :: visited))

    and private checkFragmentsMustNotHaveCyclesInSelection (fragmentDefinitions : FragmentDefinition list) (visited : string list) =
        function
        | Field field ->
            field.SelectionSet
            |> ValidationResult.collect (checkFragmentsMustNotHaveCyclesInSelection fragmentDefinitions visited)
        | InlineFragment inlineFrag ->
            inlineFrag.SelectionSet
            |> ValidationResult.collect (checkFragmentsMustNotHaveCyclesInSelection fragmentDefinitions visited)
        | FragmentSpread spread ->
            match
                fragmentDefinitions
                |> List.tryFind (fun f -> f.Name.IsSome && f.Name.Value = spread.Name)
            with
            | Some frag -> checkFragmentMustNotHaveCycles fragmentDefinitions visited spread.Name frag.SelectionSet
            | None -> Success

    let internal validateFragmentsMustNotFormCycles (ctx : ValidationContext) =
        let fragmentDefinitions =
            ctx.FragmentDefinitions
            |> List.map (fun frag -> frag.Definition)
        let fragNamesAndSelections =
            fragmentDefinitions
            |> List.vchoose (fun frag -> frag.Name |> ValueOption.map (fun n -> n, frag.SelectionSet))
        fragNamesAndSelections
        |> ValidationResult.collect (fun (name, selectionSet) -> checkFragmentMustNotHaveCycles fragmentDefinitions [] name selectionSet)

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

    let private checkInputValue (schemaInfo : SchemaInfo) (variables : VariableDefinition list option) (selection : SelectionInfo) =
        let rec checkIsCoercible (tref : IntrospectionTypeRef) (argName : string) (value : InputValue) =
            let canNotCoerce =
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
            | _ when tref.Kind = TypeKind.NON_NULL -> checkIsCoercible tref.OfType.Value argName value
            | IntValue _ ->
                match tref.Name, tref.Kind with
                | Some ("ID" | "Int" | "Float"), TypeKind.SCALAR -> Success
                | _ -> canNotCoerce
            | FloatValue _ ->
                match tref.Name, tref.Kind with
                | Some "Float", TypeKind.SCALAR -> Success
                | _ -> canNotCoerce
            | BooleanValue _ ->
                match tref.Name, tref.Kind with
                | Some "Boolean", TypeKind.SCALAR -> Success
                | _ -> canNotCoerce
            | StringValue _ ->
                let invalidScalars = [| "Int"; "Float"; "Boolean" |]
                match tref.Name, tref.Kind with
                | (Some x, TypeKind.SCALAR) when not (Array.contains x invalidScalars) -> Success
                | _ -> canNotCoerce
            | EnumValue _ ->
                match tref.Kind with
                | TypeKind.ENUM -> Success
                | _ -> canNotCoerce
            | ListValue values ->
                match tref.Kind with
                | TypeKind.LIST when tref.OfType.IsSome ->
                    values
                    |> ValidationResult.collect (checkIsCoercible tref.OfType.Value argName)
                | _ -> canNotCoerce
            | ObjectValue props ->
                match tref.Kind with
                | TypeKind.OBJECT
                | TypeKind.INTERFACE
                | TypeKind.UNION
                | TypeKind.INPUT_OBJECT when tref.Name.IsSome ->
                    match schemaInfo.TryGetTypeByRef (tref) with
                    | Some itype ->
                        let fieldMap =
                            itype.InputFields
                            |> Option.defaultValue [||]
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
                                | Some fieldTypeRef -> checkIsCoercible fieldTypeRef kvp.Key kvp.Value
                                | None ->
                                    AstError.AsResult (
                                        $"Can not coerce argument '%s{argName}'. The field '%s{kvp.Key}' is not a valid field in the argument definition.",
                                        selection.Path
                                    ))
                        canCoerceFields @@ canCoerceProps
                    | None -> canNotCoerce
                | _ -> canNotCoerce
            | VariableName varName ->
                let variableDefinition =
                    variables
                    |> Option.defaultValue []
                    |> List.tryPick (fun v ->
                        if v.VariableName = varName then
                            Some (v, schemaInfo.TryGetInputType (v.Type))
                        else
                            None)
                match variableDefinition with
                | Some (vdef, Some vtype) when vdef.DefaultValue.IsSome -> checkIsCoercible vtype argName vdef.DefaultValue.Value
                | Some (vdef, None) when vdef.DefaultValue.IsSome -> canNotCoerce
                | _ -> Success
        selection.Field.Arguments
        |> ValidationResult.collect (fun arg ->
            let argumentTypeRef =
                selection.InputValues
                |> Array.tryPick (fun x -> if x.Name = arg.Name then Some x.Type else None)
            match argumentTypeRef with
            | Some argumentTypeRef -> checkIsCoercible argumentTypeRef arg.Name arg.Value
            | None -> Success)

    let internal validateInputValues (ctx : ValidationContext) =
        ctx.Definitions
        |> ValidationResult.collect (fun def ->
            let (vars, selectionSet) =
                match def with
                | OperationDefinitionInfo odef -> (Some odef.Definition.VariableDefinitions, odef.SelectionSet)
                | FragmentDefinitionInfo fdef -> (None, fdef.SelectionSet)
            selectionSet
            |> ValidationResult.collect (checkInputValue ctx.Schema vars))

    let rec private getDistinctDirectiveNamesInSelection (path : FieldPath) (selection : Selection) : (FieldPath * Set<string>) list =
        match selection with
        | Field field ->
            let path = box field.AliasOrName :: path
            let fieldDirectives = [ path, field.Directives |> List.map (fun x -> x.Name) |> Set.ofList ]
            let selectionSetDirectives =
                field.SelectionSet
                |> List.collect (getDistinctDirectiveNamesInSelection path)
            fieldDirectives |> List.append selectionSetDirectives
        | InlineFragment frag -> getDistinctDirectiveNamesInDefinition path (FragmentDefinition frag)
        | FragmentSpread spread -> [
            path,
            spread.Directives
            |> List.map (fun x -> x.Name)
            |> Set.ofList
          ]

    and private getDistinctDirectiveNamesInDefinition (path : FieldPath) (frag : Definition) : (FieldPath * Set<string>) list =
        let fragDirectives = [ path, frag.Directives |> List.map (fun x -> x.Name) |> Set.ofList ]
        let selectionSetDirectives =
            frag.SelectionSet
            |> List.collect (getDistinctDirectiveNamesInSelection path)
        fragDirectives |> List.append selectionSetDirectives

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
        let fragmentDefinitions = ctx.FragmentDefinitions |> List.map (fun x -> x.Definition)
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
            let fieldDirectives = [ path, field.Directives |> List.map (fun x -> x.Name) ]
            let selectionSetDirectives =
                field.SelectionSet
                |> List.collect (getDirectiveNamesInSelection path)
            fieldDirectives |> List.append selectionSetDirectives
        | InlineFragment frag -> getDirectiveNamesInDefinition path (FragmentDefinition frag)
        | FragmentSpread spread -> [ path, spread.Directives |> List.map (fun x -> x.Name) ]

    and private getDirectiveNamesInDefinition (path : FieldPath) (frag : Definition) : (FieldPath * string list) list =
        let fragDirectives = [ path, frag.Directives |> List.map (fun x -> x.Name) ]
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
            let defDirectives = path, def.Directives |> List.map (fun x -> x.Name)
            let selectionSetDirectives =
                def.Definition.SelectionSet
                |> List.collect (getDirectiveNamesInSelection path)
            defDirectives :: selectionSetDirectives)
        |> ValidationResult.collect (fun (path, directives) ->
            directives
            |> List.countBy id
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
                    |> List.map (fun x -> x.VariableName)
                    |> Set.ofList
                def.SelectionSet
                |> ValidationResult.collect (checkVariablesDefinedInSelection fragmentDefinitions varNames path)
            | _ -> Success)

    let private argumentsContains (name : string) (args : Argument list) =
        let rec go xs =
            xs
            |> List.exists (function
                | VariableName varName -> varName = name
                | ObjectValue obj -> go (Map.toList obj |> List.map snd)
                | ListValue xs -> go xs
                | _ -> false)
        go (args |> List.map (fun x -> x.Value))

    let rec private variableIsUsedInFragmentSpread
        (name : string)
        (fragmentDefinitions : FragmentDefinition list)
        (visitedFragments : string list)
        (spread : FragmentSpread)
        =
        if List.contains spread.Name visitedFragments then
            false
        else
            let usedInSpread =
                match
                    fragmentDefinitions
                    |> List.tryFind (fun x -> x.Name.IsSome && x.Name.Value = spread.Name)
                with
                | Some frag ->
                    let usedInSelection =
                        frag.SelectionSet
                        |> List.exists (variableIsUsedInSelection name fragmentDefinitions (spread.Name :: visitedFragments))
                    usedInSelection
                    || (frag.Directives
                        |> List.exists (fun directive -> argumentsContains name directive.Arguments))
                | None -> false
            usedInSpread
            || (spread.Directives
                |> List.exists (fun directive -> argumentsContains name directive.Arguments))

    and private variableIsUsedInSelection (name : string) (fragmentDefinitions : FragmentDefinition list) (visitedFragments : string list) =
        function
        | Field field ->
            if argumentsContains name field.Arguments then
                true
            else
                let usedInSelection =
                    field.SelectionSet
                    |> List.exists (variableIsUsedInSelection name fragmentDefinitions visitedFragments)
                usedInSelection
                || (field.Directives
                    |> List.exists (fun directive -> argumentsContains name directive.Arguments))
        | InlineFragment frag ->
            let usedInSelection =
                frag.SelectionSet
                |> List.exists (variableIsUsedInSelection name fragmentDefinitions visitedFragments)
            usedInSelection
            || (frag.Directives
                |> List.exists (fun directive -> argumentsContains name directive.Arguments))
        | FragmentSpread spread -> variableIsUsedInFragmentSpread name fragmentDefinitions visitedFragments spread

    let internal validateAllVariablesUsed (ctx : ValidationContext) =
        let fragmentDefinitions = getFragmentDefinitions ctx.Document
        ctx.Document.Definitions
        |> ValidationResult.collect (function
            | OperationDefinition def ->
                def.VariableDefinitions
                |> ValidationResult.collect (fun varDef ->
                    let isUsed =
                        def.SelectionSet
                        |> List.exists (variableIsUsedInSelection varDef.VariableName fragmentDefinitions [])
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
                    match inputs |> Array.tryFind (fun x -> x.Name = arg.Name) with
                    | Some input ->
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
                    | None -> Success
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

    let private allValidations = [
        validateFragmentsMustNotFormCycles
        validateOperationNameUniqueness
        validateLoneAnonymousOperation
        validateSubscriptionSingleRootField
        validateSelectionFieldTypes
        validateFieldSelectionMerging
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
        validateInputValues
        validateDirectivesDefined
        validateDirectivesAreInValidLocations
        validateUniqueDirectivesPerLocation
        validateVariableUniqueness
        validateVariablesAsInputTypes
        validateVariablesUsesDefined
        validateAllVariablesUsed
        validateVariableUsagesAllowed
    ]

    /// Run all available Ast validations against the given Document and IntrospectionSchema
    let validateDocument (schema : IntrospectionSchema) (ast : Document) =
        let schemaInfo = SchemaInfo.FromIntrospectionSchema (schema)
        let context = getValidationContext schemaInfo ast
        allValidations
        |> ValidationResult.collect (fun validate -> validate context)
