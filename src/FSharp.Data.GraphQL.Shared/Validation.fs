/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Validation

open System.Collections.Generic
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Extensions
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types.Introspection

module Types =
    let validateImplements (objdef: ObjectDef) (idef: InterfaceDef) =
        let objectFields =
            objdef.Fields
        let errors =
            idef.Fields
            |> Array.fold (fun acc f ->
                match Map.tryFind f.Name objectFields with
                | None -> (sprintf "'%s' field is defined by interface %s, but not implemented in object %s" f.Name idef.Name objdef.Name)::acc
                | Some objf when objf = f -> acc
                | Some _ -> (sprintf "'%s.%s' field signature does not match it's definition in interface %s" objdef.Name f.Name idef.Name)::acc) []
        match errors with
        | [] -> Success
        | err -> ValidationError err

    let validateType typedef =
        match typedef with
        | Scalar _ -> Success
        | Object objdef ->
            let nonEmptyResult = if objdef.Fields.Count > 0 then Success else ValidationError [ objdef.Name + " must have at least one field defined" ]
            let implementsResult = objdef.Implements |> collectResults (validateImplements objdef)
            nonEmptyResult @@ implementsResult
        | InputObject indef ->
            let nonEmptyResult = if indef.Fields.Length > 0 then Success else ValidationError [ indef.Name + " must have at least one field defined" ]
            nonEmptyResult
        | Union uniondef ->
            let nonEmptyResult = if uniondef.Options.Length > 0 then Success else ValidationError [ uniondef.Name + " must have at least one type definition option" ]
            nonEmptyResult
        | Enum enumdef ->
            let nonEmptyResult = if enumdef.Options.Length > 0 then Success else ValidationError [ enumdef.Name + " must have at least one enum value defined" ]
            nonEmptyResult
        | Interface idef ->
            let nonEmptyResult = if idef.Fields.Length > 0 then Success else ValidationError [ idef.Name + " must have at least one field defined" ]
            nonEmptyResult
        | _ -> failwithf "Unexpected value of typedef: %O" typedef

    let validateTypeMap (namedTypes: TypeMap) : ValidationResult<string> =
        namedTypes.ToSeq() |> Seq.fold (fun acc (_, namedDef) -> acc @@ validateType namedDef) Success

module Ast =
    type MetaTypeFieldInfo =
        { Name : string
          ArgumentNames : string [] }

    let private metaTypeFields = 
        [| { Name = "__type"; ArgumentNames = [|"name"|] }
           { Name = "__schema"; ArgumentNames = [||] }
           { Name = "__typename"; ArgumentNames = [||] } |]
        |> Array.map (fun x -> x.Name, x)
        |> Map.ofArray

    let rec private tryGetSchemaTypeByRef (schemaTypes : Map<string, IntrospectionType>) (tref : IntrospectionTypeRef) =
        match tref.Kind with
        | TypeKind.NON_NULL | TypeKind.LIST when tref.OfType.IsSome -> tryGetSchemaTypeByRef schemaTypes tref.OfType.Value
        | _ -> tref.Name |> Option.bind schemaTypes.TryFind

    type SchemaInfo =
        { SchemaTypes : Map<string, IntrospectionType>
          QueryType : IntrospectionType option
          SubscriptionType : IntrospectionType option
          MutationType : IntrospectionType option
          Directives : IntrospectionDirective [] }
        member x.TryGetTypeByRef(tref : IntrospectionTypeRef) =
            tryGetSchemaTypeByRef x.SchemaTypes tref
        static member FromIntrospectionSchema(schema : IntrospectionSchema) =
            let schemaTypes = schema.Types |> Seq.map (fun x -> x.Name, x) |> Map.ofSeq
            { SchemaTypes = schema.Types |> Seq.map (fun x -> x.Name, x) |> Map.ofSeq
              QueryType = tryGetSchemaTypeByRef schemaTypes schema.QueryType
              MutationType = schema.MutationType |> Option.bind (tryGetSchemaTypeByRef schemaTypes)
              SubscriptionType = schema.SubscriptionType |> Option.bind (tryGetSchemaTypeByRef schemaTypes)
              Directives = schema.Directives }
        member x.TryGetOperationType(ot : OperationType) =
            match ot with
            | Query -> x.QueryType
            | Mutation -> x.MutationType
            | Subscription -> x.SubscriptionType
        member x.TryGetTypeByName(name : string) =
            x.SchemaTypes.TryFind(name)
        member x.TryGetInputType(input : InputType) =
            match input with
            | NamedType name ->
                x.TryGetTypeByName(name)
                |> Option.bind (fun x -> match x.Kind with | TypeKind.INPUT_OBJECT | TypeKind.SCALAR | TypeKind.ENUM -> Some x | _ -> None)
                |> Option.map IntrospectionTypeRef.Named
            | ListType inner -> x.TryGetInputType(inner) |> Option.map IntrospectionTypeRef.List
            | NonNullType inner -> x.TryGetInputType(inner) |> Option.map IntrospectionTypeRef.NonNull

    /// Contains information about the selection in a field or a fragment, with related GraphQL schema type information.
    and SelectionInfo =
        { /// Contains the information about the field inside the selection set of the parent type.
          Field : Field
          /// Contains the reference to tye field type in the schema.
          FieldType : IntrospectionTypeRef option
          /// Contains the reference to the parent type of the current field in the schema.
          ParentType : IntrospectionType
          /// If the field is inside a fragment selection, gets the reference to the fragment type of the current field in the schema.
          FragmentType : IntrospectionType option
          /// In case this field is part of a selection of a fragment spread, gets the name of the fragment spread.
          FragmentSpreadName : string option
          /// Contains the selection info of this field, if it is an Object, Interface or Union type.
          SelectionSet : SelectionInfoKind list
          /// In case the schema definition fo this field has input values, gets information about the input values of the field in the schema.
          InputValues : IntrospectionInputVal [] option
          /// Contains the path of the field in the document.
          Path : Path }
        /// If the field has an alias, return its alias. Otherwise, returns its name.
        member x.AliasOrName = x.Field.AliasOrName
        /// If the field is inside a selection of a fragment definition, returns the fragment type containing the field.
        /// Otherwise, return the parent type in the schema definition.
        member x.FragmentOrParentType = x.FragmentType |> Option.defaultValue x.ParentType
        /// Gets the selection set that does not make cyclic references to a specific Fragment.
        member x.NonCyclicSelectionSet = x.SelectionSet |> List.choose (function SelectionField x -> Some x | _ -> None)

    and SelectionInfoKind =
        | SelectionField of SelectionInfo
        | CyclicReferenceToFragment of fragmentName : string

    /// Contains information about an operation definition in the document, with related GraphQL schema type information.
    type OperationDefinitionInfo =
        { /// Returns the definition from the parsed document.
          Definition : OperationDefinition
          /// Returns the selection information about the operation, with related GraphQL schema type information.
          SelectionSet : SelectionInfoKind list }
        /// Returns the name of the operation definition, if it does have a name.
        member x.Name = x.Definition.Name
        /// Gets the selection set that does not make cyclic references to a specific Fragment.
        member x.NonCyclicSelectionSet = x.SelectionSet |> List.choose (function SelectionField x -> Some x | _ -> None)

    /// Contains information about a fragment definition in the document, with related GraphQL schema type information.
    type FragmentDefinitionInfo =
        { /// Returns the definition from the parsed document.
          Definition : FragmentDefinition
          /// Returns the selection information about the fragment, with related GraphQL schema type information.
          SelectionSet : SelectionInfoKind list }
        /// Returns the name of the fragment definition, if it does have a name.
        member x.Name = x.Definition.Name
        /// Gets the selection set that does not make cyclic references to a specific Fragment.
        member x.NonCyclicSelectionSet = x.SelectionSet |> List.choose (function SelectionField x -> Some x | _ -> None)

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
        /// Gets the selection set that does not make cyclic references to a specific Fragment.
        member x.NonCyclicSelectionSet = x.SelectionSet |> List.choose (function SelectionField x -> Some x | _ -> None)
    
    /// The validation  context used to run validations against a parsed document.
    /// It should have the schema type information, the original document and the definition information about the original document.
    type ValidationContext =
        { /// Gets information about all definitions in the original document, including related GraphQL schema type information for them.
          Definitions : DefinitionInfo list
          /// Gets information about the schema which the document is validated against.
          Schema : SchemaInfo
          /// Gets the document that is being validated.
          Document : Document }
        /// Gets the information about all the operations in the original document, including related GraphQL schema type information for them.
        member x.OperationDefinitions = x.Definitions |> List.choose (function | OperationDefinitionInfo x -> Some x | _ -> None)
        /// Gets the information about all the fragments in the original document, including related GraphQL schema type information for them.
        member x.FragmentDefinitions = x.Definitions |> List.choose (function | FragmentDefinitionInfo x -> Some x | _ -> None)

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
            | Inline _ -> None
            | Spread (name, _, _) -> Some name

    type SelectionInfoContext =
        { Schema : SchemaInfo
          FragmentDefinitions : FragmentDefinition list
          ParentType : IntrospectionType
          FragmentType : FragmentTypeInfo option
          Path : Path
          SelectionSet : Selection list }
        member x.FragmentOrParentType = 
            x.FragmentType
            |> Option.map (fun x -> x.TypeCondition) 
            |> Option.defaultValue x.ParentType

    let private tryFindInArrayOption (finder : 'T -> bool) = Option.bind (Array.tryFind finder)

    let rec private getFragSelectionSetInfo (visitedFragments : string list ref) (fragmentTypeInfo : FragmentTypeInfo) (fragmentSelectionSet : Selection list) (parentCtx : SelectionInfoContext) =
        match fragmentTypeInfo with
        | Inline fragType ->
            let fragCtx =
                { parentCtx with
                    ParentType = parentCtx.FragmentOrParentType
                    FragmentType = Some (Inline fragType)
                    SelectionSet = fragmentSelectionSet }
            getSelectionSetInfo visitedFragments fragCtx
        | Spread (fragName, _, _) when List.contains fragName !visitedFragments ->
            CyclicReferenceToFragment fragName |> List.singleton
        | Spread (fragName, directives, fragType) ->
            visitedFragments := fragName :: !visitedFragments
            let fragCtx =
                { parentCtx with
                    ParentType = parentCtx.FragmentOrParentType
                    FragmentType = Some (Spread (fragName, directives, fragType))
                    SelectionSet = fragmentSelectionSet }
            getSelectionSetInfo visitedFragments fragCtx
            
    and private getSelectionSetInfo (visitedFragments : string list ref) (ctx : SelectionInfoContext) : SelectionInfoKind list =
        // When building the selection info, we should not raise any error when a type referred by the document
        // is not found in the schema. Not found types are validated in another validation, without the need of the
        // selection info to do it. Info types are helpers to validate against their schema types when the match is found.
        // Because of that, whenever a field or fragment type does not have a matching type in the schema, we skip the selection production.
        ctx.SelectionSet |> List.collect (function
            | Field field ->
                let introspectionField = ctx.FragmentOrParentType.Fields |> tryFindInArrayOption (fun f -> f.Name = field.Name)
                let inputValues = introspectionField |> Option.map (fun f -> f.Args)
                let fieldTypeRef = introspectionField |> Option.map (fun f -> f.Type)
                let fieldPath = field.AliasOrName :: ctx.Path
                let fieldSelectionSet =
                    fieldTypeRef 
                    |> Option.bind ctx.Schema.TryGetTypeByRef
                    |> Option.map (fun fieldType ->
                        let fieldCtx =
                            { ctx with 
                                ParentType = fieldType
                                FragmentType = None
                                Path = fieldPath
                                SelectionSet = field.SelectionSet }
                        getSelectionSetInfo (ref !visitedFragments) fieldCtx)
                    |> Option.defaultValue []
                { Field = field
                  SelectionSet = fieldSelectionSet
                  FieldType = fieldTypeRef
                  ParentType = ctx.ParentType
                  FragmentType = ctx.FragmentType |> Option.map (fun x -> x.TypeCondition)
                  FragmentSpreadName = ctx.FragmentType |> Option.bind (fun x -> x.Name)
                  InputValues = inputValues
                  Path = fieldPath }
                |> SelectionField
                |> List.singleton
            | InlineFragment inlineFrag -> 
                inlineFrag.TypeCondition
                |> Option.bind ctx.Schema.TryGetTypeByName
                |> Option.map (fun fragType -> 
                    let fragType = Inline fragType
                    getFragSelectionSetInfo (ref !visitedFragments) fragType inlineFrag.SelectionSet ctx)
                |> Option.defaultValue List.empty
            | FragmentSpread fragSpread ->
                ctx.FragmentDefinitions
                |> List.tryFind (fun def -> def.Name.IsSome && def.Name.Value = fragSpread.Name)
                |> Option.bind (fun fragDef ->
                    fragDef.TypeCondition
                    |> Option.bind ctx.Schema.TryGetTypeByName
                    |> Option.map (fun fragType ->
                        let fragType = (Spread (fragSpread.Name, fragSpread.Directives, fragType))
                        getFragSelectionSetInfo (ref !visitedFragments) fragType fragDef.SelectionSet ctx))
                |> Option.defaultValue List.empty)

    let private getOperationDefinitions (ast : Document) = 
        ast.Definitions
        |> List.choose (function | OperationDefinition x -> Some x | _ -> None)

    let private getFragmentDefinitions (ast : Document) = 
        ast.Definitions 
        |> List.choose (function | FragmentDefinition x when x.Name.IsSome -> Some x | _ -> None)

    let internal getValidationContext (schemaInfo : SchemaInfo) (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        let fragmentInfos =
            fragmentDefinitions
            |> List.choose (fun def ->
                def.TypeCondition
                |> Option.bind (fun typeCondition ->
                    schemaInfo.TryGetTypeByName(typeCondition)
                    |> Option.map (fun fragType ->
                        let fragCtx = 
                            { Schema = schemaInfo
                              FragmentDefinitions = fragmentDefinitions
                              ParentType = fragType
                              FragmentType = Some (Spread (def.Name.Value, def.Directives, fragType))
                              Path = [def.Name.Value]
                              SelectionSet = def.SelectionSet }
                        FragmentDefinitionInfo { Definition = def
                                                 SelectionSet = getSelectionSetInfo (ref []) fragCtx })))
        let operationInfos =
            getOperationDefinitions ast
            |> List.choose (fun def ->
                schemaInfo.TryGetOperationType(def.OperationType)
                |> Option.map (fun parentType ->
                    let path = match def.Name with | Some name -> [name] | None -> []
                    let opCtx = 
                        { Schema = schemaInfo
                          FragmentDefinitions = fragmentDefinitions
                          ParentType = parentType
                          FragmentType = None
                          Path = path
                          SelectionSet = def.SelectionSet }
                    OperationDefinitionInfo { Definition = def
                                              SelectionSet = getSelectionSetInfo (ref []) opCtx }))
        { Definitions = fragmentInfos @ operationInfos
          Schema = schemaInfo
          Document = ast }

    let internal validateOperationNameUniqueness (ctx : ValidationContext) =
        let names = ctx.Document.Definitions |> List.choose (fun x -> x.Name)
        names
        |> List.map (fun name -> name, names |> List.filter (fun x -> x = name) |> List.length)
        |> List.distinctBy fst
        |> collectResults (fun (name, count) ->
            if count <= 1
            then Success
            else AstError.AsResult(sprintf "Operation '%s' has %i definitions. Each operation name must be unique." name count))

    let internal validateLoneAnonymousOperation (ctx : ValidationContext) =
        let operations = ctx.OperationDefinitions |> List.map (fun x -> x.Definition)
        let unamed = operations |> List.filter (fun x -> x.Name.IsNone)
        if unamed.Length = 0 then Success
        elif unamed.Length = 1 && operations.Length = 1
        then Success
        else AstError.AsResult("An anonymous operation must be the only operation in a document. This document has at least one anonymous operation and more than one operation.")

    let internal validateSubscriptionSingleRootField (ctx : ValidationContext) =
        let fragmentDefinitions = getFragmentDefinitions ctx.Document
        let rec getFieldNames (selectionSet : Selection list) =
            ([], selectionSet)
            ||> List.fold (fun acc -> function
                | Field field -> field.AliasOrName :: acc
                | InlineFragment frag -> List.append (getFieldNames frag.SelectionSet) acc
                | FragmentSpread spread ->
                    fragmentDefinitions
                    |> List.tryFind (fun x -> x.Name.IsSome && x.Name.Value = spread.Name)
                    |> Option.unwrap acc (fun frag -> getFieldNames frag.SelectionSet))
        ctx.Document.Definitions
        |> collectResults (function
            | OperationDefinition def when def.OperationType = Subscription ->
                let fieldNames = getFieldNames def.SelectionSet
                if fieldNames.Length <= 1
                then Success
                else
                    let fieldNamesAsString = System.String.Join(", ", fieldNames)
                    match def.Name with
                    | Some operationName -> AstError.AsResult(sprintf "Subscription operations should have only one root field. Operation '%s' has %i fields (%s)." operationName fieldNames.Length fieldNamesAsString)
                    | None -> AstError.AsResult(sprintf "Subscription operations should have only one root field. Operation has %i fields (%s)." fieldNames.Length fieldNamesAsString)
            | _ -> Success)

    let internal validateSelectionFieldTypes (ctx : ValidationContext) =
        let rec checkFieldsOfType (visitedFragments : string list) (selectionSet : SelectionInfo list) =
            selectionSet
            |> collectResults (fun selection ->
                match selection.FragmentSpreadName with
                | Some spreadName when List.contains spreadName visitedFragments -> Success
                | _ ->
                    let selectionIsValid =
                        if metaTypeFields.ContainsKey(selection.Field.Name)
                        then Success
                        else
                            let exists = selection.FragmentOrParentType.Fields |> Option.map (Array.exists(fun f -> f.Name = selection.Field.Name)) |> Option.defaultValue false
                            if not exists
                            then AstError.AsResult(sprintf "Field '%s' is not defined in schema type '%s'." selection.Field.Name selection.FragmentOrParentType.Name, selection.Path)
                            else Success
                    let visitedFragments =
                        match selection.FragmentSpreadName with
                        | Some spreadName -> spreadName :: visitedFragments
                        | None -> visitedFragments
                    let innerSelectionIsValid = checkFieldsOfType visitedFragments selection.NonCyclicSelectionSet
                    selectionIsValid @@ innerSelectionIsValid)
        ctx.Definitions
        |> collectResults (function def -> checkFieldsOfType [] def.NonCyclicSelectionSet)

    let private typesAreApplicable (parentType : IntrospectionType, fragmentType : IntrospectionType) =
        let parentPossibleTypes = parentType.PossibleTypes |> Option.defaultValue [||] |> Array.choose (fun x -> x.Name) |> Array.append [|parentType.Name|] |> Set.ofArray
        let fragmentPossibleTypes = fragmentType.PossibleTypes |> Option.defaultValue [||] |> Array.choose (fun x -> x.Name) |> Array.append [|fragmentType.Name|] |> Set.ofArray
        let applicableTypes = Set.intersect parentPossibleTypes fragmentPossibleTypes
        applicableTypes.Count > 0

    let rec private sameResponseShape (fieldA : SelectionInfo, fieldB : SelectionInfo) =
        if fieldA.FieldType = fieldB.FieldType
        then
            let fieldsForName = Dictionary<string, SelectionInfo list>()
            fieldA.NonCyclicSelectionSet 
            |> List.iter (fun selection -> Dictionary.addWith (List.append) selection.AliasOrName [selection] fieldsForName)
            fieldB.NonCyclicSelectionSet 
            |> List.iter (fun selection -> Dictionary.addWith (List.append) selection.AliasOrName [selection] fieldsForName)
            fieldsForName
            |> collectResults (fun (KeyValue (_, selectionSet)) ->
                if selectionSet.Length < 2
                then Success
                else List.pairwise selectionSet |> collectResults sameResponseShape)
        else AstError.AsResult(sprintf "Field name or alias '%s' appears two times, but they do not have the same return types in the scope of the parent type." fieldA.AliasOrName, fieldA.Path)

    let rec private fieldsInSetCanMerge (set : SelectionInfo list) =
        let fieldsForName = set |> List.groupBy (fun x -> x.AliasOrName)
        fieldsForName
        |> collectResults (fun (aliasOrName, selectionSet) ->
            if selectionSet.Length < 2
            then Success
            else
                List.pairwise selectionSet
                |> collectResults (fun (fieldA, fieldB) ->
                    let hasSameShape = sameResponseShape (fieldA, fieldB)
                    if fieldA.FragmentOrParentType = fieldB.FragmentOrParentType || fieldA.FragmentOrParentType.Kind <> TypeKind.OBJECT || fieldB.FragmentOrParentType.Kind <> TypeKind.OBJECT
                    then
                        if fieldA.Field.Name <> fieldB.Field.Name then hasSameShape @@ AstError.AsResult(sprintf "Field name or alias '%s' is referring to fields '%s' and '%s', but they are different fields in the scope of the parent type." aliasOrName fieldA.Field.Name fieldB.Field.Name, fieldA.Path)
                        else if fieldA.Field.Arguments <> fieldB.Field.Arguments then hasSameShape @@ AstError.AsResult(sprintf "Field name or alias '%s' refers to field '%s' two times, but each reference has different argument sets." aliasOrName fieldA.Field.Name, fieldA.Path)
                        else
                            let mergedSet = fieldA.NonCyclicSelectionSet @ fieldB.NonCyclicSelectionSet
                            hasSameShape @@ (fieldsInSetCanMerge mergedSet)
                    else hasSameShape))

    let internal validateFieldSelectionMerging (ctx : ValidationContext) =
        ctx.Definitions |> collectResults (fun def -> fieldsInSetCanMerge def.NonCyclicSelectionSet)

    let rec private checkLeafFieldSelection (selection : SelectionInfo) =
        let rec validateByKind (fieldType : IntrospectionTypeRef) (selectionSetLength : int) =
            match fieldType.Kind with
            | TypeKind.NON_NULL | TypeKind.LIST when fieldType.OfType.IsSome ->
                validateByKind fieldType.OfType.Value selectionSetLength
            | TypeKind.SCALAR | TypeKind.ENUM when selectionSetLength > 0 ->
                AstError.AsResult(sprintf "Field '%s' of '%s' type is of type kind %s, and therefore should not contain inner fields in its selection." selection.Field.Name selection.FragmentOrParentType.Name (fieldType.Kind.ToString()), selection.Path)
            | TypeKind.INTERFACE | TypeKind.UNION | TypeKind.OBJECT when selectionSetLength = 0 ->
                AstError.AsResult(sprintf "Field '%s' of '%s' type is of type kind %s, and therefore should have inner fields in its selection." selection.Field.Name selection.FragmentOrParentType.Name (fieldType.Kind.ToString()), selection.Path)
            | _ -> Success
        let validKind =
            match selection.FieldType with
            | Some fieldType -> validateByKind fieldType selection.SelectionSet.Length
            | None -> Success
        validKind
        @@ (selection.NonCyclicSelectionSet |> collectResults checkLeafFieldSelection)

    let internal validateLeafFieldSelections (ctx : ValidationContext) =
        ctx.Definitions
        |> collectResults (fun def -> def.NonCyclicSelectionSet |> collectResults checkLeafFieldSelection)

    let private checkFieldArgumentNames (schemaInfo : SchemaInfo) (selection : SelectionInfo) =
        let argumentsValid =
            selection.Field.Arguments
            |> collectResults (fun arg ->
                let schemaArgumentNames =
                    metaTypeFields.TryFind(selection.Field.Name)
                    |> Option.map (fun x -> x.ArgumentNames)
                    |> Option.orElseWith (fun () -> selection.InputValues |> Option.map (Array.map (fun x -> x.Name)))
                match schemaArgumentNames |> tryFindInArrayOption (fun x -> x = arg.Name) with
                | Some _ -> Success
                | None -> AstError.AsResult(sprintf "Field '%s' of type '%s' does not have an input named '%s' in its definition." selection.Field.Name selection.FragmentOrParentType.Name arg.Name, selection.Path))
        let directivesValid =
            selection.Field.Directives
            |> collectResults (fun directive ->
                match schemaInfo.Directives |> Array.tryFind (fun d -> d.Name = directive.Name) with
                | Some directiveType ->
                    directive.Arguments
                    |> collectResults (fun arg ->
                        match directiveType.Args |> Array.tryFind (fun argt -> argt.Name = arg.Name) with
                        | Some _ -> Success
                        | _ -> AstError.AsResult(sprintf "Directive '%s' of field '%s' of type '%s' does not have an argument named '%s' in its definition." directiveType.Name selection.Field.Name selection.FragmentOrParentType.Name arg.Name, selection.Path))
                | None -> Success)
        argumentsValid @@ directivesValid

    let internal validateArgumentNames (ctx : ValidationContext) =
        ctx.Definitions
        |> collectResults (fun def -> def.NonCyclicSelectionSet |> collectResults (checkFieldArgumentNames ctx.Schema))

    let rec private checkArgumentUniqueness (selectionSet : SelectionInfo list) =
        let validateArgs (fieldOrDirective : string) (path : Path) (args : Argument list) =
            args
            |> List.countBy(fun x -> x.Name)
            |> collectResults (fun (name, length) ->
                if length > 1
                then AstError.AsResult(sprintf "There are %i arguments with name '%s' defined in %s. Field arguments must be unique." length name fieldOrDirective, path)
                else Success)
        selectionSet
        |> collectResults (fun selection ->
            let argsValid = validateArgs (sprintf "alias or field '%s'" selection.AliasOrName) selection.Path selection.Field.Arguments
            let directiveArgsValid = selection.Field.Directives |> collectResults (fun directive -> validateArgs (sprintf "directive '%s'" directive.Name) selection.Path directive.Arguments)
            let innerArgsValid = checkArgumentUniqueness selection.NonCyclicSelectionSet
            argsValid @@ directiveArgsValid @@ innerArgsValid)

    let internal validateArgumentUniqueness (ctx : ValidationContext) =
        ctx.Definitions
        |> collectResults (fun def -> checkArgumentUniqueness def.NonCyclicSelectionSet)

    let private checkRequiredArguments (schemaInfo : SchemaInfo) (selection : SelectionInfo) =
        let inputsValid =
            selection.InputValues
            |> Option.map(collectResults (fun argDef ->
                match argDef.Type.Kind with
                | TypeKind.NON_NULL when argDef.DefaultValue.IsNone ->
                    match selection.Field.Arguments |> List.tryFind (fun arg -> arg.Name = argDef.Name) with
                    | Some arg when arg.Value <> EnumValue "null" -> Success // TODO: null values are being mapped into an enum value! Isn't it better to have a case for null values?
                    | _ -> AstError.AsResult(sprintf "Argument '%s' of field '%s' of type '%s' is required and does not have a default value." argDef.Name selection.Field.Name selection.FragmentOrParentType.Name, selection.Path)
                | _ -> Success))
            |> Option.defaultValue Success
        let directivesValid =
            selection.Field.Directives
            |> collectResults (fun directive ->
                match schemaInfo.Directives |> Array.tryFind (fun d -> d.Name = directive.Name) with
                | Some directiveType ->
                    directiveType.Args
                    |> collectResults (fun argDef ->
                        match argDef.Type.Kind with
                        | TypeKind.NON_NULL when argDef.DefaultValue.IsNone ->
                            match directive.Arguments |> List.tryFind (fun arg -> arg.Name = argDef.Name) with
                            | Some arg when arg.Value <> EnumValue "null" -> Success // TODO: null values are being mapped into an enum value! Isn't it better to have a case for null values?
                            | _ -> AstError.AsResult(sprintf "Argument '%s' of directive '%s' of field '%s' of type '%s' is required and does not have a default value." argDef.Name directiveType.Name selection.Field.Name selection.FragmentOrParentType.Name, selection.Path)
                        | _ -> Success)
                | None -> Success)
        inputsValid @@ directivesValid

    let internal validateRequiredArguments (ctx : ValidationContext) =
        ctx.Definitions
        |> collectResults (fun def -> def.NonCyclicSelectionSet |> collectResults (checkRequiredArguments ctx.Schema))

    let internal validateFragmentNameUniqueness (ctx : ValidationContext) =
        let counts = Dictionary<string, int>()
        ctx.FragmentDefinitions
        |> List.iter(fun frag -> frag.Definition.Name |> Option.iter(fun name -> Dictionary.addWith (+) name 1 counts))

        counts
        |> collectResults (fun (KeyValue(name, length)) ->
            if length > 1
            then AstError.AsResult(sprintf "There are %i fragments with name '%s' in the document. Fragment definitions must have unique names." length name)
            else Success)

    let rec private checkFragmentTypeExistence (fragmentDefinitions : FragmentDefinition list) (schemaInfo : SchemaInfo) (path : Path) (frag : FragmentDefinition) =
        let typeConditionsValid =
            match frag.TypeCondition |> Option.bind schemaInfo.TryGetTypeByName with
            | Some _ -> Success
            | None when frag.Name.IsSome -> AstError.AsResult(sprintf "Fragment '%s' has type condition '%s', but that type does not exist in the schema." frag.Name.Value frag.TypeCondition.Value)
            | None -> AstError.AsResult(sprintf "Inline fragment has type condition '%s', but that type does not exist in the schema." frag.TypeCondition.Value, path)
        typeConditionsValid @@ (frag.SelectionSet |> collectResults (checkFragmentTypeExistenceInSelection fragmentDefinitions schemaInfo path))

    and private checkFragmentTypeExistenceInSelection (fragmentDefinitions : FragmentDefinition list) (schemaInfo : SchemaInfo) (path : Path) =
        function
        | Field field ->
            let path = field.AliasOrName :: path
            field.SelectionSet
            |> collectResults (checkFragmentTypeExistenceInSelection fragmentDefinitions schemaInfo path)
        | InlineFragment frag -> checkFragmentTypeExistence fragmentDefinitions schemaInfo path frag
        | _ -> Success

    let internal validateFragmentTypeExistence (ctx : ValidationContext) =
        let fragmentDefinitions = getFragmentDefinitions ctx.Document
        ctx.Document.Definitions
        |> collectResults (function
            | FragmentDefinition frag ->
                let path = frag.Name |> Option.toList
                checkFragmentTypeExistence fragmentDefinitions ctx.Schema path frag
            | OperationDefinition odef ->
                let path = odef.Name |> Option.toList
                odef.SelectionSet
                |> collectResults (checkFragmentTypeExistenceInSelection fragmentDefinitions ctx.Schema path))

    let rec private checkFragmentOnCompositeType (selection : SelectionInfo) =
        let fragmentTypeValid =
            match selection.FragmentType with
            | Some fragType ->
                match fragType.Kind with
                | TypeKind.UNION | TypeKind.OBJECT | TypeKind.INTERFACE -> Success
                | _ when selection.FragmentSpreadName.IsSome -> AstError.AsResult(sprintf "Fragment '%s' has type kind %s, but fragments can only be defined in UNION, OBJECT or INTERFACE types." selection.FragmentSpreadName.Value (fragType.Kind.ToString()), selection.Path)
                | _ -> AstError.AsResult(sprintf "Inline fragment has type kind %s, but fragments can only be defined in UNION, OBJECT or INTERFACE types." (fragType.Kind.ToString()), selection.Path)
            | None -> Success
        fragmentTypeValid @@ (selection.NonCyclicSelectionSet |> collectResults checkFragmentOnCompositeType)

    let internal validateFragmentsOnCompositeTypes (ctx : ValidationContext) =
        ctx.Definitions
        |> List.collect (fun def -> def.NonCyclicSelectionSet)
        |> collectResults checkFragmentOnCompositeType

    let internal validateFragmentsMustBeUsed (ctx : ValidationContext) =
        let rec getSpreadNames (acc : Set<string>) =
            function
            | Field field -> field.SelectionSet |> Set.ofList |> Set.collect (getSpreadNames acc)
            | InlineFragment frag -> frag.SelectionSet |> Set.ofList |> Set.collect (getSpreadNames acc)
            | FragmentSpread spread -> acc.Add spread.Name
        let fragmentSpreadNames =
            Set.ofList ctx.Document.Definitions
            |> Set.collect (fun def ->
                Set.ofList def.SelectionSet
                |> Set.collect (getSpreadNames Set.empty))
        getFragmentDefinitions ctx.Document
        |> collectResults (fun def ->
            if def.Name.IsSome && Set.contains def.Name.Value fragmentSpreadNames
            then Success
            else AstError.AsResult(sprintf "Fragment '%s' is not used in any operation in the document. Fragments must be used in at least one operation." def.Name.Value))

    let rec private fragmentSpreadTargetDefinedInSelection (fragmentDefinitionNames : string list) (path : Path) =
        function
        | Field field ->
            let path = field.AliasOrName :: path
            field.SelectionSet
            |> collectResults (fragmentSpreadTargetDefinedInSelection fragmentDefinitionNames path)
        | InlineFragment frag ->
            frag.SelectionSet
            |> collectResults (fragmentSpreadTargetDefinedInSelection fragmentDefinitionNames path)
        | FragmentSpread spread ->
            if List.contains spread.Name fragmentDefinitionNames
            then Success
            else AstError.AsResult(sprintf "Fragment spread '%s' refers to a non-existent fragment definition in the document." spread.Name, path)

    let internal validateFragmentSpreadTargetDefined (ctx : ValidationContext) =
        let fragmentDefinitionNames = ctx.FragmentDefinitions |> List.choose (fun def -> def.Name)
        ctx.Document.Definitions
        |> collectResults (function
            | FragmentDefinition frag ->
                let path = Option.toList frag.Name
                frag.SelectionSet
                |> collectResults (fragmentSpreadTargetDefinedInSelection fragmentDefinitionNames path)
            | OperationDefinition odef ->
                let path = Option.toList odef.Name
                odef.SelectionSet
                |> collectResults (fragmentSpreadTargetDefinedInSelection fragmentDefinitionNames path))

    let rec private checkFragmentMustNotHaveCycles (fragmentDefinitions : FragmentDefinition list) (visited : string list ref) (validated : string list ref) (frag : FragmentDefinition) =
        frag.Name
        |> Option.map (fun name ->
            if List.contains name !visited
            then
                if not (List.contains name !validated) then
                    validated := name :: !validated
                    AstError.AsResult(sprintf "Fragment '%s' is making a cyclic reference." name)
                else Success
            else
                visited := name :: !visited
                frag.SelectionSet
                |> collectResults (checkFragmentsMustNotHaveCyclesInSelection fragmentDefinitions (ref !visited) (ref !validated)))
        |> Option.defaultValue Success

    and private checkFragmentsMustNotHaveCyclesInSelection (fragmentDefinitions : FragmentDefinition list) (visited : string list ref) (validated : string list ref) =
        function
        | Field field ->
            field.SelectionSet
            |> collectResults (checkFragmentsMustNotHaveCyclesInSelection fragmentDefinitions (ref !visited) (ref !validated))
        | InlineFragment frag -> checkFragmentMustNotHaveCycles fragmentDefinitions visited validated frag
        | FragmentSpread spread ->
            match fragmentDefinitions |> List.tryFind (fun f -> f.Name.IsSome && f.Name.Value = spread.Name) with
            | Some frag -> checkFragmentMustNotHaveCycles fragmentDefinitions visited validated frag
            | None -> Success

    let internal validateFragmentsMustNotFormCycles (ctx : ValidationContext) =
        let fragmentDefinitions = ctx.FragmentDefinitions |> List.map (fun x -> x.Definition)
        fragmentDefinitions
        |> collectResults (fun x -> checkFragmentMustNotHaveCycles fragmentDefinitions (ref []) (ref []) x)

    let private checkFragmentSpreadIsPossibleInSelection (path : Path, parentType : IntrospectionType, fragmentType : IntrospectionType) =
        if not (typesAreApplicable (parentType, fragmentType))
        then AstError.AsResult(sprintf "Fragment type condition '%s' is not applicable to the parent type of the field '%s'." fragmentType.Name parentType.Name, path)
        else Success

    let rec private getFragmentAndParentTypes (set : SelectionInfo list) =
        ([], set)
        ||> List.fold(fun acc selection ->
            match selection.FragmentType with
            | Some fragType when fragType.Name <> selection.ParentType.Name -> (selection.Path, selection.ParentType, fragType) :: acc
            | _ -> acc)

    let internal validateFragmentSpreadIsPossible (ctx : ValidationContext) =
        ctx.Definitions
        |> collectResults (fun def ->
            def.NonCyclicSelectionSet
            |> getFragmentAndParentTypes
            |> collectResults (checkFragmentSpreadIsPossibleInSelection))

    let private checkInputValue (schemaInfo : SchemaInfo) (variables : VariableDefinition list option) (selection : SelectionInfo) =
        let rec getFieldMap (fields : (string * IntrospectionTypeRef) seq) : Map<string, IntrospectionTypeRef> =
            (Map.empty, fields)
            ||> Seq.fold (fun acc (name, tref) -> Map.add name tref acc)

        let rec checkIsCoercible (tref : IntrospectionTypeRef) (argName : string) (value : Value) =
            let canNotCoerce = AstError.AsResult(sprintf "Argument field or value named '%s' can not be coerced. It does not match a valid literal representation for the type." argName, selection.Path)
            match value with
            // TODO: null values are being parsed as an Enum. Isn't it better to make an option for null values?
            | EnumValue "null" when tref.Kind = TypeKind.NON_NULL -> AstError.AsResult(sprintf "Argument '%s' value can not be coerced. It's type is non-nullable but the argument has a null value." argName, selection.Path)
            | EnumValue "null" -> Success
            | _ when tref.Kind = TypeKind.NON_NULL -> checkIsCoercible tref.OfType.Value argName value
            | IntValue _ ->
                match tref.Name, tref.Kind with
                | Some ("Int" | "Float"), TypeKind.SCALAR -> Success
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
                | TypeKind.LIST when tref.OfType.IsSome -> values |> collectResults (checkIsCoercible tref.OfType.Value argName)
                | _ -> canNotCoerce
            | ObjectValue props ->
                match tref.Kind with
                | TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION | TypeKind.INPUT_OBJECT when tref.Name.IsSome ->
                    match schemaInfo.TryGetTypeByRef(tref) with
                    | Some itype ->
                        let fieldMap = itype.InputFields |> Option.defaultValue [||] |> Array.map (fun x -> x.Name, x.Type) |> getFieldMap
                        let canCoerceFields =
                            fieldMap
                            |> collectResults (fun kvp ->
                                if kvp.Value.Kind = TypeKind.NON_NULL && not (props.ContainsKey(kvp.Key))
                                then AstError.AsResult (sprintf "Can not coerce argument '%s'. Argument definition '%s' have a required field '%s', but that field does not exist in the literal value for the argument." argName tref.Name.Value kvp.Key, selection.Path)
                                else Success)
                        let canCoerceProps =
                            props
                            |> collectResults (fun kvp ->
                                match Map.tryFind kvp.Key fieldMap with
                                | Some fieldTypeRef -> checkIsCoercible fieldTypeRef kvp.Key kvp.Value
                                | None -> AstError.AsResult(sprintf "Can not coerce argument '%s'. The field '%s' is not a valid field in the argument definition." argName kvp.Key, selection.Path))
                        canCoerceFields @@ canCoerceProps
                    | None -> canNotCoerce
                | _ -> canNotCoerce
            | Variable varName ->
                let variableDefinition =
                    variables
                    |> Option.defaultValue []
                    |> List.tryPick (fun v -> if v.VariableName = varName then Some (v, schemaInfo.TryGetInputType(v.Type)) else None)
                match variableDefinition with
                | Some (vdef, Some vtype) when vdef.DefaultValue.IsSome -> checkIsCoercible vtype argName vdef.DefaultValue.Value
                | Some (vdef, None) when vdef.DefaultValue.IsSome -> canNotCoerce
                | _ -> Success
        selection.Field.Arguments
        |> collectResults (fun arg ->
            let argumentTypeRef = selection.InputValues |> Option.defaultValue [||] |> Array.tryPick (fun x -> if x.Name = arg.Name then Some x.Type else None)
            match argumentTypeRef with
            | Some argumentTypeRef -> checkIsCoercible argumentTypeRef arg.Name arg.Value
            | None -> Success)

    let internal validateInputValues (ctx : ValidationContext) =
        ctx.Definitions
        |> collectResults (fun def ->
            let (vars, selectionSet) =
                match def with
                | OperationDefinitionInfo odef -> (Some odef.Definition.VariableDefinitions, odef.NonCyclicSelectionSet)
                | FragmentDefinitionInfo fdef -> (None, fdef.NonCyclicSelectionSet)
            selectionSet |> collectResults (checkInputValue ctx.Schema vars))

    let rec private getDistinctDirectiveNamesInSelection (path : Path) (selection : Selection) : (Path * Set<string>) list =
        match selection with
        | Field field ->
            let path = field.AliasOrName :: path
            let fieldDirectives = [ path, field.Directives |> List.map (fun x -> x.Name) |> Set.ofList ]
            let selectionSetDirectives = field.SelectionSet |> List.collect (getDistinctDirectiveNamesInSelection path)
            fieldDirectives |> List.append selectionSetDirectives
        | InlineFragment frag -> getDistinctDirectiveNamesInDefinition path (FragmentDefinition frag)
        | FragmentSpread spread -> [ path, spread.Directives |> List.map (fun x -> x.Name) |> Set.ofList ]

    and private getDistinctDirectiveNamesInDefinition (path : Path) (frag : Definition) : (Path * Set<string>) list =
        let fragDirectives = [ path, frag.Directives |> List.map (fun x -> x.Name) |> Set.ofList ]
        let selectionSetDirectives = frag.SelectionSet |> List.collect (getDistinctDirectiveNamesInSelection path)
        fragDirectives |> List.append selectionSetDirectives

    let internal validateDirectivesDefined (ctx : ValidationContext) =
        ctx.Definitions
        |> List.collect (fun def ->
            let path = match def.Name with | Some name -> [name] | None -> []
            getDistinctDirectiveNamesInDefinition path def.Definition)
        |> collectResults (fun (path, names) ->
            names
            |> collectResults (fun name ->
                if ctx.Schema.Directives |> Array.exists (fun x -> x.Name = name)
                then Success
                else AstError.AsResult(sprintf "Directive '%s' is not defined in the schema." name, path)))

    let private validateDirective (schemaInfo : SchemaInfo) (path : Path) (location : DirectiveLocation) (onError : Directive -> string) (directive : Directive) =
        schemaInfo.Directives
        |> collectResults (fun d ->
            if d.Name = directive.Name
            then
                if d.Locations |> Array.contains location then Success
                else AstError.AsResult (onError directive, path)
            else Success)

    let rec private checkDirectivesInSelectionSet (schemaInfo : SchemaInfo) (fragmentDefinitions : FragmentDefinition list) (path : Path) (visitedFragments : string list ref) (selectionSet : Selection list) =
        selectionSet
        |> collectResults (checkDirectivesInValidLocationOnSelection schemaInfo fragmentDefinitions path visitedFragments)

    and private checkDirectivesInFragment (schemaInfo : SchemaInfo)  (fragmentDefinitions : FragmentDefinition list) (path : Path) (visitedFragments : string list ref) (frag : FragmentDefinition) =
        let expectedLocation =
            match frag.Name with
            | Some _ -> DirectiveLocation.FRAGMENT_DEFINITION
            | None -> DirectiveLocation.INLINE_FRAGMENT
        let directivesValid =
            frag.Directives
            |> collectResults (validateDirective schemaInfo path expectedLocation (fun d ->
                match frag.Name with
                | Some fragName -> sprintf "Fragment definition '%s' has a directive '%s', but this directive location is not supported by the schema definition." fragName d.Name
                | None -> sprintf "An inline fragment has a directive '%s', but this directive location is not supported by the schema definition." d.Name))
        match frag.Name with
        | Some fragName when List.contains fragName !visitedFragments -> directivesValid
        | _ -> directivesValid @@ checkDirectivesInSelectionSet schemaInfo fragmentDefinitions path visitedFragments frag.SelectionSet

    and private checkDirectivesInValidLocationOnSelection (schemaInfo : SchemaInfo) (fragmentDefinitions : FragmentDefinition list) (path : Path) (visitedFragments : string list ref) =
        function
        | Field field ->
            let path = field.AliasOrName :: path
            let directivesValid =
                field.Directives
                |> collectResults (validateDirective schemaInfo path DirectiveLocation.FIELD (fun directiveDef ->
                    sprintf "Field or alias '%s' has a directive '%s', but this directive location is not supported by the schema definition." field.AliasOrName directiveDef.Name))
            directivesValid @@ (checkDirectivesInSelectionSet schemaInfo fragmentDefinitions path visitedFragments field.SelectionSet)
        | InlineFragment frag -> checkDirectivesInFragment schemaInfo fragmentDefinitions path visitedFragments frag
        | FragmentSpread spread ->
            spread.Directives
            |> collectResults (validateDirective schemaInfo path DirectiveLocation.FRAGMENT_SPREAD (fun directiveDef ->
                sprintf "Fragment spread '%s' has a directive '%s', but this directive location is not supported by the schema definition." spread.Name directiveDef.Name))

    let private checkDirectivesInOperation (schemaInfo : SchemaInfo) (fragmentDefinitions : FragmentDefinition list) (path : Path) (visitedFragments : string list ref) (operation : OperationDefinition) =
        let expectedLocation =
            match operation.OperationType with
            | Query -> DirectiveLocation.QUERY
            | Mutation -> DirectiveLocation.MUTATION
            | Subscription -> DirectiveLocation.SUBSCRIPTION
        let directivesValid =
            operation.Directives
            |> collectResults (validateDirective schemaInfo path expectedLocation (fun directiveDef ->
                match operation.Name with
                | Some operationName -> sprintf "%s operation '%s' has a directive '%s', but this directive location is not supported by the schema definition." (operation.OperationType.ToString()) operationName directiveDef.Name
                | None -> sprintf "This %s operation has a directive '%s', but this directive location is not supported by the schema definition." (operation.OperationType.ToString()) directiveDef.Name))
        let operationsValid =
            operation.SelectionSet
            |> collectResults (checkDirectivesInValidLocationOnSelection schemaInfo fragmentDefinitions path visitedFragments)
        directivesValid @@ operationsValid

    let internal validateDirectivesAreInValidLocations (ctx : ValidationContext) =
        let fragmentDefinitions = ctx.FragmentDefinitions |> List.map (fun x -> x.Definition)
        let visitedFragments = ref []
        ctx.Document.Definitions
        |> collectResults (fun def ->
            let path = def.Name |> Option.toList
            match def with
            | OperationDefinition odef -> checkDirectivesInOperation ctx.Schema fragmentDefinitions path visitedFragments odef
            | FragmentDefinition frag -> checkDirectivesInFragment ctx.Schema fragmentDefinitions path visitedFragments frag)

    let rec private getDirectiveNamesInSelection (path : Path) (selection : Selection) : (Path * string list) list =
        match selection with
        | Field field ->
            let path = field.AliasOrName :: path
            let fieldDirectives = [ path, field.Directives |> List.map (fun x -> x.Name) ]
            let selectionSetDirectives = field.SelectionSet |> List.collect (getDirectiveNamesInSelection path)
            fieldDirectives |> List.append selectionSetDirectives
        | InlineFragment frag -> getDirectiveNamesInDefinition path (FragmentDefinition frag)
        | FragmentSpread spread -> [ path, spread.Directives |> List.map (fun x -> x.Name) ]

    and private getDirectiveNamesInDefinition (path : Path) (frag : Definition) : (Path * string list) list =
        let fragDirectives = [ path, frag.Directives |> List.map (fun x -> x.Name) ]
        let selectionSetDirectives = frag.SelectionSet |> List.collect (getDirectiveNamesInSelection path)
        fragDirectives |> List.append selectionSetDirectives

    let internal validateUniqueDirectivesPerLocation (ctx : ValidationContext) =
        ctx.Definitions
        |> List.collect (fun def ->
            let path = match def.Name with | Some name -> [name] | None -> []
            let defDirectives = path, def.Directives |> List.map (fun x -> x.Name)
            let selectionSetDirectives = def.Definition.SelectionSet |> List.collect (getDirectiveNamesInSelection path)
            defDirectives :: selectionSetDirectives)
        |> collectResults (fun (path, directives) ->
            directives
            |> List.countBy id
            |> collectResults (fun (name, count) ->
                if count <= 1
                then Success
                else AstError.AsResult(sprintf "Directive '%s' appears %i times in the location it is used. Directives must be unique in their locations." name count, path)))

    let internal validateVariableUniqueness (ctx : ValidationContext) =
        ctx.Document.Definitions
        |> collectResults (function
            | OperationDefinition def ->
                def.VariableDefinitions
                |> List.countBy id
                |> collectResults (fun (var, count) ->
                    match def.Name with
                    | _ when count < 2 -> Success
                    | Some operationName -> AstError.AsResult(sprintf "Variable '%s' in operation '%s' is declared %i times. Variables must be unique in their operations." var.VariableName operationName count)
                    | None -> AstError.AsResult(sprintf "Variable '%s' is declared %i times in the operation. Variables must be unique in their operations." var.VariableName count))
            | _ -> Success)

    let internal validateVariablesAsInputTypes (ctx : ValidationContext) =
        ctx.Document.Definitions
        |> collectResults (function
            | OperationDefinition def ->
                def.VariableDefinitions
                |> collectResults (fun var ->
                    match def.Name, ctx.Schema.TryGetInputType(var.Type) with
                    | Some operationName, None ->
                        AstError.AsResult(sprintf "Variable '%s' in operation '%s' has a type that is not an input type defined by the schema (%s)." var.VariableName operationName (var.Type.ToString()))
                    | None, None ->
                        AstError.AsResult(sprintf "Variable '%s' has a type is not an input type defined by the schema (%s)." var.VariableName (var.Type.ToString()))
                    | _ -> Success)
            | _ -> Success)

    let private checkVariablesDefinedInDirective (variableDefinitions : Set<string>) (path : Path) (directive : Directive) =
        directive.Arguments
        |> collectResults (fun arg ->
            match arg.Value with
            | Variable varName ->
                if variableDefinitions |> Set.contains varName
                then Success
                else AstError.AsResult(sprintf "A variable '%s' is referenced in an argument '%s' of directive '%s' of field with alias or name '%s', but that variable is not defined in the operation." varName arg.Name directive.Name path.Head, path)
            | _ -> Success)

    let rec private checkVariablesDefinedInSelection (fragmentDefinitions : FragmentDefinition list) (visitedFragments : string list ref) (variableDefinitions : Set<string>) (path : Path) =
        function
        | Field field ->
            let path = field.AliasOrName :: path
            let variablesValid =
                field.Arguments
                |> collectResults (fun arg ->
                    match arg.Value with
                    | Variable varName ->
                        if variableDefinitions |> Set.contains varName
                        then Success
                        else AstError.AsResult(sprintf "A variable '%s' is referenced in argument '%s' of field with alias or name '%s', but that variable is not defined in the operation." varName arg.Name field.AliasOrName)
                    | _ -> Success)
            variablesValid
            @@ (field.SelectionSet |> collectResults (checkVariablesDefinedInSelection fragmentDefinitions visitedFragments variableDefinitions path))
            @@ (field.Directives |> collectResults (checkVariablesDefinedInDirective variableDefinitions path))
        | InlineFragment frag ->
            let variablesValid =
                frag.SelectionSet
                |> collectResults (checkVariablesDefinedInSelection fragmentDefinitions visitedFragments variableDefinitions path)
            variablesValid @@ (frag.Directives |> collectResults (checkVariablesDefinedInDirective variableDefinitions path))
        | FragmentSpread spread ->
            if List.contains spread.Name !visitedFragments
            then Success
            else
                visitedFragments := spread.Name :: !visitedFragments
                let spreadValid =
                    match fragmentDefinitions |> List.tryFind (fun x -> x.Name.IsSome && x.Name.Value = spread.Name) with
                    | Some frag ->
                        let variablesValid =
                            frag.SelectionSet
                            |> collectResults (checkVariablesDefinedInSelection fragmentDefinitions visitedFragments variableDefinitions path)
                        variablesValid @@ (frag.Directives |> collectResults (checkVariablesDefinedInDirective variableDefinitions path))
                    | None -> Success
                spreadValid @@ (spread.Directives |> collectResults (checkVariablesDefinedInDirective variableDefinitions path))

    let internal validateVariablesUsesDefined (ctx : ValidationContext) =
        let fragmentDefinitions = getFragmentDefinitions ctx.Document
        let visitedFragments = ref []
        ctx.Document.Definitions
        |> collectResults (function
            | OperationDefinition def ->
                let path = Option.toList def.Name
                let varNames = def.VariableDefinitions |> List.map(fun x -> x.VariableName) |> Set.ofList
                def.SelectionSet |> collectResults (checkVariablesDefinedInSelection fragmentDefinitions visitedFragments varNames path)
            | _ -> Success)

    let rec private argumentsContains (name : string) (args : Argument list) =
        args |> List.exists (fun x -> match x.Value with | Variable varName -> varName = name | _ -> false)

    let rec private variableIsUsedInSelection (name : string) (fragmentDefinitions : FragmentDefinition list) (visitedFragments : string list ref) =
        function
        | Field field ->
            if argumentsContains name field.Arguments
            then true
            else
                let usedInSelection = field.SelectionSet |> List.exists (variableIsUsedInSelection name fragmentDefinitions visitedFragments)
                usedInSelection || (field.Directives |> List.exists (fun directive -> argumentsContains name directive.Arguments))
        | InlineFragment frag ->
                let usedInSelection = frag.SelectionSet |> List.exists (variableIsUsedInSelection name fragmentDefinitions visitedFragments)
                usedInSelection || (frag.Directives |> List.exists (fun directive -> argumentsContains name directive.Arguments))
        | FragmentSpread spread ->
            if List.contains spread.Name !visitedFragments
            then false
            else
                let usedInSpread =
                    match fragmentDefinitions |> List.tryFind (fun x -> x.Name.IsSome && x.Name.Value = spread.Name) with
                    | Some frag ->
                        let usedInSelection = frag.SelectionSet |> List.exists (variableIsUsedInSelection name fragmentDefinitions visitedFragments)
                        usedInSelection || (frag.Directives |> List.exists (fun directive -> argumentsContains name directive.Arguments))
                    | None -> false
                usedInSpread || (spread.Directives |> List.exists (fun directive -> argumentsContains name directive.Arguments))

    let internal validateAllVariablesUsed (ctx : ValidationContext) =
        let fragmentDefinitions = getFragmentDefinitions ctx.Document
        let visitedFragments = ref  []
        ctx.Document.Definitions
        |> collectResults (function
            | OperationDefinition def ->
                def.VariableDefinitions
                |> collectResults (fun varDef ->
                        let isUsed = def.SelectionSet |> List.exists (variableIsUsedInSelection varDef.VariableName fragmentDefinitions visitedFragments)
                        match def.Name, isUsed with
                        | _, true -> Success
                        | Some operationName, _ -> AstError.AsResult(sprintf "Variable definition '%s' is not used in operation '%s'. Every variable must be used." varDef.VariableName operationName)
                        | None, _ -> AstError.AsResult(sprintf "Variable definition '%s' is not used in operation. Every variable must be used." varDef.VariableName))
            | _ -> Success)

    let rec private areTypesCompatible (variableTypeRef : IntrospectionTypeRef) (locationTypeRef : IntrospectionTypeRef) =
        if locationTypeRef.Kind = TypeKind.NON_NULL && locationTypeRef.OfType.IsSome
        then
            if variableTypeRef.Kind <> TypeKind.NON_NULL
            then false
            elif variableTypeRef.OfType.IsSome then areTypesCompatible variableTypeRef.OfType.Value locationTypeRef.OfType.Value
            else false
        elif variableTypeRef.Kind = TypeKind.NON_NULL && variableTypeRef.OfType.IsSome then areTypesCompatible variableTypeRef.OfType.Value locationTypeRef
        elif locationTypeRef.Kind = TypeKind.LIST && locationTypeRef.OfType.IsSome
        then
            if variableTypeRef.Kind <> TypeKind.LIST
            then false
            elif variableTypeRef.OfType.IsSome then areTypesCompatible variableTypeRef.OfType.Value locationTypeRef.OfType.Value
            else false
        elif variableTypeRef.Kind = TypeKind.LIST then false
        else variableTypeRef.Name = locationTypeRef.Name && variableTypeRef.Kind = locationTypeRef.Kind

    let private checkVariableUsageAllowedOnArguments (inputs : IntrospectionInputVal []) (varNamesAndTypeRefs : Map<string, VariableDefinition *  IntrospectionTypeRef>) (path : Path) (args : Argument list) =
        args
        |> collectResults (fun arg ->
            match arg.Value with
            | Variable varName ->
                match varNamesAndTypeRefs.TryFind(varName) with
                | Some (varDef, variableTypeRef) ->
                    let err = AstError.AsResult(sprintf "Variable '%s' can not be used in its reference. The type of the variable definition is not compatible with the type of its reference." varName, path)
                    match inputs |> Array.tryFind (fun x -> x.Name = arg.Name) with
                    | Some input ->
                        let locationTypeRef = input.Type
                        if locationTypeRef.Kind = TypeKind.NON_NULL && locationTypeRef.OfType.IsSome && variableTypeRef.Kind <> TypeKind.NON_NULL
                        then
                            let hasNonNullVariableDefaultValue = varDef.DefaultValue.IsSome
                            let hasLocationDefaultValue = input.DefaultValue.IsSome
                            if not hasNonNullVariableDefaultValue && not hasLocationDefaultValue
                            then err
                            else
                                let nullableLocationType = locationTypeRef.OfType.Value
                                if not (areTypesCompatible variableTypeRef nullableLocationType)
                                then err else Success
                        elif not (areTypesCompatible variableTypeRef locationTypeRef)
                        then err else Success
                    | None -> Success
                | None -> Success
            | _ -> Success)

    let rec private checkVariableUsageAllowedOnSelection (varNamesAndTypeRefs : Map<string, VariableDefinition * IntrospectionTypeRef>) (visitedFragments : string list ref) (selection : SelectionInfo) =
        let inputs = Option.defaultValue [||] selection.InputValues
        match selection.FragmentSpreadName with
        | Some spreadName when List.contains spreadName !visitedFragments -> Success
        | _ ->
            if selection.FragmentSpreadName.IsSome then visitedFragments := selection.FragmentSpreadName.Value :: !visitedFragments
            match selection.FieldType with
            | Some _ ->
                let argumentsValid = 
                    selection.Field.Arguments 
                    |> checkVariableUsageAllowedOnArguments inputs varNamesAndTypeRefs selection.Path
                let selectionValid = 
                    selection.NonCyclicSelectionSet 
                    |> collectResults (checkVariableUsageAllowedOnSelection varNamesAndTypeRefs visitedFragments)
                argumentsValid
                @@ selectionValid
                @@ (selection.Field.Directives |> collectResults (fun directive -> directive.Arguments |> checkVariableUsageAllowedOnArguments inputs varNamesAndTypeRefs selection.Path))
            | None -> Success

    let internal validateVariableUsagesAllowed (ctx : ValidationContext) =
        let visitedFragments = ref []
        ctx.OperationDefinitions
        |> collectResults (fun def ->
            let varNamesAndTypeRefs =
                def.Definition.VariableDefinitions
                |> List.choose (fun varDef -> ctx.Schema.TryGetInputType(varDef.Type) |> Option.map(fun t -> varDef.VariableName, (varDef, t)))
                |> Map.ofList
            def.NonCyclicSelectionSet |> collectResults (checkVariableUsageAllowedOnSelection varNamesAndTypeRefs visitedFragments))

    let private allValidations =
        [ validateFragmentsMustNotFormCycles
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
          validateVariableUsagesAllowed ]

    let validateDocument (schema : IntrospectionSchema) (ast : Document) =
        let schemaInfo = SchemaInfo.FromIntrospectionSchema(schema)
        let context = getValidationContext schemaInfo ast
        allValidations |> collectResults (fun validate -> validate context)
