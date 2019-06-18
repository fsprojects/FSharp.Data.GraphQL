/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Validation

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types.Introspection

module Types =
    type ValidationResult =
        | Success
        | Error of string list
    
    let (@) res1 res2 =
        match res1, res2 with
        | Success, Success -> Success
        | Success, _ -> res2
        | _, Success -> res1
        | Error e1, Error e2 -> Error (e1 @ e2)

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
        | err -> Error err
            
    let validateType typedef =
        match typedef with
        | Scalar _ -> Success
        | Object objdef -> 
            let nonEmptyResult = if objdef.Fields.Count > 0 then Success else Error [ objdef.Name + " must have at least one field defined" ]
            let implementsResult =
                objdef.Implements
                |> Array.fold (fun acc i -> acc @ validateImplements objdef i) Success
            nonEmptyResult @ implementsResult
        | InputObject indef -> 
            let nonEmptyResult = if indef.Fields.Length > 0 then Success else Error [ indef.Name + " must have at least one field defined" ]
            nonEmptyResult
        | Union uniondef ->
            let nonEmptyResult = if uniondef.Options.Length > 0 then Success else Error [ uniondef.Name + " must have at least one type definition option" ]
            nonEmptyResult
        | Enum enumdef -> 
            let nonEmptyResult = if enumdef.Options.Length > 0 then Success else Error [ enumdef.Name + " must have at least one enum value defined" ]
            nonEmptyResult
        | Interface idef -> 
            let nonEmptyResult = if idef.Fields.Length > 0 then Success else Error [ idef.Name + " must have at least one field defined" ]
            nonEmptyResult
        | _ -> failwithf "Unexpected value of typedef: %O" typedef

    let validateTypeMap (namedTypes: TypeMap) : ValidationResult =
        namedTypes.ToSeq()
        |> Seq.map snd
        |> Seq.fold (fun acc namedDef -> acc @ validateType namedDef) Success

module Ast =
    type Path = string list

    type Error = 
        { Message : string
          Path : Path option }
        static member AsResult(message : string, ?path : Path) =
            [ { Message = message; Path = path |> Option.map List.rev } ]
            |> ValidationResult.Error

    and ValidationResult =
        | Success
        | Error of Error list
    
    let (@) res1 res2 =
        match res1, res2 with
        | Success, Success -> Success
        | Success, _ -> res2
        | _, Success -> res1
        | Error e1, Error e2 -> Error (e1 @ e2)

    type SchemaInfo =
        { SchemaTypes : Map<string, IntrospectionType>
          QueryType : IntrospectionType option
          SubscriptionType : IntrospectionType option
          MutationType : IntrospectionType option
          Directives : IntrospectionDirective [] }
        static member private TryGetSchemaTypeByRef (schemaTypes : Map<string, IntrospectionType>) (tref : IntrospectionTypeRef) =
            match tref.Kind with
            | TypeKind.NON_NULL | TypeKind.LIST when tref.OfType.IsSome -> SchemaInfo.TryGetSchemaTypeByRef schemaTypes tref.OfType.Value
            | _ -> tref.Name |> Option.bind schemaTypes.TryFind
        member x.TryGetTypeByRef(tref : IntrospectionTypeRef) =
            SchemaInfo.TryGetSchemaTypeByRef x.SchemaTypes tref
        static member FromIntrospectionSchema(schema : IntrospectionSchema) =
            let schemaTypes = schema.Types |> Seq.map (fun x -> x.Name, x) |> Map.ofSeq
            { SchemaTypes = schema.Types |> Seq.map (fun x -> x.Name, x) |> Map.ofSeq
              QueryType = SchemaInfo.TryGetSchemaTypeByRef schemaTypes schema.QueryType
              MutationType = schema.MutationType |> Option.bind (SchemaInfo.TryGetSchemaTypeByRef schemaTypes)
              SubscriptionType = schema.SubscriptionType |> Option.bind (SchemaInfo.TryGetSchemaTypeByRef schemaTypes)
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
                |> Option.bind (fun x -> match x.Kind with | TypeKind.INPUT_OBJECT | TypeKind.SCALAR -> Some x | _ -> None)
                |> Option.map IntrospectionTypeRef.Named
            | ListType inner -> x.TryGetInputType(inner) |> Option.map IntrospectionTypeRef.List
            | NonNullType inner -> x.TryGetInputType(inner) |> Option.map IntrospectionTypeRef.NonNull
    
    type SelectionInfo =
        { Field : Field
          SelectionSet : SelectionInfo list
          FieldType : IntrospectionTypeRef option
          ParentType : IntrospectionType
          FragmentType : IntrospectionType option
          FragmentSpreadName : string option
          InputValues : IntrospectionInputVal [] option
          Path : Path }
        member x.AliasOrName = x.Field.AliasOrName
        member x.FragmentOrParentType = x.FragmentType |> Option.defaultValue x.ParentType
    
    type OperationDefinitionInfo =
        { Definition : OperationDefinition
          SelectionSet : SelectionInfo list }
        member x.Name = x.Definition.Name
    
    type FragmentDefinitionInfo =
        { Definition : FragmentDefinition
          SelectionSet : SelectionInfo list }
        member x.Name = x.Definition.Name

    type DefinitionInfo =
        | OperationDefinitionInfo of OperationDefinitionInfo
        | FragmentDefinitionInfo of FragmentDefinitionInfo
        member x.Definition =
            match x with
            | OperationDefinitionInfo x -> OperationDefinition x.Definition
            | FragmentDefinitionInfo x -> FragmentDefinition x.Definition
        member x.Name = x.Definition.Name
        member x.SelectionSet =
            match x with
            | OperationDefinitionInfo x -> x.SelectionSet
            | FragmentDefinitionInfo x -> x.SelectionSet
        member x.Directives =
            match x with
            | OperationDefinitionInfo x -> x.Definition.Directives
            | FragmentDefinitionInfo x -> x.Definition.Directives
    
    type ValidationContext =
        { Definitions : DefinitionInfo list
          Schema : SchemaInfo
          Document : Document }
        member x.OperationDefinitions = x.Definitions |> List.choose (function | OperationDefinitionInfo x -> Some x | _ -> None)
        member x.FragmentDefinitions = x.Definitions |> List.choose (function | FragmentDefinitionInfo x -> Some x | _ -> None)

    let rec private getSelectionSetInfo (schemaInfo : SchemaInfo) (fragmentDefinitions : FragmentDefinition list) (parentType : IntrospectionType) (fragmentSpreadName : string option) (fragmentType : IntrospectionType option) (path : Path) (visitedFragments : Map<string, SelectionInfo list> ref) (selectionSet : Selection list) =
        let getFragSelectionInfo (frag : FragmentDefinition) =
            match frag.Name with
            | Some fragName when (!visitedFragments).ContainsKey(fragName) -> (!visitedFragments).[fragName]
            | _ ->
                let parentType =
                    match fragmentType with
                    | Some outerFragmentType -> outerFragmentType
                    | None -> parentType
                let fragmentType = frag.TypeCondition |> Option.bind schemaInfo.TryGetTypeByName
                let fragSelection = getSelectionSetInfo schemaInfo fragmentDefinitions parentType frag.Name fragmentType path visitedFragments frag.SelectionSet
                frag.Name |> Option.iter (fun fragName -> visitedFragments := (!visitedFragments).Add(fragName, fragSelection))
                fragSelection
        selectionSet |> List.collect (function
        | Field field ->
            let ifield = 
                match parentType.Fields |> Option.bind (Array.tryFind (fun f -> f.Name = field.Name)), fragmentType with
                | None, Some fragType -> fragType.Fields |> Option.bind (Array.tryFind (fun f -> f.Name = field.Name))
                | x, _ -> x
            let inputValues = ifield |> Option.map (fun f -> f.Args)
            let fieldType = ifield |> Option.map (fun f -> f.Type)
            let path = field.AliasOrName :: path
            let selectionSet = 
                match fragmentSpreadName with
                | Some fragName when (!visitedFragments).ContainsKey(fragName) -> (!visitedFragments).[fragName]
                | _ -> 
                    let selection = getSelectionSetInfo schemaInfo fragmentDefinitions parentType fragmentSpreadName fragmentType path visitedFragments field.SelectionSet
                    fragmentSpreadName |> Option.iter (fun fragName -> visitedFragments := (!visitedFragments).Add(fragName, selection))
                    selection
            [ { Field = field
                SelectionSet = selectionSet
                FieldType = fieldType
                ParentType = parentType
                FragmentType = fragmentType
                FragmentSpreadName = fragmentSpreadName
                InputValues = inputValues
                Path = path } ]
        | InlineFragment frag -> getFragSelectionInfo frag
        | FragmentSpread spread ->
            match (!visitedFragments).TryFind(spread.Name) with
            | Some fragSelection -> fragSelection
            | None ->
                match fragmentDefinitions |> List.tryFind (fun f -> f.Name.IsSome && f.Name.Value = spread.Name) with
                | Some frag -> 
                    let fragSelection = getFragSelectionInfo frag
                    visitedFragments := (!visitedFragments).Add(spread.Name, fragSelection)
                    fragSelection
                | None -> [])

    let private getOperationDefinitions (ast : Document) =
        ast.Definitions |> List.choose (function | OperationDefinition x -> Some x | _ -> None)

    let private getFragmentDefinitions (ast : Document) =
        ast.Definitions |> List.choose (function | FragmentDefinition x -> Some x | _ -> None)

    let internal getValidationContext (schemaInfo : SchemaInfo) (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        let fragmentInfos = 
            fragmentDefinitions
            |> List.choose (fun def ->
                def.TypeCondition
                |> Option.bind (fun typeCondition ->
                    schemaInfo.TryGetTypeByName(typeCondition)
                    |> Option.map (fun parentType ->
                        let path = match def.Name with | Some name -> [name] | None -> []
                        { Definition = def
                          SelectionSet = getSelectionSetInfo schemaInfo fragmentDefinitions parentType def.Name (Some parentType) path (ref Map.empty) def.SelectionSet  })))
            |> List.map FragmentDefinitionInfo
        let operationInfos =
            getOperationDefinitions ast
            |> List.choose (fun def ->
                schemaInfo.TryGetOperationType(def.OperationType)
                |> Option.map (fun parentType ->
                    let path = match def.Name with | Some name -> [name] | None -> []
                    { OperationDefinitionInfo.Definition = def
                      SelectionSet = getSelectionSetInfo schemaInfo fragmentDefinitions parentType None None path (ref Map.empty) def.SelectionSet }))
            |> List.map OperationDefinitionInfo
        { Definitions = List.append operationInfos fragmentInfos
          Schema = schemaInfo
          Document = ast }

    let validateOperationNameUniqueness (ctx : ValidationContext) =
        let names = ctx.Document.Definitions |> List.choose (fun x -> x.Name)
        names
        |> List.map (fun name -> name, names |> List.filter (fun x -> x = name) |> List.length)
        |> List.distinctBy fst
        |> List.filter (fun (_, count) -> count > 1)
        |> List.fold (fun acc (name, count) -> acc @ Error.AsResult(sprintf "Operation '%s' has %i definitions. Each operation name must be unique." name count)) Success

    let validateLoneAnonymousOperation (ctx : ValidationContext) =
        let operations = ctx.OperationDefinitions |> List.map (fun x -> x.Definition)
        let unamed = operations |> List.filter (fun x -> x.Name.IsNone)
        if unamed.Length = 1 && operations.Length = 1
        then Success
        else Error.AsResult("An anonymous operation must be the only operation in a document. This document has at least one anonymous operation and more than one operation.")

    let validateSubscriptionSingleRootField (ctx : ValidationContext) =
        let fragmentDefinitions = getFragmentDefinitions ctx.Document
        let rec getFieldNames (acc : string list) (selectionSet : Selection list) =
            match selectionSet with
            | [] -> acc
            | selection :: tail ->
                let acc =
                    match selection with
                    | Field field -> field.AliasOrName :: acc
                    | InlineFragment frag -> getFieldNames acc frag.SelectionSet
                    | FragmentSpread spread ->
                        let frag = fragmentDefinitions |> List.tryFind (fun x -> x.Name.IsSome && x.Name.Value = spread.Name)
                        match frag with
                        | Some frag -> getFieldNames acc frag.SelectionSet
                        | None -> acc
                getFieldNames acc tail
        let validate (acc : ValidationResult) (operationName : string option) (fieldNames : string list) =
            if fieldNames.Length <= 1
            then acc
            else
                let rec fieldNamesToString (acc : string) (fieldNames : string list) =
                    match fieldNames with
                    | [] -> acc
                    | [last] -> acc + " " + last
                    | fieldName :: tail -> fieldNamesToString (acc + fieldName + ", ") tail
                let fieldNamesAsString = fieldNamesToString "" fieldNames
                match operationName with
                | Some operationName -> acc @ Error.AsResult(sprintf "Subscription operations should have only one root field. Operation '%s' has %i fields (%s)." operationName fieldNames.Length fieldNamesAsString)
                | None -> acc @ Error.AsResult(sprintf "Subscription operations should have only one root field. Operation has %i fields (%s)." fieldNames.Length fieldNamesAsString)
        ctx.Document.Definitions
        |> List.choose (function 
            | OperationDefinition x -> if x.OperationType = Subscription then Some (x.Name, getFieldNames [] x.SelectionSet) else None 
            | _ -> None)
        |> List.fold (fun acc (name, fieldNames) -> validate acc name fieldNames) Success

    let validateSelectionFieldTypes (ctx : ValidationContext) =
        let rec validateFragmentDefinition (acc : ValidationResult) (frag : FragmentDefinitionInfo)  =
           let fdef = frag.Definition
           match fdef.TypeCondition with
           | Some typeCondition ->
               match ctx.Schema.TryGetTypeByName typeCondition with
               | Some fragType -> checkFieldsOfType acc frag.SelectionSet fragType
               | None when fdef.Name.IsSome -> acc @ Error.AsResult(sprintf "Fragment '%s' has type condition '%s', but that type does not exist in schema definition." fdef.Name.Value typeCondition)
               | None -> acc @ Error.AsResult(sprintf "A fragment has type condition '%s', but that type does not exist in schema definition." typeCondition)
           | None -> acc
        and checkFieldsOfType (acc : ValidationResult) (selectionSet : SelectionInfo list) (objectType : IntrospectionType) =
            match selectionSet with
            | [] -> acc
            | selection :: tail ->
                let acc =
                    let exists = objectType.Fields |> Option.map (Array.exists (fun f -> f.Name = selection.Field.Name)) |> Option.defaultValue false
                    if not exists
                    then acc @ Error.AsResult(sprintf "Field '%s' is not defined in schema type '%s'." selection.Field.Name objectType.Name, selection.Path)
                    else acc
                checkFieldsOfType acc tail objectType
        ctx.Definitions
        |> List.fold (fun acc def ->
            match def with
            | OperationDefinitionInfo odef ->
                match ctx.Schema.TryGetOperationType(odef.Definition.OperationType) with
                | Some otype -> checkFieldsOfType acc odef.SelectionSet otype
                | None when odef.Definition.Name.IsNone -> acc @ Error.AsResult("Could not find an operation type in the schema.")
                | None -> acc @ Error.AsResult(sprintf "Could not find operation '%s' in the schema." odef.Definition.Name.Value)
            | FragmentDefinitionInfo frag -> validateFragmentDefinition acc frag) Success

    let private typesAreApplicable (parentType : IntrospectionType, fragmentType : IntrospectionType) =
        let parentPossibleTypes = parentType.PossibleTypes |> Option.defaultValue [||] |> Array.choose (fun x -> x.Name) |> Array.append [|parentType.Name|] |> Set.ofArray
        let fragmentPossibleTypes = fragmentType.PossibleTypes |> Option.defaultValue [||] |> Array.choose (fun x -> x.Name) |> Array.append [|fragmentType.Name|] |> Set.ofArray
        let applicableTypes = Set.intersect parentPossibleTypes fragmentPossibleTypes
        applicableTypes.Count > 0

    let rec private sameResponseShape (acc : ValidationResult) (fieldA : SelectionInfo, fieldB : SelectionInfo) =
        if fieldA.FieldType = fieldB.FieldType
        then
            let mergedSet = fieldA.SelectionSet |> List.append fieldB.SelectionSet
            let fieldsForName = mergedSet |> List.groupBy (fun x -> x.AliasOrName)
            fieldsForName
            |> List.map snd
            |> List.fold (fun acc selectionSet ->
                if selectionSet.Length < 2
                then acc
                else List.pairwise selectionSet |> List.fold (fun acc pairs -> sameResponseShape acc pairs) acc) acc
        else acc @ Error.AsResult(sprintf "Field name or alias '%s' appears two times, but they do not have the same return types in the scope of the parent type." fieldA.AliasOrName, fieldA.Path)

    let rec private fieldsInSetCanMerge (set : SelectionInfo list) =
        let fieldsForName = set |> List.groupBy (fun x -> x.AliasOrName)
        fieldsForName
        |> List.fold (fun acc (aliasOrName, selectionSet) ->
            if selectionSet.Length < 2
            then acc
            else
                List.pairwise selectionSet
                |> List.map (fun pairs -> pairs, sameResponseShape acc pairs)
                |> List.map (fun ((fieldA, fieldB), acc) ->
                    if fieldA.FragmentOrParentType = fieldB.FragmentOrParentType || fieldA.FragmentOrParentType.Kind <> TypeKind.OBJECT || fieldB.FragmentOrParentType.Kind <> TypeKind.OBJECT
                    then
                        if fieldA.Field.Name <> fieldB.Field.Name then acc @ Error.AsResult(sprintf "Field name or alias '%s' is referring to fields '%s' and '%s', but they are different fields in the scope of the parent type." aliasOrName fieldA.Field.Name fieldB.Field.Name, fieldA.Path)
                        else if fieldA.Field.Arguments <> fieldB.Field.Arguments then acc @ Error.AsResult(sprintf "Field name or alias '%s' refers to field '%s' two times, but each reference has different argument sets." aliasOrName fieldA.Field.Name, fieldA.Path)
                        else
                            let mergedSet = fieldA.SelectionSet |> List.append fieldB.SelectionSet
                            acc @ (fieldsInSetCanMerge mergedSet)
                    else acc)
                |> List.reduce (@)) Success

    let validateFieldSelectionMerging (ctx : ValidationContext) =
        ctx.Definitions |> List.fold (fun acc def -> acc @ (fieldsInSetCanMerge def.SelectionSet)) Success

    let rec private checkLeafFieldSelection (acc : ValidationResult) (selection : SelectionInfo) =
        let rec validateByKind (acc : ValidationResult) (fieldType : IntrospectionTypeRef) (selectionSetLength : int) =
            match fieldType.Kind with
            | TypeKind.NON_NULL | TypeKind.LIST when fieldType.OfType.IsSome -> 
                validateByKind acc fieldType.OfType.Value selectionSetLength
            | TypeKind.SCALAR | TypeKind.ENUM when selectionSetLength > 0 ->
                acc @ Error.AsResult(sprintf "Field '%s' of '%s' type is of type kind %s, and therefore should not contain inner fields in its selection." selection.Field.Name selection.FragmentOrParentType.Name (fieldType.Kind.ToString()), selection.Path)
            | TypeKind.INTERFACE | TypeKind.UNION | TypeKind.OBJECT when selectionSetLength = 0 ->
                acc @ Error.AsResult(sprintf "Field '%s' of '%s' type is of type kind %s, and therefore should have inner fields in its selection." selection.Field.Name selection.FragmentOrParentType.Name (fieldType.Kind.ToString()), selection.Path)
            | _ -> acc
        let acc =
            match selection.FieldType with
            | Some fieldType -> validateByKind acc fieldType selection.SelectionSet.Length
            | None -> acc
        selection.SelectionSet |> List.fold (fun acc selection -> checkLeafFieldSelection acc selection) acc

    let validateLeafFieldSelections (ctx : ValidationContext) =
        ctx.Definitions |> List.fold (fun acc def -> def.SelectionSet |> List.fold (fun acc selection -> checkLeafFieldSelection acc selection) acc) Success

    let private checkFieldArgumentNames (acc : ValidationResult) (schemaInfo : SchemaInfo) (selection : SelectionInfo) =
        let acc =
            selection.Field.Arguments
            |> List.fold (fun acc arg ->
                match selection.InputValues |> Option.map (Array.tryFind (fun d -> d.Name = arg.Name)) |> Option.flatten with
                | Some _ -> acc
                | None -> acc @ Error.AsResult(sprintf "Field '%s' of type '%s' does not have an input named '%s' in its definition." selection.Field.Name selection.FragmentOrParentType.Name arg.Name, selection.Path)) acc
        selection.Field.Directives
        |> List.fold (fun acc directive ->
            match schemaInfo.Directives |> Array.tryFind (fun d -> d.Name = directive.Name) with
            | Some directiveType ->
                directive.Arguments
                |> List.fold (fun acc arg ->
                    match directiveType.Args |> Array.tryFind (fun argt -> argt.Name = arg.Name) with
                    | Some _ -> acc
                    | None -> acc @ Error.AsResult(sprintf "Directive '%s' of field '%s' of type '%s' does not have an argument named '%s' in its definition." directiveType.Name selection.Field.Name selection.FragmentOrParentType.Name arg.Name, selection.Path)) acc
            | None -> acc) acc

    let validateArgumentNames (ctx : ValidationContext) =
        ctx.Definitions
        |> List.fold (fun acc def -> def.SelectionSet |> List.fold (fun acc selection -> checkFieldArgumentNames acc ctx.Schema selection) acc) Success

    let rec private checkArgumentUniqueness (acc : ValidationResult) (selectionSet : SelectionInfo list) =
        match selectionSet with
        | [] -> acc
        | selection :: tail ->
            let acc =
                selection.Field.Arguments
                |> List.groupBy (fun x -> x.Name)
                |> List.map (fun (name, args) -> name, args.Length)
                |> List.fold (fun acc (name, length) ->
                    if length > 1
                    then acc @ Error.AsResult(sprintf "More than one argument named '%s' was defined in field '%s'. Field arguments must be unique." name selection.Field.Name, selection.Path)
                    else acc) acc
            let acc = selection.SelectionSet |> checkArgumentUniqueness acc
            checkArgumentUniqueness acc tail

    let validateArgumentUniqueness (ctx : ValidationContext) =
        ctx.Definitions
        |> List.map (fun def -> def.SelectionSet)
        |> List.fold (fun acc selectionSet -> checkArgumentUniqueness acc selectionSet) Success

    let private checkRequiredArguments (acc : ValidationResult) (schemaInfo : SchemaInfo) (selection : SelectionInfo) =
        let acc =
            selection.InputValues
            |> Option.map (Array.fold (fun acc argDef ->
                match argDef.Type.Kind with
                | TypeKind.NON_NULL when argDef.DefaultValue.IsNone ->
                    match selection.Field.Arguments |> List.tryFind (fun arg -> arg.Name = argDef.Name) with
                    | Some arg when arg.Value <> EnumValue "null" -> acc // TODO: null values are being mapped into an enum value! Isn't it better to have a case for null values?
                    | _ -> acc @ Error.AsResult(sprintf "Argument '%s' of field '%s' of type '%s' is required and does not have a default value." argDef.Name selection.Field.Name selection.FragmentOrParentType.Name, selection.Path)
                | _ -> acc) acc)
            |> Option.defaultValue acc
        selection.Field.Directives
        |> List.fold (fun acc directive ->
            match schemaInfo.Directives |> Array.tryFind (fun d -> d.Name = directive.Name) with
            | Some directiveType ->
                directiveType.Args
                |> Array.fold (fun acc argDef ->
                    match argDef.Type.Kind with
                    | TypeKind.NON_NULL when argDef.DefaultValue.IsNone ->
                        match selection.Field.Arguments |> List.tryFind (fun arg -> arg.Name = argDef.Name) with
                        | Some arg when arg.Value <> EnumValue "null" -> acc // TODO: null values are being mapped into an enum value! Isn't it better to have a case for null values?
                        | _ -> acc @ Error.AsResult(sprintf "Argument '%s' of directive '%s' of field '%s' of type '%s' is required and does not have a default value." argDef.Name directiveType.Name selection.Field.Name selection.FragmentOrParentType.Name, selection.Path)
                    | _ -> acc) acc
            | None -> acc) acc

    let validateRequiredArguments (ctx : ValidationContext) =
        ctx.Definitions
        |> List.fold (fun acc def -> def.SelectionSet |> List.fold (fun acc selection -> checkRequiredArguments acc ctx.Schema selection) acc) Success

    let validateFragmentNameUniqueness (ctx : ValidationContext) =
        ctx.FragmentDefinitions
        |> List.filter (fun f -> f.Definition.Name.IsSome)
        |> List.groupBy (fun f -> f.Definition.Name.Value)
        |> List.choose (fun (name, frags) -> if frags.Length > 1 then Some (name, frags.Length) else None)
        |> List.fold (fun acc (name, length) -> acc @ Error.AsResult(sprintf "There are %i fragments with name '%s' in the document. Fragment definitions must have unique names." length name)) Success

    let rec private checkFragmentTypeExistence (acc : ValidationResult) (fragmentDefinitions : FragmentDefinition list) (schemaInfo : SchemaInfo) (path : Path) (frag : FragmentDefinition) =
        let acc =
            match frag.TypeCondition |> Option.map schemaInfo.TryGetTypeByName |> Option.flatten with
            | Some _ -> acc
            | None when frag.Name.IsSome -> acc @ Error.AsResult(sprintf "Fragment '%s' has type condition '%s', but that type does not exist in the schema." frag.Name.Value frag.TypeCondition.Value)
            | None -> acc @ Error.AsResult(sprintf "Inline fragment has type condition '%s', but that type does not exist in the schema." frag.TypeCondition.Value, path)
        match frag.SelectionSet with
        | [] -> acc
        | selectionSet -> selectionSet |> List.map (checkFragmentTypeExistenceInSelection acc fragmentDefinitions schemaInfo path) |> List.reduce (@)

    and private checkFragmentTypeExistenceInSelection (acc : ValidationResult) (fragmentDefinitions : FragmentDefinition list) (schemaInfo : SchemaInfo) (path : Path) =
        function
        | Field field -> 
            let path = field.AliasOrName :: path
            match field.SelectionSet |> List.map (checkFragmentTypeExistenceInSelection acc fragmentDefinitions schemaInfo path) with
            | [] -> acc
            | results -> List.reduce (@) results
        | InlineFragment frag -> checkFragmentTypeExistence acc fragmentDefinitions schemaInfo path frag
        | _ -> acc
       
    let validateFragmentTypeExistence (ctx : ValidationContext) =
        let fragmentDefinitions = getFragmentDefinitions ctx.Document
        ctx.Document.Definitions
        |> List.fold (fun acc def ->
            match def with
            | FragmentDefinition frag -> 
                let path = match frag.Name with | Some name -> [name] | None -> []
                checkFragmentTypeExistence acc fragmentDefinitions ctx.Schema path frag
            | OperationDefinition odef -> 
                let path = match odef.Name with | Some name -> [name] | None -> []
                match odef.SelectionSet |> List.map (checkFragmentTypeExistenceInSelection acc fragmentDefinitions ctx.Schema path) with
                | [] -> acc
                | results -> List.reduce (@) results) Success

    let rec private checkFragmentOnCompositeType (acc : ValidationResult) (selection : SelectionInfo) =
        let acc =
            match selection.FragmentType with
            | Some fragType ->
                match fragType.Kind with
                | TypeKind.UNION | TypeKind.OBJECT | TypeKind.INTERFACE -> acc
                | _ when selection.FragmentSpreadName.IsSome -> acc @ Error.AsResult(sprintf "Fragment '%s' has type kind %s, but fragments can only be defined in UNION, OBJECT or INTERFACE types." selection.FragmentSpreadName.Value (fragType.Kind.ToString()), selection.Path)
                | _ -> acc @ Error.AsResult(sprintf "Inline fragment has type kind %s, but fragments can only be defined in UNION, OBJECT or INTERFACE types." (fragType.Kind.ToString()), selection.Path)
            | None -> acc
        selection.SelectionSet |> List.fold (fun acc selection -> checkFragmentOnCompositeType acc selection) acc

    let validateFragmentsOnCompositeTypes (ctx : ValidationContext) =
        ctx.Definitions
        |> List.collect (fun def -> def.SelectionSet)
        |> List.fold (fun acc selection -> checkFragmentOnCompositeType acc selection) Success

    let validateFragmentsMustBeUsed (ctx : ValidationContext) =
        let rec getSpreadNames (acc : string list) =
            function
            | Field field -> field.SelectionSet |> List.collect (getSpreadNames acc)
            | InlineFragment frag -> frag.SelectionSet |> List.collect (getSpreadNames acc)
            | _ -> acc
        let fragmentSpreadNames =
            ctx.Document.Definitions
            |> List.collect (function
                | FragmentDefinition frag -> frag.SelectionSet |> List.collect (getSpreadNames [])
                | OperationDefinition odef -> odef.SelectionSet |> List.collect (getSpreadNames []))
            |> List.distinct
        getFragmentDefinitions ctx.Document
        |> List.fold (fun acc def ->
            if def.Name.IsSome && List.contains def.Name.Value fragmentSpreadNames
            then acc
            else acc @ Error.AsResult(sprintf "Fragment '%s' is not used in any operation in the document. Fragments must be used in at least one operation." def.Name.Value)) Success

    let rec private fragmentSpreadTargetDefinedInSelection (acc : ValidationResult) (fragmentDefinitionNames : string list) (path : Path) =
        function
        | Field field -> 
            let path = field.AliasOrName :: path
            match field.SelectionSet |> List.map (fragmentSpreadTargetDefinedInSelection acc fragmentDefinitionNames path) with
            | [] -> acc
            | results -> List.reduce (@) results
        | InlineFragment frag ->
            match frag.SelectionSet |> List.map (fragmentSpreadTargetDefinedInSelection acc fragmentDefinitionNames path) with
            | [] -> acc
            | results -> List.reduce (@) results
        | FragmentSpread spread ->
            if List.contains spread.Name fragmentDefinitionNames
            then acc
            else acc @ Error.AsResult(sprintf "Fragment spread '%s' refers to a non-existent fragment definition in the document." spread.Name, path)

    let validateFragmentSpreadTargetDefined (ctx : ValidationContext) =
        let fragmentDefinitionNames = ctx.FragmentDefinitions |> List.choose (fun def -> def.Name)
        ctx.Document.Definitions
        |> List.fold (fun acc def ->
            match def with
            | FragmentDefinition frag -> 
                let path = match frag.Name with | Some name -> [name] | None -> []
                match frag.SelectionSet |> List.map (fragmentSpreadTargetDefinedInSelection acc fragmentDefinitionNames path) with
                | [] -> acc
                | results -> List.reduce (@) results
            | OperationDefinition odef ->
                let path = match odef.Name with | Some name -> [name] | None -> []
                match odef.SelectionSet |> List.map (fragmentSpreadTargetDefinedInSelection acc fragmentDefinitionNames path) with
                | [] -> acc
                | results -> List.reduce (@) results) Success

    let rec private checkFragmentMustNotHaveCycles (acc : ValidationResult) (fragmentDefinitions : FragmentDefinition list) (visited : string list ref) (frag : FragmentDefinition) =
        frag.Name 
        |> Option.map (fun name ->
            visited := name :: !visited
            if List.contains name !visited
            then acc @ Error.AsResult(sprintf "Fragment '%s' is making a cyclic reference." name)
            else
                match frag.SelectionSet |> List.map (checkFragmentsMustNotHaveCyclesInSelection acc fragmentDefinitions visited) with
                | [] -> acc
                | results -> List.reduce (@) results)
        |> Option.defaultValue acc

    and private checkFragmentsMustNotHaveCyclesInSelection (acc : ValidationResult) (fragmentDefinitions : FragmentDefinition list) (visited : string list ref) =
        function
        | Field field -> 
            match field.SelectionSet |> List.map (checkFragmentsMustNotHaveCyclesInSelection acc fragmentDefinitions visited) with
            | [] -> acc
            | results -> List.reduce (@) results
        | InlineFragment frag -> checkFragmentMustNotHaveCycles acc fragmentDefinitions visited frag
        | FragmentSpread spread ->
            visited := spread.Name :: !visited
            if List.contains spread.Name !visited
            then acc @ Error.AsResult(sprintf "Fragment '%s' is making a cyclic reference." spread.Name)
            else
                match fragmentDefinitions |> List.tryFind (fun f -> f.Name.IsSome && f.Name.Value = spread.Name) with
                | Some frag -> checkFragmentMustNotHaveCycles acc fragmentDefinitions visited frag
                | None -> acc

    let validateFragmentsMustNotFormCycles (ctx : ValidationContext) =
        let fragmentDefinitions = ctx.FragmentDefinitions |> List.map (fun x -> x.Definition)
        fragmentDefinitions |> List.fold (fun acc def -> checkFragmentMustNotHaveCycles acc fragmentDefinitions (ref []) def) Success

    let private checkFragmentSpreadIsPossibleInSelection (acc : ValidationResult) (path : Path, parentType : IntrospectionType, fragmentType : IntrospectionType) =
        if not (typesAreApplicable (parentType, fragmentType))
        then acc @ Error.AsResult(sprintf "Fragment type condition '%s' is not applicable to the parent type of the field '%s'." fragmentType.Name parentType.Name, path)
        else acc

    let rec private getFragmentAndParentTypes (acc : (Path * IntrospectionType * IntrospectionType) list) (set : SelectionInfo list) =
        match set with
        | [] -> acc
        | selection :: tail ->
            let acc =
                match selection.FragmentType with
                | Some fragType when fragType.Name <> selection.ParentType.Name -> (selection.Path, selection.ParentType, fragType) :: acc
                | _ -> acc
            getFragmentAndParentTypes acc tail

    let validateFragmentSpreadIsPossible (ctx : ValidationContext) =
        ctx.Definitions
        |> List.fold (fun acc def ->
            def.SelectionSet
            |> getFragmentAndParentTypes []
            |> List.fold (fun acc types -> checkFragmentSpreadIsPossibleInSelection acc types) acc) Success

    let private checkInputValue (acc : ValidationResult) (schemaInfo : SchemaInfo) (variables : VariableDefinition list option) (selection : SelectionInfo) =
        let rec getFieldMap (acc : Map<string, IntrospectionTypeRef>) (fields : (string * IntrospectionTypeRef) list) : Map<string, IntrospectionTypeRef> =
            match fields with
            | [] -> acc
            | (name, tref) :: fields -> getFieldMap (acc.Add(name, tref)) fields
        let rec checkIsCoercible (tref : IntrospectionTypeRef) (argName : string) (value : Value) =
            let canNotCoerce = Error.AsResult(sprintf "Argument field or value named '%s' can not be coerced. It does not match a valid literal representation for the type." argName, selection.Path)
            match value with
            // TODO: null values are being parsed as an Enum. Isn't it better to make an option for null values?
            | EnumValue "null" when tref.Kind = TypeKind.NON_NULL -> Error.AsResult(sprintf "Argument '%s' value can not be coerced. It's type is non-nullable but the argument has a null value." argName, selection.Path)
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
                match tref.Name, tref.Kind with
                | Some ("String" | "URI" | "ID"), TypeKind.SCALAR -> Success
                | _ -> canNotCoerce
            | EnumValue _ ->
                match tref.Kind with
                | TypeKind.ENUM -> Success
                | _ -> canNotCoerce
            | ListValue values ->
                match tref.Kind with
                | TypeKind.LIST when tref.OfType.IsSome && values.Length > 0 -> values |> List.map (checkIsCoercible tref.OfType.Value argName) |> List.reduce (@)
                | TypeKind.LIST when tref.OfType.IsSome -> acc
                | _ -> canNotCoerce
            | ObjectValue props ->
                match tref.Kind with
                | TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION | TypeKind.INPUT_OBJECT when tref.Name.IsSome ->
                    match schemaInfo.TryGetTypeByRef(tref) with
                    | Some itype ->
                        let fieldMap = itype.InputFields |> Option.defaultValue [||] |> Array.map (fun x -> x.Name, x.Type) |> List.ofArray |> getFieldMap Map.empty
                        let acc =
                            fieldMap |> Seq.fold (fun acc kvp -> 
                                if kvp.Value.Kind = TypeKind.NON_NULL && not (props.ContainsKey(kvp.Key))
                                then acc @ Error.AsResult (sprintf "Can not coerce argument '%s'. Argument definition '%s' have a required field '%s', but that field does not exist in the literal value for the argument." argName tref.Name.Value kvp.Key, selection.Path)
                                else acc) acc
                        props |> Seq.fold (fun acc kvp ->
                            match fieldMap.TryFind(kvp.Key) with
                            | Some fieldTypeRef -> checkIsCoercible fieldTypeRef kvp.Key kvp.Value
                            | None -> acc @ Error.AsResult(sprintf "Can not coerce argument '%s'. The field '%s' is not a valid field in the argument definition." argName kvp.Key, selection.Path)) acc
                    | None -> canNotCoerce
                | _ -> canNotCoerce
            | Variable varName ->
                let variableDefinition = 
                    variables
                    |> Option.defaultValue [] 
                    |> List.tryFind (fun v -> v.VariableName = varName)
                    |> Option.map (fun v -> v, schemaInfo.TryGetInputType(v.Type))
                match variableDefinition with
                | Some (vdef, Some vtype) when vdef.DefaultValue.IsSome -> checkIsCoercible vtype argName vdef.DefaultValue.Value
                | Some (vdef, None) when vdef.DefaultValue.IsSome -> canNotCoerce
                | _ -> acc
        selection.Field.Arguments
        |> List.fold (fun acc arg ->
            let argumentTypeRef = selection.InputValues |> Option.defaultValue [||] |> Array.tryFind (fun x -> x.Name = arg.Name) |> Option.map (fun x -> x.Type)
            match argumentTypeRef with
            | Some argumentTypeRef -> acc @ (checkIsCoercible argumentTypeRef arg.Name arg.Value)
            | None -> acc) acc

    let validateInputValues (ctx : ValidationContext) =
        ctx.Definitions
        |> List.choose (function
            | OperationDefinitionInfo odef -> Some (Some odef.Definition.VariableDefinitions, odef.SelectionSet)
            | FragmentDefinitionInfo fdef -> Some (None, fdef.SelectionSet))
        |> List.fold (fun acc (vars, selectionSet) -> selectionSet |> List.fold (fun acc selection -> checkInputValue acc ctx.Schema vars selection) acc) Success

    let rec private getDistinctDirectiveNamesInSelection (path : Path) (selection : Selection) : (Path * string list) list =
        match selection with
        | Field field ->
            let path = field.AliasOrName :: path
            let fieldDirectives = [ path, field.Directives |> List.map (fun x -> x.Name) |> List.distinct ]
            let selectionSetDirectives = field.SelectionSet |> List.collect (getDistinctDirectiveNamesInSelection path)
            fieldDirectives |> List.append selectionSetDirectives
        | InlineFragment frag -> getDistinctDirectiveNamesInDefinition path (FragmentDefinition frag)
        | FragmentSpread spread -> [ path, spread.Directives |> List.map (fun x -> x.Name) |> List.distinct ]

    and private getDistinctDirectiveNamesInDefinition (path : Path) (frag : Definition) : (Path * string list) list =
        let fragDirectives = [ path, frag.Directives |> List.map (fun x -> x.Name) |> List.distinct ]
        let selectionSetDirectives = frag.SelectionSet |> List.collect (getDistinctDirectiveNamesInSelection path)
        fragDirectives |> List.append selectionSetDirectives

    let validateDirectivesDefined (ctx : ValidationContext) =
        ctx.Definitions
        |> List.collect (fun def ->
            let path = match def.Name with | Some name -> [name] | None -> []
            getDistinctDirectiveNamesInDefinition path def.Definition)
        |> List.fold (fun acc (path, names) ->
            names |> List.fold (fun acc name ->
                if ctx.Schema.Directives |> Array.exists (fun x -> x.Name = name)
                then acc
                else acc @ Error.AsResult(sprintf "Directive '%s' is not defined in the schema." name, path)) acc) Success

    let rec private checkDirectivesInSelectionSet (acc : ValidationResult) (schemaInfo : SchemaInfo) (fragmentDefinitions : FragmentDefinition list) (path : Path) (selectionSet : Selection list) =
        match selectionSet |> List.map (checkDirectivesInValidLocationOnSelection acc schemaInfo fragmentDefinitions path) with
        | [] -> acc
        | results -> List.reduce (@) results

    and private checkDirectivesInFragment (acc : ValidationResult) (schemaInfo : SchemaInfo)  (fragmentDefinitions : FragmentDefinition list) (path : Path) (visitedFragments : string list ref) (frag : FragmentDefinition) =
        let acc =
            frag.Directives
            |> List.fold (fun acc directive ->
                match schemaInfo.Directives |> Array.tryFind (fun x -> x.Name = directive.Name) with
                | Some directiveDef ->
                    let expectedLocation =
                        match frag.Name with
                        | Some _ -> DirectiveLocation.FRAGMENT_DEFINITION
                        | None -> DirectiveLocation.INLINE_FRAGMENT
                    if directiveDef.Locations |> Array.contains expectedLocation
                    then acc
                    else 
                        match frag.Name with
                        | Some fragName -> acc @ Error.AsResult(sprintf "Fragment definition '%s' has a directive '%s', but this directive location is not supported by the schema definition." fragName directive.Name, path)
                        | None -> acc @ Error.AsResult(sprintf "An inline fragment has a directive '%s', but this directive location is not supported by the schema definition." directive.Name, path)
                | None -> acc) acc
        match frag.Name with
        | Some fragName when List.contains fragName !visitedFragments -> acc
        | _ -> checkDirectivesInSelectionSet acc schemaInfo fragmentDefinitions path frag.SelectionSet

    and private checkDirectivesInValidLocationOnSelection (acc : ValidationResult) (schemaInfo : SchemaInfo) (fragmentDefinitions : FragmentDefinition list) (path : Path) =
        function
        | Field field ->
            let path = field.AliasOrName :: path
            let acc =
                field.Directives
                |> List.fold (fun acc directive ->
                    match schemaInfo.Directives |> Array.tryFind (fun x -> x.Name = directive.Name) with
                    | Some directiveDef ->
                        if directiveDef.Locations |> Array.contains DirectiveLocation.FIELD
                        then acc
                        else acc @ Error.AsResult(sprintf "Field or alias '%s' has a directive '%s', but this directive location is not supported by the schema definition." field.AliasOrName directive.Name, path)
                    | None -> acc) acc
            checkDirectivesInSelectionSet acc schemaInfo fragmentDefinitions path field.SelectionSet
        | InlineFragment frag -> checkDirectivesInFragment acc schemaInfo fragmentDefinitions path (ref []) frag
        | FragmentSpread spread ->
            spread.Directives
            |> List.fold (fun acc directive ->
                match schemaInfo.Directives |> Array.tryFind (fun x -> x.Name = directive.Name) with
                | Some directiveDef ->
                    if directiveDef.Locations |> Array.contains DirectiveLocation.FRAGMENT_SPREAD
                    then acc
                    else acc @ Error.AsResult(sprintf "Fragment spread '%s' has a directive '%s', but this directive location is not supported by the schema definition." spread.Name directive.Name, path)
                | None -> acc) acc

    let private checkDirectivesInOperation (acc : ValidationResult) (schemaInfo : SchemaInfo) (fragmentDefinitions : FragmentDefinition list) (path : Path) (operation : OperationDefinition) =
        let expectedDirectiveLocation =
            match operation.OperationType with
            | Query -> DirectiveLocation.QUERY
            | Mutation -> DirectiveLocation.MUTATION
            | Subscription -> DirectiveLocation.SUBSCRIPTION
        let acc =
            operation.Directives
            |> List.fold (fun acc directive ->
                match schemaInfo.Directives |> Array.tryFind (fun x -> x.Name = directive.Name) with
                | Some directiveDef ->
                    if directiveDef.Locations |> Array.contains expectedDirectiveLocation
                    then acc
                    else 
                        match operation.Name with
                        | Some operationName -> acc @ Error.AsResult(sprintf "%s operation '%s' has a directive '%s', but this directive location is not supported by the schema definition." (operation.OperationType.ToString()) operationName directive.Name, path)
                        | None -> acc @ Error.AsResult(sprintf "This %s operation has a directive '%s', but this directive location is not supported by the schema definition." (operation.OperationType.ToString()) directive.Name, path)
                | None -> acc) acc
        operation.SelectionSet |> List.fold (fun acc selection -> checkDirectivesInValidLocationOnSelection acc schemaInfo fragmentDefinitions path selection) acc

    let validateDirectivesAreInValidLocations (ctx : ValidationContext) =
        let fragmentDefinitions = ctx.FragmentDefinitions |> List.map (fun x -> x.Definition)
        ctx.Document.Definitions
        |> List.fold (fun acc def ->
            let path = match def.Name with | Some name -> [name] | None -> []
            match def with
            | OperationDefinition odef -> checkDirectivesInOperation acc ctx.Schema fragmentDefinitions path odef
            | FragmentDefinition frag -> checkDirectivesInFragment acc ctx.Schema fragmentDefinitions path (ref []) frag) Success

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

    let validateUniqueDirectivesPerLocation (ctx : ValidationContext) =
        ctx.Definitions
        |> List.collect (fun def ->
            let path = match def.Name with | Some name -> [name] | None -> []
            let defDirectives = path, def.Directives |> List.map (fun x -> x.Name)
            let selectionSetDirectives = def.Definition.SelectionSet |> List.collect (getDirectiveNamesInSelection path)
            defDirectives :: selectionSetDirectives)
        |> List.map (fun (path, directives) -> path, directives |> List.groupBy id |> List.map (fun (k, v) -> k, v.Length))
        |> List.fold (fun acc (path, directives) ->
            directives 
            |> List.filter (fun (_, length) -> length > 1)
            |> List.fold (fun acc (name, length) -> acc @ Error.AsResult(sprintf "Directive '%s' appears %i times in the location it is used. Directives must be unique in their locations." name length, path)) acc) Success

    let validateVariableUniqueness (ctx : ValidationContext) =
        ctx.Document.Definitions
        |> List.choose (function OperationDefinition def -> Some (def.Name, def.VariableDefinitions) | _ -> None)
        |> List.fold (fun acc (operationName, vars) ->
            vars
            |> List.groupBy id
            |> List.map (fun (k, v) -> k, v.Length)
            |> List.filter (fun (_, length) -> length > 1)
            |> List.fold (fun acc (var, length) ->
                match operationName with
                | Some operationName -> acc @ Error.AsResult(sprintf "Variable '%s' in operation '%s' is declared %i times. Variables must be unique in their operations." var.VariableName operationName length)
                | None -> acc @ Error.AsResult(sprintf "Variable '%s' is declared %i times in the operation. Variables must be unique in their operations." var.VariableName length)) acc) Success

    let validateVariablesAsInputTypes (ctx : ValidationContext) =
        ctx.Document.Definitions
        |> List.choose (function OperationDefinition def -> Some (def.Name, def.VariableDefinitions) | _ -> None)
        |> List.fold (fun acc (operationName, vars) ->
            vars |> List.fold (fun acc var ->
                match operationName, ctx.Schema.TryGetInputType(var.Type) with
                | Some operationName, None -> 
                    acc @ Error.AsResult(sprintf "Variable '%s' in operation '%s' has a type that is not an input type defined by the schema (%s)." var.VariableName operationName (var.Type.ToString()))
                | None, None ->
                    acc @ Error.AsResult(sprintf "Variable '%s' has a type is not an input type defined by the schema (%s)." var.VariableName (var.Type.ToString()))
                | _ -> acc) acc) Success

    let private checkVariablesDefinedInDirective (acc : ValidationResult) (variableDefinitions : VariableDefinition list) (path : Path) (directive : Directive) =
        directive.Arguments
        |> List.choose (fun arg -> match arg.Value with | Variable varName -> Some (arg.Name, varName) | _ -> None)
        |> List.fold (fun acc (argName, varName) -> 
            match variableDefinitions |> List.tryFind (fun x -> x.VariableName = varName) with
            | Some _ -> acc
            | None -> acc @ Error.AsResult(sprintf "A variable '%s' is referenced in an argument '%s' of directive '%s' of field with alias or name '%s', but that variable is not defined in the operation." varName argName directive.Name path.Head, path)) acc

    let rec private checkVariablesDefinedInSelection (acc : ValidationResult) (fragmentDefinitions : FragmentDefinition list) (visitedFragments : string list ref) (variableDefinitions : VariableDefinition list) (path : Path) =
        function
        | Field field -> 
            let path = field.AliasOrName :: path
            let acc = 
                field.Arguments 
                |> List.choose (fun arg -> match arg.Value with | Variable varName -> Some (arg.Name, varName) | _ -> None)
                |> List.fold (fun acc (argName, varName) ->
                    match variableDefinitions |> List.tryFind (fun x -> x.VariableName = varName) with
                    | Some _ -> acc
                    | None -> acc @ Error.AsResult(sprintf "A variable '%s' is referenced in argument '%s' of field with alias or name '%s', but that variable is not defined in the operation." varName argName field.AliasOrName)) acc
            let acc =
                field.SelectionSet 
                |> List.fold (fun acc selection -> checkVariablesDefinedInSelection acc fragmentDefinitions visitedFragments variableDefinitions path selection) acc
            field.Directives
            |> List.fold (fun acc directive -> checkVariablesDefinedInDirective acc variableDefinitions path directive) acc
        | InlineFragment frag -> 
            let acc =
                frag.SelectionSet
                |> List.fold (fun acc selection -> checkVariablesDefinedInSelection acc fragmentDefinitions visitedFragments variableDefinitions path selection) acc
            frag.Directives
            |> List.fold (fun acc directive -> checkVariablesDefinedInDirective acc variableDefinitions path directive) acc
        | FragmentSpread spread ->
            if List.contains spread.Name !visitedFragments
            then acc
            else
                let acc =
                    match fragmentDefinitions |> List.tryFind (fun x -> x.Name.IsSome && x.Name.Value = spread.Name) with
                    | Some frag -> 
                        let acc =
                            frag.SelectionSet 
                            |> List.fold (fun acc selection -> checkVariablesDefinedInSelection acc fragmentDefinitions visitedFragments variableDefinitions path selection) acc
                        frag.Directives
                        |> List.fold (fun acc directive -> checkVariablesDefinedInDirective acc variableDefinitions path directive) acc
                    | None -> acc
                spread.Directives
                |> List.fold (fun acc directive -> checkVariablesDefinedInDirective acc variableDefinitions path directive) acc

    let validateVariablesUsesDefined (ctx : ValidationContext) =
        let fragmentDefinitions = getFragmentDefinitions ctx.Document
        ctx.Document.Definitions
        |> List.choose (function OperationDefinition def -> Some (def.Name, def.SelectionSet, def.VariableDefinitions) | _ -> None)
        |> List.fold (fun acc (operationName, selectionSet, varDefs) ->
            let path = match operationName with | Some name -> [name] | None -> []
            selectionSet
            |> List.fold (fun acc selection -> checkVariablesDefinedInSelection acc fragmentDefinitions (ref []) varDefs path selection) acc) Success

    let rec private variableIsUsedInDirective (name : string) (directive : Directive) =
        let validArgNames = directive.Arguments |> List.choose (fun x -> match x.Value with | Variable varName -> Some varName | _ -> None)
        List.contains name validArgNames

    let rec private variableIsUsedInSelection (name : string) (fragmentDefinitions : FragmentDefinition list) (visitedFragments : string list ref) =
        function
        | Field field ->
            let validArgNames = field.Arguments |> List.choose (fun x -> match x.Value with | Variable varName -> Some varName | _ -> None)
            if List.contains name validArgNames
            then true
            else 
                let acc = field.SelectionSet |> List.fold (fun acc selection -> acc || variableIsUsedInSelection name fragmentDefinitions visitedFragments selection) false
                field.Directives |> List.fold (fun acc directive -> acc || variableIsUsedInDirective name directive) acc
        | InlineFragment frag ->
            let acc = frag.SelectionSet |> List.fold (fun acc selection -> acc || variableIsUsedInSelection name fragmentDefinitions visitedFragments selection) false
            frag.Directives |> List.fold (fun acc directive -> acc || variableIsUsedInDirective name directive) acc
        | FragmentSpread spread ->
            if List.contains spread.Name !visitedFragments
            then false
            else
                let acc =
                    match fragmentDefinitions |> List.tryFind (fun x -> x.Name.IsSome && x.Name.Value = spread.Name) with
                    | Some frag -> 
                        let acc = frag.SelectionSet |> List.fold (fun acc selection -> acc || variableIsUsedInSelection name fragmentDefinitions visitedFragments selection) false
                        frag.Directives |> List.fold (fun acc directive -> acc || variableIsUsedInDirective name directive) acc
                    | None -> false
                spread.Directives |> List.fold (fun acc directive -> acc || variableIsUsedInDirective name directive) acc

    let validateAllVariablesUsed (ctx : ValidationContext) =
        let fragmentDefinitions = getFragmentDefinitions ctx.Document
        ctx.Document.Definitions
        |> List.choose (function OperationDefinition def -> Some (def.Name, def.SelectionSet, def.VariableDefinitions) | _ -> None)
        |> List.fold (fun acc (operationName, selectionSet, varDefs) ->
            varDefs |> List.map (fun x -> x.VariableName) |> List.fold (fun acc varName ->
                let isUsed = selectionSet |> List.fold (fun acc selection -> acc || variableIsUsedInSelection varName fragmentDefinitions (ref []) selection) false
                match operationName, isUsed with
                | _ , true -> acc
                | Some operationName, _ -> acc @ Error.AsResult(sprintf "Variable definition '%s' is not used in operation '%s'. Every variable must be used." varName operationName)
                | None, _ -> acc @ Error.AsResult(sprintf "Variable definition '%s' is not used in operation. Every variable must be used." varName))
                acc) Success