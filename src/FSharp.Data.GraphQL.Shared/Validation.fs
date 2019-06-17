/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Validation

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types.Introspection

type ValidationResult =
    | Success
    | Error of string list

let (@) res1 res2 =
    match res1, res2 with
    | Success, Success -> Success
    | Success, _ -> res2
    | _, Success -> res1
    | Error e1, Error e2 -> Error (e1 @ e2)

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
        | NamedType name -> x.TryGetTypeByName(name) |> Option.map IntrospectionTypeRef.Named
        | ListType inner -> x.TryGetInputType(inner) |> Option.map IntrospectionTypeRef.List
        | NonNullType inner -> x.TryGetInputType(inner) |> Option.map IntrospectionTypeRef.NonNull

type SelectionInfo =
    { Alias : string option
      Name : string
      SelectionSet : SelectionInfo list
      Arguments : Argument list
      Directives : Directive list
      FieldType : IntrospectionTypeRef option
      ParentType : IntrospectionType
      FragmentType : IntrospectionType option
      InputValues : IntrospectionInputVal [] option }
    member x.AliasOrName = x.Alias |> Option.defaultValue x.Name
    member x.FragmentOrParentType = x.FragmentType |> Option.defaultValue x.ParentType

module Ast =
    let private getOperationDefinitions (ast : Document) =
        ast.Definitions |> List.choose (function | OperationDefinition x -> Some x | _ -> None)

    let private getFragmentDefinitions (ast : Document) =
        ast.Definitions |> List.choose (function | FragmentDefinition x -> Some x | _ -> None)

    let validateOperationNameUniqueness (ast : Document) =
        let names = getOperationDefinitions ast |> List.choose (fun x -> x.Name)
        names
        |> List.map (fun name -> name, names |> List.filter (fun x -> x = name) |> List.length)
        |> List.distinctBy fst
        |> List.filter (fun (_, count) -> count > 0)
        |> List.fold (fun acc (name, count) -> acc @ Error [ (sprintf "Operation '%s' has %i definitions. Each operation name must be unique." name count ) ]) Success

    let validateLoneAnonymousOperation (ast : Document) =
        let operations = getOperationDefinitions ast
        let unamed = operations |> List.filter (fun x -> x.Name.IsNone)
        if unamed.Length = 1 && operations.Length = 1
        then Success
        else Error [ "An anonymous operation must be the only operation in a document. This document has at least one anonymous operation and more than one operation." ]

    let validateSubscriptionSingleRootField (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
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
        let rec validate (acc : ValidationResult) (operationName : string option) (fieldNames : string list) =
            if fieldNames.Length = 1
            then acc
            else
                let rec fieldNamesToString (acc : string) (fieldNames : string list) =
                    match fieldNames with
                    | [] -> acc
                    | [last] -> acc + " " + last
                    | fieldName :: tail -> fieldNamesToString (acc + fieldName + ", ") tail
                let fieldNamesAsString = fieldNamesToString "" fieldNames
                match operationName with
                | Some operationName -> acc @ Error [ sprintf "Subscription operations should have only one root field. Operation '%s' has %i fields (%s)." operationName fieldNames.Length fieldNamesAsString ]
                | None -> acc @ Error [ sprintf "Subscription operations should have only one root field. Operation has %i fields (%s)." fieldNames.Length fieldNamesAsString ]
        getOperationDefinitions ast
        |> List.choose (fun def -> if def.OperationType = Subscription then Some (def.Name, getFieldNames [] def.SelectionSet) else None)
        |> List.fold (fun acc (name, fieldNames) -> validate acc name fieldNames) Success

    let validateSelectionFieldTypes (schemaInfo : SchemaInfo) (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        let rec validateFragmentDefinition (acc : ValidationResult) (frag : FragmentDefinition)  =
           match frag.TypeCondition with
           | Some typeCondition ->
               match schemaInfo.TryGetTypeByName typeCondition with
               | Some fragType -> checkFieldsOfType acc frag.SelectionSet fragType
               | None when frag.Name.IsSome -> acc @ Error [ sprintf "Fragment '%s' has type condition '%s', but that type does not exist in schema definition." frag.Name.Value frag.TypeCondition.Value ]
               | None -> acc @ Error [ sprintf "A fragment has type condition '%s', but that type does not exist in schema definition." typeCondition ]
           | None when frag.Name.IsSome -> acc @ Error [ sprintf "Fragment '%s' does not have a type condition." frag.Name.Value ]
           | None -> acc @ Error [ "Fragment does not have a type condition." ]
        and checkFieldsOfType (acc : ValidationResult) (selectionSet : Selection list) (objectType : IntrospectionType) =
            match selectionSet with
            | [] -> acc
            | selection :: tail ->
                let acc =
                    match selection with
                    | Field field ->
                       let exists = objectType.Fields |> Option.map (Array.exists (fun f -> f.Name = field.Name)) |> Option.defaultValue false
                       if not exists
                       then acc @ Error [ sprintf "Field '%s' is not defined in schema type '%s'." field.Name objectType.Name ]
                       else acc
                    | InlineFragment frag -> validateFragmentDefinition acc frag
                    | FragmentSpread spread ->
                       let frag = fragmentDefinitions |> List.tryFind (fun x -> x.Name.IsSome && x.Name.Value = spread.Name)
                       match frag with
                       | Some frag -> validateFragmentDefinition acc frag
                       | None -> acc @ Error [ sprintf "Fragment '%s' was not defined in the document." spread.Name ]
                checkFieldsOfType acc tail objectType
        ast.Definitions
        |> List.fold (fun acc def ->
            match def with
            | OperationDefinition odef ->
                let operationType = schemaInfo.TryGetOperationType(odef.OperationType)
                match operationType with
                | Some otype -> checkFieldsOfType acc odef.SelectionSet otype
                | None when odef.Name.IsNone -> acc @ Error [ "Could not find operation in the schema." ]
                | None -> acc @ Error [ sprintf "Could not find operation '%s' in the schema." odef.Name.Value ]
            | FragmentDefinition frag -> validateFragmentDefinition acc frag) Success

    let private typesAreApplicable (parentType : IntrospectionType, fragmentType : IntrospectionType) =
        let parentPossibleTypes = parentType.PossibleTypes |> Option.defaultValue [||] |> Array.choose (fun x -> x.Name) |> Array.append [|parentType.Name|] |> Set.ofArray
        let fragmentPossibleTypes = fragmentType.PossibleTypes |> Option.defaultValue [||] |> Array.choose (fun x -> x.Name) |> Array.append [|fragmentType.Name|] |> Set.ofArray
        let applicableTypes = Set.intersect parentPossibleTypes fragmentPossibleTypes
        applicableTypes.Count > 0

    let rec private getSelectionSetInfo (schemaInfo : SchemaInfo) (fragmentDefinitions : FragmentDefinition list) (parentType : IntrospectionType) (fragmentType : IntrospectionType option) (selectionSet : Selection list) =
        let getFragSelectionInfo (frag : FragmentDefinition) =
            let parentType =
                match fragmentType with
                | Some outerFragmentType -> outerFragmentType
                | None -> parentType
            let fragmentType = frag.TypeCondition |> Option.bind schemaInfo.TryGetTypeByName
            getSelectionSetInfo schemaInfo fragmentDefinitions parentType fragmentType frag.SelectionSet
        selectionSet
        |> List.collect (function
            | Field field ->
                let ifield = 
                    match parentType.Fields |> Option.bind (Array.tryFind (fun f -> f.Name = field.Name)), fragmentType with
                    | None, Some fragType -> fragType.Fields |> Option.bind (Array.tryFind (fun f -> f.Name = field.Name))
                    | x, _ -> x
                let inputValues = ifield |> Option.map (fun f -> f.Args)
                let fieldType = ifield |> Option.map (fun f -> f.Type)
                let selectionSet = getSelectionSetInfo schemaInfo fragmentDefinitions parentType fragmentType field.SelectionSet
                [ { Alias = field.Alias
                    Name = field.Name
                    SelectionSet = selectionSet
                    Arguments = field.Arguments
                    Directives = field.Directives
                    FieldType = fieldType
                    ParentType = parentType
                    FragmentType = fragmentType
                    InputValues = inputValues } ]
            | InlineFragment frag -> getFragSelectionInfo frag
            | FragmentSpread spread ->
                match fragmentDefinitions |> List.tryFind (fun f -> f.Name.IsSome && f.Name.Value = spread.Name) with
                | Some frag -> getFragSelectionInfo frag
                | None -> failwithf "Failure reading schema. Can not find fragment '%s'." spread.Name)

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
        else acc @ Error [ sprintf "Field name or alias '%s' appears two times, but they do not have the same return types in the scope of the parent type." fieldA.AliasOrName ]

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
                        if fieldA.Name <> fieldB.Name then acc @ Error [ sprintf "Field name or alias '%s' is referring to fields '%s' and '%s', but they are different fields in the scope of the parent type." aliasOrName fieldA.Name fieldB.Name ]
                        else if fieldA.Arguments <> fieldB.Arguments then acc @ Error [ sprintf "Field name or alias '%s' refers to field '%s' two times, but each reference has different argument sets." aliasOrName fieldA.Name ]
                        else
                            let mergedSet = fieldA.SelectionSet |> List.append fieldB.SelectionSet
                            acc @ (fieldsInSetCanMerge mergedSet)
                    else acc)
                |> List.reduce (@)) Success

    let private tryGetSelectionSetAndType (schemaInfo : SchemaInfo) =
        function
        | FragmentDefinition f ->
            match f.TypeCondition with
            | Some typeCondition ->
                match schemaInfo.TryGetTypeByName typeCondition with
                | Some ftype -> Some (ftype, f.SelectionSet)
                | None -> None
            | None -> None
        | OperationDefinition o ->
            match schemaInfo.TryGetOperationType(o.OperationType) with
            | Some otype -> Some (otype, o.SelectionSet)
            | None -> None

    let validateFieldSelectionMerging (schemaInfo : SchemaInfo) (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        ast.Definitions
        |> List.choose (tryGetSelectionSetAndType schemaInfo)
        |> List.fold (fun acc (objectType, selectionSet) ->
            let set = getSelectionSetInfo schemaInfo fragmentDefinitions objectType None selectionSet
            acc @ (fieldsInSetCanMerge set)) Success

    let rec private checkLeafFieldSelection (acc : ValidationResult) (selection : SelectionInfo) =
        let rec validateByKind (acc : ValidationResult) (fieldType : IntrospectionTypeRef) (selectionSetLength : int) =
            match fieldType.Kind with
            | TypeKind.NON_NULL | TypeKind.LIST when fieldType.OfType.IsSome -> 
                validateByKind acc fieldType.OfType.Value selectionSetLength
            | TypeKind.SCALAR | TypeKind.ENUM when selectionSetLength > 0 ->
                acc @ Error [ sprintf "Field '%s' of '%s' type is of type kind %s, and therefore should not contain inner fields in its selection." selection.Name selection.FragmentOrParentType.Name (fieldType.Kind.ToString()) ]
            | TypeKind.INTERFACE | TypeKind.UNION | TypeKind.OBJECT when selectionSetLength = 0 ->
                acc @ Error [ sprintf "Field '%s' of '%s' type is of type kind %s, and therefore should have inner fields in its selection." selection.Name selection.FragmentOrParentType.Name (fieldType.Kind.ToString()) ]
            | _ -> acc
        let acc =
            match selection.FieldType with
            | Some fieldType -> validateByKind acc fieldType selection.SelectionSet.Length
            | None -> acc
        selection.SelectionSet |> List.fold (fun acc selection -> checkLeafFieldSelection acc selection) acc

    let validateLeafFieldSelections (schemaInfo : SchemaInfo) (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        ast.Definitions
        |> List.choose (tryGetSelectionSetAndType schemaInfo)
        |> List.fold (fun acc (objectType, selectionSet) ->
            let set = getSelectionSetInfo schemaInfo fragmentDefinitions objectType None selectionSet
            set |> List.fold (fun acc selection -> checkLeafFieldSelection acc selection) acc) Success

    let rec private checkFieldArgumentNames (acc : ValidationResult) (schemaInfo : SchemaInfo) (selection : SelectionInfo) =
        let acc =
            selection.Arguments
            |> List.fold (fun acc arg ->
                match selection.InputValues |> Option.map (Array.tryFind (fun d -> d.Name = arg.Name)) |> Option.flatten with
                | Some _ -> acc
                | None -> acc @ Error [ sprintf "Field '%s' of type '%s' does not have an input named '%s' in its definition." selection.Name selection.FragmentOrParentType.Name arg.Name ]) acc
        selection.Directives
        |> List.fold (fun acc directive ->
            match schemaInfo.Directives |> Array.tryFind (fun d -> d.Name = directive.Name) with
            | Some directiveType ->
                directive.Arguments
                |> List.fold (fun acc arg ->
                    match directiveType.Args |> Array.tryFind (fun argt -> argt.Name = arg.Name) with
                    | Some _ -> acc
                    | None -> acc @ Error [ sprintf "Directive '%s' of field '%s' of type '%s' does not have an argument named '%s' in its definition." directiveType.Name selection.Name selection.FragmentOrParentType.Name arg.Name ]) acc
            | None -> acc) acc

    let validateArgumentNames (schemaInfo : SchemaInfo) (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        ast.Definitions
        |> List.choose (tryGetSelectionSetAndType schemaInfo)
        |> List.fold (fun acc (objectType, selectionSet) ->
            let set = getSelectionSetInfo schemaInfo fragmentDefinitions objectType None selectionSet
            set |> List.fold (fun acc selection -> checkFieldArgumentNames acc schemaInfo selection) acc) Success

    let rec private checkArgumentUniqueness (acc : ValidationResult) (fragmentDefinitions : FragmentDefinition list) =
        function
        | Field field -> 
            field.Arguments
            |> List.groupBy (fun x -> x.Name)
            |> List.map (fun (name, args) -> name, args.Length)
            |> List.fold (fun acc (name, length) ->
                if length > 1
                then acc @ Error [ sprintf "More than one argument named '%s' was defined in field '%s'. Field arguments must be unique." name field.Name ]
                else acc) acc
        | InlineFragment frag -> 
            match frag.SelectionSet |> List.map (checkArgumentUniqueness acc fragmentDefinitions) with
            | [] -> acc
            | results -> List.reduce (@) results
        | FragmentSpread spread ->
            match fragmentDefinitions |> List.tryFind (fun f -> f.Name.IsSome && f.Name.Value = spread.Name) with
            | Some frag -> 
                match frag.SelectionSet |> List.map (checkArgumentUniqueness acc fragmentDefinitions) with
                | [] -> acc
                | results -> List.reduce (@) results
            | None -> acc

    let validateArgumentUniqueness (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        ast.Definitions
        |> List.collect (function
            | FragmentDefinition frag -> frag.SelectionSet
            | OperationDefinition op -> op.SelectionSet)
        |> List.fold (fun acc def -> checkArgumentUniqueness acc fragmentDefinitions def) Success

    let rec private checkRequiredArguments (acc : ValidationResult) (schemaInfo : SchemaInfo) (selection : SelectionInfo) =
        let acc =
            selection.InputValues
            |> Option.map (Array.fold (fun acc argDef ->
                match argDef.Type.Kind with
                | TypeKind.NON_NULL when argDef.DefaultValue.IsNone ->
                    match selection.Arguments |> List.tryFind (fun arg -> arg.Name = argDef.Name) with
                    | Some arg when arg.Value <> EnumValue "null" -> acc // TODO: null values are being mapped into an enum value, this should be fixed in the parser! A null value must be able to be parsed.
                    | _ -> acc @ Error [ sprintf "Argument '%s' of field '%s' of type '%s' is required and does not have a default value." argDef.Name selection.Name selection.FragmentOrParentType.Name ]
                | _ -> acc) acc)
            |> Option.defaultValue acc
        selection.Directives
        |> List.fold (fun acc directive ->
            match schemaInfo.Directives |> Array.tryFind (fun d -> d.Name = directive.Name) with
            | Some directiveType ->
                directiveType.Args
                |> Array.fold (fun acc argDef ->
                    match argDef.Type.Kind with
                    | TypeKind.NON_NULL when argDef.DefaultValue.IsNone ->
                        match selection.Arguments |> List.tryFind (fun arg -> arg.Name = argDef.Name) with
                        | Some arg when arg.Value <> EnumValue "null" -> acc
                        | _ -> acc @ Error [ sprintf "Argument '%s' of directive '%s' of field '%s' of type '%s' is required and does not have a default value." argDef.Name directiveType.Name selection.Name selection.FragmentOrParentType.Name ]
                    | _ -> acc) acc
            | None -> acc) acc

    let validateRequiredArguments (schemaInfo : SchemaInfo) (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        ast.Definitions
        |> List.choose (tryGetSelectionSetAndType schemaInfo)
        |> List.fold (fun acc (objectType, selectionSet) ->
            let set = getSelectionSetInfo schemaInfo fragmentDefinitions objectType None selectionSet
            set |> List.fold (fun acc selection -> checkRequiredArguments acc schemaInfo selection) acc) Success

    let validateFragmentNameUniqueness (ast : Document) =   
        let fragmentDefinitions = getFragmentDefinitions ast
        fragmentDefinitions
        |> List.filter (fun f -> f.Name.IsSome)
        |> List.groupBy (fun f -> f.Name.Value)
        |> List.choose (fun (name, frags) -> if frags.Length > 1 then Some (name, frags.Length) else None)
        |> List.fold (fun acc (name, length) -> acc @ Error [ sprintf "There are %i fragments with name '%s' in the document. Fragment definitions must have unique names." length name ]) Success

    let rec private checkFragmentTypeExistence (acc : ValidationResult) (fragmentDefinitions : FragmentDefinition list) (schemaInfo : SchemaInfo) (frag : FragmentDefinition) =
        let acc =
            match frag.TypeCondition |> Option.map schemaInfo.TryGetTypeByName |> Option.flatten with
            | Some _ -> acc
            | None when frag.Name.IsSome -> acc @ Error [ sprintf "Fragment '%s' has type condition '%s', but that type does not exist in the schema." frag.Name.Value frag.TypeCondition.Value ]
            | None -> acc @ Error [ sprintf "An inline fragment in the document has type condition '%s', but that type does not exist in the schema." frag.TypeCondition.Value ]
        match frag.SelectionSet with
        | [] -> acc
        | selectionSet -> selectionSet |> List.map (checkFragmentTypeExistenceInSelection acc fragmentDefinitions schemaInfo) |> List.reduce (@)

    and private checkFragmentTypeExistenceInSelection (acc : ValidationResult) (fragmentDefinitions : FragmentDefinition list) (schemaInfo : SchemaInfo) =
        function
        | Field field -> 
            match field.SelectionSet |> List.map (checkFragmentTypeExistenceInSelection acc fragmentDefinitions schemaInfo) with
            | [] -> acc
            | results -> List.reduce (@) results
        | InlineFragment frag -> checkFragmentTypeExistence acc fragmentDefinitions schemaInfo frag
        | FragmentSpread spread ->
            match fragmentDefinitions |> List.tryFind(fun f -> f.Name.IsSome && f.Name.Value = spread.Name) with
            | Some frag -> checkFragmentTypeExistence acc fragmentDefinitions schemaInfo frag
            | None -> acc
       
    let validateFragmentTypeExistence (schemaInfo : SchemaInfo) (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        ast.Definitions
        |> List.fold (fun acc def ->
            match def with
            | FragmentDefinition frag -> checkFragmentTypeExistence acc fragmentDefinitions schemaInfo frag
            | OperationDefinition odef -> 
                match odef.SelectionSet |> List.map (checkFragmentTypeExistenceInSelection acc fragmentDefinitions schemaInfo) with
                | [] -> acc
                | results -> List.reduce (@) results) Success

    let rec private checkFragmentOnCompositeTypes (acc : ValidationResult) (fragmentDefinitions : FragmentDefinition list) (schemaInfo : SchemaInfo) (frag : FragmentDefinition) =
        let acc =
            match frag.TypeCondition |> Option.map schemaInfo.TryGetTypeByName |> Option.flatten with
            | Some fragType ->
                match fragType.Kind with
                | TypeKind.UNION | TypeKind.OBJECT | TypeKind.INTERFACE -> acc
                | _ when frag.Name.IsSome -> acc @ Error [ sprintf "Fragment '%s' has type kind %s, but fragments can only be defined in UNION, OBJECT or INTERFACE types." frag.Name.Value (fragType.Kind.ToString()) ]
                | _ -> acc @ Error [ sprintf "An inline fragment has type kind %s, but fragments can only be defined in UNION, OBJECT or INTERFACE types." (fragType.Kind.ToString()) ]
            | None -> acc
        match frag.SelectionSet |> List.map (checkSelectionFragmentsOnCompositeType acc fragmentDefinitions schemaInfo) with
        | [] -> acc
        | results -> List.reduce (@) results

    and private checkSelectionFragmentsOnCompositeType (acc : ValidationResult) (fragmentDefinitions : FragmentDefinition list) (schemaInfo : SchemaInfo) =
        function
        | Field field ->
            match field.SelectionSet |> List.map (checkSelectionFragmentsOnCompositeType acc fragmentDefinitions schemaInfo) with
            | [] -> acc
            | results -> List.reduce (@) results
        | InlineFragment frag -> checkFragmentOnCompositeTypes acc fragmentDefinitions schemaInfo frag
        | FragmentSpread spread ->
            match fragmentDefinitions |> List.tryFind(fun f -> f.Name.IsSome && f.Name.Value = spread.Name) with
            | Some frag -> checkFragmentOnCompositeTypes acc fragmentDefinitions schemaInfo frag
            | None -> acc

    let validateFragmentsOnCompositeTypes (schemaInfo : SchemaInfo) (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        ast.Definitions
        |> List.fold (fun acc def ->
            match def with
            | OperationDefinition odef -> 
                match odef.SelectionSet |> List.map (checkSelectionFragmentsOnCompositeType acc fragmentDefinitions schemaInfo) with
                | [] -> acc
                | results -> List.reduce (@) results
            | FragmentDefinition frag -> checkFragmentOnCompositeTypes acc fragmentDefinitions schemaInfo frag) Success

    let validateFragmentsMustBeUsed (ast : Document) =
        let rec getSpreadNames (acc : string list) =
            function
            | Field field -> field.SelectionSet |> List.collect (getSpreadNames acc)
            | InlineFragment frag -> frag.SelectionSet |> List.collect (getSpreadNames acc)
            | _ -> acc
        let fragmentSpreadNames =
            ast.Definitions
            |> List.collect (function
                | FragmentDefinition frag -> frag.SelectionSet |> List.collect (getSpreadNames [])
                | OperationDefinition odef -> odef.SelectionSet |> List.collect (getSpreadNames []))
            |> List.distinct
        getFragmentDefinitions ast
        |> List.fold (fun acc def ->
            if def.Name.IsSome && List.contains def.Name.Value fragmentSpreadNames
            then acc
            else acc @ Error [ sprintf "Fragment '%s' is not used in any operation in the document. Fragments must be used in at least one operation." def.Name.Value ]) Success

    let rec private fragmentSpreadTargetDefinedInSelection (acc : ValidationResult) (fragmentDefinitionNames : string list) =
        function
        | Field field -> 
            match field.SelectionSet |> List.map (fragmentSpreadTargetDefinedInSelection acc fragmentDefinitionNames) with
            | [] -> acc
            | results -> List.reduce (@) results
        | InlineFragment frag ->
            match frag.SelectionSet |> List.map (fragmentSpreadTargetDefinedInSelection acc fragmentDefinitionNames) with
            | [] -> acc
            | results -> List.reduce (@) results
        | FragmentSpread spread ->
            if List.contains spread.Name fragmentDefinitionNames
            then acc
            else acc @ Error [ sprintf "Fragment spread '%s' refers to a non-existent fragment definition in the document." spread.Name ]

    let validateFragmentSpreadTargetDefined (ast : Document) =
        let fragmentDefinitionNames = getFragmentDefinitions ast |> List.choose (fun def -> def.Name)
        ast.Definitions
        |> List.fold (fun acc def ->
            match def with
            | FragmentDefinition frag -> 
                match frag.SelectionSet |> List.map (fragmentSpreadTargetDefinedInSelection acc fragmentDefinitionNames) with
                | [] -> acc
                | results -> List.reduce (@) results
            | OperationDefinition odef ->
                match odef.SelectionSet |> List.map (fragmentSpreadTargetDefinedInSelection acc fragmentDefinitionNames) with
                | [] -> acc
                | results -> List.reduce (@) results) Success

    let rec private checkFragmentMustNotHaveCycles (acc : ValidationResult) (fragmentDefinitions : FragmentDefinition list) (visited : string list ref) (frag : FragmentDefinition) =
        frag.Name 
        |> Option.map (fun name ->
            visited := name :: !visited
            if List.contains name !visited
            then acc @ Error [ sprintf "Fragment '%s' is making a cyclic reference." name ]
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
            then acc @ Error [ sprintf "Fragment '%s' is making a cyclic reference." spread.Name ]
            else
                match fragmentDefinitions |> List.tryFind (fun f -> f.Name.IsSome && f.Name.Value = spread.Name) with
                | Some frag -> checkFragmentMustNotHaveCycles acc fragmentDefinitions visited frag
                | None -> acc

    let validateFragmentsMustNotFormCycles (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        fragmentDefinitions |> List.fold (fun acc def -> checkFragmentMustNotHaveCycles acc fragmentDefinitions (ref []) def) Success

    let private checkFragmentSpreadIsPossibleInSelection (acc : ValidationResult) (parentType : IntrospectionType, fragmentType : IntrospectionType) =
        if not (typesAreApplicable (parentType, fragmentType))
        then acc @ Error [ sprintf "Fragment type condition '%s' is not applicable to the parent type of the field '%s'." fragmentType.Name parentType.Name ]
        else acc

    let rec private getFragmentAndParentTypes (acc : (IntrospectionType * IntrospectionType) list) (set : SelectionInfo list) =
        match set with
        | [] -> acc
        | selection :: tail ->
            let acc =
                match selection.FragmentType with
                | Some fragType when fragType.Name <> selection.ParentType.Name -> (selection.ParentType, fragType) :: acc
                | _ -> acc
            getFragmentAndParentTypes acc tail

    let validateFragmentSpreadIsPossible (schemaInfo : SchemaInfo) (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        ast.Definitions
        |> List.choose (tryGetSelectionSetAndType schemaInfo)
        |> List.fold (fun acc (objectType, selectionSet) ->
            getSelectionSetInfo schemaInfo fragmentDefinitions objectType None selectionSet
            |> getFragmentAndParentTypes []
            |> List.fold (fun acc types -> checkFragmentSpreadIsPossibleInSelection acc types) acc) Success

    let checkInputValue (acc : ValidationResult) (schemaInfo : SchemaInfo) (variables : VariableDefinition list option) (selection : SelectionInfo) =
        let rec getFieldMap (acc : Map<string, IntrospectionTypeRef>) (fields : (string * IntrospectionTypeRef) list) : Map<string, IntrospectionTypeRef> =
            match fields with
            | [] -> acc
            | (name, tref) :: fields -> getFieldMap (acc.Add(name, tref)) fields
        let rec checkIsCoercible (tref : IntrospectionTypeRef) (argName : string) (value : Value) =
            let canNotCoerce = Error [ sprintf "Argument field or value named '%s' can not be coerced. It does not match a valid literal representation for the type." argName ]
            match value with
            // TODO: null values are being parsed as an Enum. Isn't it better to make an option for null values?
            | EnumValue "null" when tref.Kind = TypeKind.NON_NULL -> Error [ sprintf "Argument '%s' value can not be coerced. It's type is non-nullable but the argument has a null value." argName ]
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
                                then acc @ Error [ sprintf "Can not coerce argument '%s'. Type '%s' have a required field '%s', but that field does not exist in the argument." argName tref.Name.Value kvp.Key ]
                                else acc) acc
                        props |> Seq.fold (fun acc kvp ->
                            match fieldMap.TryFind(kvp.Key) with
                            | Some fieldTypeRef -> checkIsCoercible fieldTypeRef kvp.Key kvp.Value
                            | None -> acc @ Error []) acc
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
        selection.Arguments
        |> List.fold (fun acc arg ->
            let argumentTypeRef = selection.InputValues |> Option.defaultValue [||] |> Array.tryFind (fun x -> x.Name = arg.Name) |> Option.map (fun x -> x.Type)
            match argumentTypeRef with
            | Some argumentTypeRef -> acc @ (checkIsCoercible argumentTypeRef arg.Name arg.Value)
            | None -> acc @ Error [ sprintf "Can not coerce argument '%s'. The argument can not be infered from the schema." arg.Name ]) acc

    let validateInputValues (schemaInfo : SchemaInfo) (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        ast.Definitions
        |> List.choose (fun def ->
            let selectionSetAndType = tryGetSelectionSetAndType schemaInfo def
            match def, selectionSetAndType with
            | FragmentDefinition _, Some selectionSetAndType -> Some (None, selectionSetAndType)
            | OperationDefinition odef, Some selectionSetAndType -> Some (Some odef.VariableDefinitions, selectionSetAndType)
            | _ -> None)
        |> List.fold (fun acc (vars, (objectType, selectionSet)) ->
            getSelectionSetInfo schemaInfo fragmentDefinitions objectType None selectionSet
            |> List.fold (fun acc selection -> checkInputValue acc schemaInfo vars selection) acc) Success