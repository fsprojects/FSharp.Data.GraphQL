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
      MutationType : IntrospectionType option }
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
          SubscriptionType = schema.SubscriptionType |> Option.bind (SchemaInfo.TryGetSchemaTypeByRef schemaTypes) }
    member x.TryGetOperationType(ot : OperationType) =
        match ot with
        | Query -> x.QueryType
        | Mutation -> x.MutationType
        | Subscription -> x.SubscriptionType
    member x.TryGetTypeByName(name : string) =
        x.SchemaTypes.TryFind(name)

type SelectionInfo =
    { Alias : string option
      Name : string
      SelectionSet : SelectionInfo list
      Args : Argument list
      FieldType : IntrospectionTypeRef
      ParentType : IntrospectionType }
    member x.AliasOrName = x.Alias |> Option.defaultValue x.Name

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

    let rec private getSelectionSetInfo (schemaInfo : SchemaInfo) (fragmentDefinitions : FragmentDefinition list) (parentType : IntrospectionType) (selectionSet : Selection list) =
        let getFragSelectionInfo (frag : FragmentDefinition) =
            let fragType =
                match frag.TypeCondition with
                | Some typeCondition ->
                    match schemaInfo.TryGetTypeByName(typeCondition) with
                    | Some fragType -> fragType
                    | None -> failwithf "Failure reading schema. Can not find type '%s' specified by a fragment type condition." typeCondition
                | None -> parentType
            getSelectionSetInfo schemaInfo fragmentDefinitions fragType frag.SelectionSet
        selectionSet
        |> List.collect (function
            | Field field ->
                let fieldType = 
                    match parentType.Fields |> Option.map (Array.tryFind (fun f -> f.Name = field.Name)) |> Option.flatten with
                    | Some t -> t.Type
                    | None -> failwithf "Failure reading schema. Can not find field '%s' of type '%s'." field.Name parentType.Name
                [ { Alias = field.Alias
                    Name = field.Name
                    SelectionSet = getSelectionSetInfo schemaInfo fragmentDefinitions parentType field.SelectionSet
                    Args = field.Arguments
                    FieldType = fieldType
                    ParentType = parentType } ]
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
                    if fieldA.ParentType = fieldB.ParentType || fieldA.ParentType.Kind <> TypeKind.OBJECT || fieldB.ParentType.Kind <> TypeKind.OBJECT
                    then
                        if fieldA.Name <> fieldB.Name then acc @ Error [ sprintf "Field name or alias '%s' is referring to fields '%s' and '%s', but they are different fields in the scope of the parent type." aliasOrName fieldA.Name fieldB.Name ]
                        else if fieldA.Args <> fieldB.Args then acc @ Error [ sprintf "Field name or alias '%s' refers to field '%s' two times, but each reference has different argument sets." aliasOrName fieldA.Name ]
                        else
                            let mergedSet = fieldA.SelectionSet |> List.append fieldB.SelectionSet
                            acc @ (fieldsInSetCanMerge mergedSet)
                    else acc)
                |> List.reduce (@)) Success

    let validateFieldSelectionMerging (schemaInfo : SchemaInfo) (ast : Document) =
        let fragmentDefinitions = getFragmentDefinitions ast
        ast.Definitions
        |> List.choose (function 
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
                | None -> None)
        |> List.fold (fun acc (objectType, selectionSet) ->
            let set = getSelectionSetInfo schemaInfo fragmentDefinitions objectType selectionSet
            acc @ (fieldsInSetCanMerge set)) Success

