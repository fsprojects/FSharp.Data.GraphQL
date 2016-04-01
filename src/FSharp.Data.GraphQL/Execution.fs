/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Execution

open System
open FSharp.Data.GraphQL.Types

/// 6.4 Evaluating selection sets
let collectFields objectType selectionSet visitedFragments = ()

let doesFragmentTypeApply objectType fragmentType = ()

/// 6.5.1 Field entries
let getFieldEntry objectType objectInstance fields = ()
let getFieldTypeFromObjectType objectType firstField = ()
let resolveFieldOnObject objectType objectInstance firstField = ()
let mergeSelectionSets fields = ()
let completeValue fieldType result subSelectionSet = ()