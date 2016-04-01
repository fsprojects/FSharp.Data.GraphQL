# FSharp.Data.GraphQL

## WARNING: Work in progress
F# implementation of Facebook [GraphQL query language specification](https://facebook.github.io/graphql).

## Implementation progress

1. Introduction
2. [x] Language

    2.1. [x] Source Text

    - 2.1.1. [x] Unicode
    - 2.1.2. [x] White Space
    - 2.1.3. [x] Line Terminators
    - 2.1.4. [x] Comments
    - 2.1.5. [x] Insignificant Commas
    - 2.1.6. [x] Lexical Tokens
    - 2.1.7. [x] Ignored Tokens
    - 2.1.8. [x] Punctuators
    - 2.1.9. [x] Names

    2.2. [x] Query Document

    - 2.2.1. [x] Operations
    - 2.2.2. [x] Selection Sets
    - 2.2.3. [x] Fields
    - 2.2.4. [x] Arguments
    - 2.2.5. [x] Field Alias
    - 2.2.6. [x] Fragments
        - 2.2.6.1. [x] Type Conditions
        - 2.2.6.2. [x] Inline Fragments
    - 2.2.7. [x] Input Values
        - 2.2.7.1. [x] Int Value
        - 2.2.7.2. [x] Float Value
        - 2.2.7.3. [x] Boolean Value
        - 2.2.7.4. [x] String Value
        - 2.2.7.5. [x] Enum Value
        - 2.2.7.6. [x] List Value
        - 2.2.7.7. [ ] Input Object Values
    - 2.2.8. [x] Variables
        - 2.2.8.1. [x] Variable use within Fragments
    - 2.2.9. [x] Input Types
    - 2.2.10. [x] Directives
        - 2.2.10.1. [x] Fragment Directives

3. [ ] Type System

    3.1. [x] Types
    - 3.1.1. [x] Scalars
        - 3.1.1.1. [x] Built-in Scalars
            - 3.1.1.1.1. [x] Int
            - 3.1.1.1.2. [x] Float
            - 3.1.1.1.3. [x] String
            - 3.1.1.1.4. [x] Boolean
            - 3.1.1.1.5. [x] ID
    - 3.1.2. [x] Objects
        - 3.1.2.1. [x] Object Field Arguments
        - 3.1.2.2. [ ] Object Field deprecation
        - 3.1.2.3. [ ] Object type validation
    - 3.1.3. [x] Interfaces
        - 3.1.3.1. [ ] Interface type validation
    - 3.1.4. [x] Unions
        - 3.1.4.1. [ ] Union type validation
    - 3.1.5. [x] Enums
    - 3.1.6. [ ] Input Objects
    - 3.1.7. [x] Lists
    - 3.1.8. [x] Non-Null

    3.2. [x] Directives
    - 3.2.1. [x] @skip
    - 3.2.2. [x] @include
    
    3.3. [ ] Starting types

4. [ ] Introspection

    4.1. [ ] General Principles
    - 4.1.1. [ ] Naming conventions
    - 4.1.2. [ ] Documentation
    - 4.1.3. [ ] Deprecation
    - 4.1.4. [ ] Type Name Introspection

    4.2. [x] Schema Introspection
    - 4.2.1. [x] The "__Type" Type
    - 4.2.2. [x] Type Kinds
        - 4.2.2.1. [x] Scalar
        - 4.2.2.2. [x] Object
        - 4.2.2.3. [x] Union
        - 4.2.2.4. [x] Interface
        - 4.2.2.5. [x] Enum
        - 4.2.2.6. [x] Input Object
        - 4.2.2.7. [x] List
        - 4.2.2.8. [x] Non-null
        - 4.2.2.9. [x] Combining List and Non-Null
    - 4.2.3. [x] The \_\_Field Type
    - 4.2.4. [x] The \_\_InputValue Type

5. [ ] Validation

    5.1. [ ] Operations
    - 5.1.1. [ ] Named Operation Definitions
        - 5.1.1.1. [ ] Operation Name Uniqueness
    - 5.1.2. [ ] Anonymous Operation Definitions
        - 5.1.2.1. [ ] Lone Anonymous Operation

    5.2. [ ] Fields
    - 5.2.1. [ ] Field Selections on Objects, Interfaces, and Unions Types
    - 5.2.2. [ ] Field Selection Merging
    - 5.2.3. [ ] Leaf Field Selections

    5.3. [ ] Arguments
    - 5.3.1. [ ] Argument Names
    - 5.3.2. [ ] Argument Uniqueness
    - 5.3.3. [ ] Argument Values Type Correctness
        - 5.3.3.1. [ ] Compatible Values
        - 5.3.3.2. [ ] Required Arguments

    5.4. [ ] Fragments
    - 5.4.1. [ ] Fragment Declarations
        - 5.4.1.1. [ ] Fragment Name Uniqueness
        - 5.4.1.2. [ ] Fragment Spread Type Existence
        - 5.4.1.3. [ ] Fragments On Composite Types
        - 5.4.1.4. [ ] Fragments Must Be Used
    - 5.4.2. [ ] Fragment Spreads
        - 5.4.2.1. [ ] Fragment spread target defined
        - 5.4.2.2. [ ] Fragment spreads must not form cycles
        - 5.4.2.3. [ ] Fragment spread is possible
            - 5.4.2.3.1. [ ] Object Spreads In Object Scope
            - 5.4.2.3.2. [ ] Abstract Spreads in Object Scope
            - 5.4.2.3.3. [ ] Object Spreads In Abstract Scope
            - 5.4.2.3.4. [ ] Abstract Spreads in Abstract Scope

    5.5. [ ] Values
    - 5.5.1. [ ] Input Object Field Uniqueness

    5.6. [ ] Directives
    - 5.6.1. [ ] Directives Are Defined

    5.7. [ ] Variables
    - 5.7.1. [ ] Variable Uniqueness
    - 5.7.2. [ ] Variable Default Values Are Correctly Typed
    - 5.7.3. [ ] Variables Are Input Types
    - 5.7.4. [ ] All Variable Uses Defined
    - 5.7.5. [ ] All Variables Used
    - 5.7.6. [ ] All Variable Usages are Allowed

6. [ ] Execution

    6.1. [ ] Evaluating requests

    6.2. [ ] Coercing Variables

    6.3. [ ] Evaluating operations

    6.4. [ ] Evaluating selection sets

    6.5. [ ] Evaluating a grouped field set
    - 6.5.1. [ ] Field entries
    - 6.5.2. [ ] Normal evaluation
    - 6.5.3. [ ] Serial execution
    - 6.5.4. [ ] Error handling
    - 6.5.5. [ ] Nullability

7. [ ] Response

    7.1. [ ] Serialization Format
    - 7.1.1. [ ] JSON Serialization

    7.2. [ ] Response Format
    - 7.2.1. [ ] Data
    - 7.2.2. [ ] Errors
