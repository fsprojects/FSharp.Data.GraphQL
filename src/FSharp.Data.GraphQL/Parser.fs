/// The MIT License (MIT)
/// Copyright (c) 2015-Mar 2016 Kevin Thompson @kthompson
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Parser

open System
open System.Globalization
open FParsec
open FSharp.Data.GraphQL.Ast

// Modeled after https://facebook.github.io/graphql/
let private someOrEmpty l =
    match l with
    | Some list -> list
    | None -> []
let private someOrEmptyString s =
    match s with
    | Some str -> str
    | None -> ""

let private BP (p: Parser<_,_>) stream =
    p stream // set a breakpoint here

let private (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let private pSelection, pSelectionRef = createParserForwardedToRef<_, unit>()
let private pField, pFieldRef = createParserForwardedToRef<_, unit>()
let private pSelectionSet, pSelectionSetRef = createParserForwardedToRef<Selection list, unit>()
let private pType, pTypeRef = createParserForwardedToRef<InputType, unit>()
let private pValue, pValueRef = createParserForwardedToRef<Value, unit>()

let private ``???`` = pzero

let private oneOf str =
    str
    |> Seq.map (fun x -> pstring (x.ToString()))
    |> choice        

let arrayOrEmptyArray optArray =
    match optArray with 
    | None -> Array.empty
    | Some (list : list<_>) -> list |> Array.ofList

// Section 2.1 Source text
let private SourceCharacter = anyChar

// 2.1.1 White Space
let private WhiteSpace =
    regex "[\u0009\u000B\u000C\u0020\u00A0]"
    <?> "WhiteSpace"

// 2.1.2 Line Terminators
let private lineTerminatorChars = "\u000A\u000D\u2028\u2029"
let private LineTerminator =    
    regex "[\u000A\u000D\u2028\u2029]"
    <?> "NewLine"

// 2.1.3 Comments
let private pCommentChar =
    //SourceCharacter but not LineTerminator        
    noneOf lineTerminatorChars

let private pComment =
    pchar '#' 
    >>. many pCommentChar 
    |>> fun x -> x |> string

// 2.1.4 Insignificant Commas
let private pComma = pstring ","


// 2.1.6 Ignored Tokens
let private pIgnored =
    many <| choice [
        WhiteSpace;
        LineTerminator;
        pComment;
        pComma;
    ] 
    <?> "Ignored"
    <!> "Ignored"

let private pIgnored1 =
    many1 <| choice [
        WhiteSpace;
        LineTerminator;
        pComment;
        pComma;
    ] 
    <?> "Ignored1"
    <!> "Ignored1"

let private sepByIgnored p = 
    many (p .>> pIgnored) .>> pIgnored

let private ws p = p .>> pIgnored
let private wsstr s = pstring s .>> pIgnored
//    // 2.1.7 Punctuators
//    let pPunctuator =
//        choice [
//            pstring "!"; pstring "$"; pstring "("; pstring ")";
//            pstring "..."; pstring ":"; pstring "="; pstring "@";
//            pstring "["; pstring "]"; pstring "{"; pstring "}";
//        ]     

// 2.1.8 Names
let private pName = 
    regex "[_A-Za-z][_0-9A-Za-z]*" 
    <?> "Name"
    <!> "Name"

// 2.2.7.1 Int Value
let private pDigit : Parser<string, string> = regex "[0-9]"
let private pDigits = regex "[0-9]*"
let private pIntPart =
    let negSign = pstring "-"
    let nonZeroDigit = anyOf "123456789" |>> fun x -> x.ToString()        

    let zero = opt negSign >>. pstring "0"
    let nonZero = 
        opt negSign .>>. nonZeroDigit .>>. pDigits
        |>> fun ((oneg, d1), drest) ->
            someOrEmptyString oneg + d1 + drest

    zero <|> nonZero

let private pIntValue = pIntPart |>> Int32.Parse

// 2.2.7.2 Float Value
let private pFloatValue = pfloat

// 2.2.7.3 Bool Value
let private pBooleanValue = 
    (wsstr "true" >>% true) 
    <|>
    (wsstr "false" >>% false)

// 2.2.7.4 String Value
let private pStringValue = 
    let escapeChar =
        let pEscapedUnicode = 
            regex "[0-9A-Fa-f]{4}"
            |>> fun x -> char(Int32.Parse(x, NumberStyles.HexNumber))

        let pEscapedCharacter = 
            anyOf "\"\\/bfnrt"
            |>> fun x -> x
        
        let encoded = 
            choice [
                pstring "u" >>. pEscapedUnicode
                pEscapedCharacter
            ]
        pstring "\\" >>. encoded

    let sourceChar = satisfy <| fun c -> 
            match c with 
            | '\"' | '\\' | '\u000A' | '\u000D' | '\u2028' | '\u2029' -> false
            | _ -> true

    let pStringCharacter = sourceChar <|> escapeChar        
    let pQuote = pchar '"'

    between pQuote pQuote (manyChars pStringCharacter)
    <?> "String"

// 2.2.8 Variables
let private pVariable =
    pstring "$" >>. ws pName 
    <?> "Variable"
    <!> "Variable"

let private pEnumValue = pName
let private pListValue = wsstr "[" >>. (sepByIgnored pValue) .>> wsstr "]"

let private pObjectValue = ``???``

// 2.2.7 Input Values
do pValueRef := 
    choice [
        pVariable       |>> Variable
        pIntValue       |>> IntValue
        pFloatValue     |>> FloatValue
        pStringValue    |>> StringValue
        pBooleanValue   |>> BooleanValue
        pEnumValue      |>> EnumValue
        pListValue      |>> ListValue
        pObjectValue    |>> ObjectValue
    ] <?> "Value"
        
// 2.2.5 Field Alias
let private pAlias =
    ws pName .>> wsstr ":"
    <?> "Alias"
    <!> "Alias"


// 2.2.4 Arguments
let private pArgument : Parser<Argument, _> = 
    ws pName .>> wsstr ":" .>>. pValue
    |>> fun (name, value) -> { Name = name; Value = value}
    <?> "Argument"

let private pArguments = wsstr "(" >>.  sepByIgnored pArgument .>> wsstr ")"


// 2.2.10 Directives
let private pDirective : Parser<Directive, _> =
    pstring "@" >>. pName .>>. opt pArguments
    |>> fun (name, args) -> { Name = name; Arguments = someOrEmpty args}

let private pDirectives = many pDirective

// 2.2.6 Fragments
let private pFragmentName = pName

let private pFragmentSpread : Parser<FragmentSpread, _> = 
    ws pFragmentName .>>. opt pDirectives
    <?> "FragmentSpread"
    |>> fun (name, directives) -> { Name = name; Directives = someOrEmpty directives }

// 2.2.9 Input Types    
let private pNamedType = pName |>> fun name -> NamedType(name)
let private pListType = between (pstring "[")  (pstring "]") !pTypeRef |>> ListType

let private pNonNullType = 
    choice [
        pNamedType .>> pstring "!"
        pListType .>> pstring "!" ]
    |>> fun name -> NonNullType(name)

do pTypeRef := 
    choice [
        pNamedType
        pListType
        pNonNullType
    ]

// 2.2.6.1 Type Conditions
let private pTypeCondition = 
    //pNamedType 
    pName //dont convert to variabletype right now

// 2.2.6.2 Inline Fragments
let private pInlineFragment : Parser<FragmentDefinition, _> =
    let func typeCond directives selSet = {
        Name = None
        TypeCondition = if String.IsNullOrEmpty typeCond then None else Some typeCond
        Directives = directives
        SelectionSet = selSet
    }

    pipe3 (wsstr "on" >>. ws pTypeCondition) pDirectives pSelectionSet func
    <?> "InlineFragment"

//    let pInlineFragment = 
//        pstring "..." >>. pInlineFragmentTail

let private pSelectionFragment =
    let fragmentTail = 
        (pInlineFragment |>> Selection.InlineFragment <!> "InlineFragment")            
        <|>
        (pFragmentSpread |>> Selection.FragmentSpread <!> "FragmentSpread")

    wsstr "..." >>. fragmentTail

// 2.2.2 Selection Sets    
do pSelectionRef := 
    choice [|
        (pField |>> Field) <!> "Field"
        pSelectionFragment
    |]
    <?> "Selection"
    <!> "Selection"

do pSelectionSetRef :=
    between (wsstr "{") (wsstr "}") (sepByIgnored pSelection)
    <?> "SelectionSet"
    <!> "SelectionSet"

// 2.2.3 Fields
do pFieldRef := 
    let func oalias name oargs directives oselection = 
        {
            Alias = oalias
            Name = name
            Arguments = someOrEmpty oargs
            Directives = directives 
            SelectionSet = someOrEmpty oselection
        }
    pipe5 (opt (attempt pAlias)) (ws pName) (opt pArguments) pDirectives (opt pSelectionSet) func
    <?> "Field"

// 2.2.8 Variables
let private pDefaultValue = wsstr "=" >>. pValue

let private pVariableDefinition: Parser<VariableDefinition, _> = 
    let func varName varType oDefVal =
        {
            VariableName = varName
            Type = varType
            DefaultValue = oDefVal
        }
    pipe3 (pVariable .>> wsstr ":") pType (opt pDefaultValue) func
    <?> "Variable Definition"
    

let private pVariableDefinitions = between (wsstr "(") (wsstr ")") (many pVariableDefinition)


// 2.2.1 Operations
let private pOperationType =
    (wsstr "query" >>% Query)
    <|> (wsstr "mutation" >>% Mutation)

let private pOperationDefinition1 : Parser<OperationDefinition, _> =
    pipe5 pOperationType (ws pName) (opt pVariableDefinitions) pDirectives pSelectionSet
    <| fun otype name ovars directives selection ->
        {
            OperationType = otype
            Name = Some name
            VariableDefinitions = someOrEmpty ovars 
            Directives = directives
            SelectionSet = selection
        }

let private pOperationDefinition2 : Parser<OperationDefinition, _> =
    pSelectionSet
    |>> fun x ->
        {
            OperationType = OperationType.Query
            Name = None
            VariableDefinitions = []
            Directives = []
            SelectionSet = x
        }    

let private pOperationDefinition = 
    pOperationDefinition1
    <|>
    pOperationDefinition2
    <?> "OperationDefinition"
    <!> "OperationDefinition"
        

// 2.2.6 Fragments
let private pFragmentDefinition: Parser<FragmentDefinition, _> = 
    pipe4 (wsstr "fragment" >>. ws pFragmentName .>> wsstr "on") (ws pTypeCondition) pDirectives pSelectionSet
    <| fun name typeCond directives selSet ->
            {
                Name = Some name
                TypeCondition = if String.IsNullOrEmpty typeCond then None else Some typeCond
                Directives = directives
                SelectionSet = selSet
            }
    <?> "FragmentDefinition"
    <!> "FragmentDefinition"
    

// 2.2 Query Document
let private pDefinition =
    (pOperationDefinition |>> OperationDefinition)
    <|> (pFragmentDefinition |>> FragmentDefinition)

let private pDocument = many pDefinition |>> fun x -> { Document.Definitions = x }    
let private entry = ws pDocument .>> eof

let parse query = 
  match run entry query with
  | Success(result, _, _) -> result
  | Failure(errorMsg, _, _) -> raise (System.FormatException(errorMsg))
  