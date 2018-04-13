module Parser exposing (..)

import String exposing (startsWith, dropLeft, split, lines, words, toList, length, trim, concat, indexes, slice, join, fromChar)
import Combine exposing (parse, regex, manyTill, (*>), string, Parser, map, sequence, many, choice, lookAhead, (<|>))
import Combine.Char exposing (anyChar)


type BlockExpr
    = InlineBlock InlineExpr
    | BlockQuote (List BlockExpr)
    | UList (List (List BlockExpr))


type InlineExpr
    = Header Int (List HtmlText)
    | Paragraph (List HtmlText)


type HtmlText
    = Bold (List HtmlText)
    | Italics (List HtmlText)
    | Unformatted String


parseAll : String -> List BlockExpr
parseAll string =
    parseBlock (split "\n" string) [] [] ""


parseBlock : List String -> List BlockExpr -> List String -> String -> List BlockExpr
parseBlock lines blockExprs currLines currContext =
    let
        blockExpr =
            buildBlockExpr currLines currContext
    in
        case lines of
            [] ->
                appendBlockExpr blockExprs blockExpr

            x :: xs ->
                let
                    trimmedLine =
                        trim x
                in
                    if length trimmedLine == 0 then
                        parseBlock xs (appendBlockExpr blockExprs blockExpr) [] ""
                    else if startsWith ">" trimmedLine then
                        if currContext == ">" then
                            parseBlock xs blockExprs (List.append currLines [ trimmedLine ]) currContext
                        else
                            parseBlock xs (appendBlockExpr blockExprs blockExpr) [ trimmedLine ] ">"
                    else if startsWith "* " x then
                        if currContext == "* " then
                            parseBlock xs blockExprs (List.append currLines [ trimmedLine ]) currContext
                        else
                            parseBlock xs (appendBlockExpr blockExprs blockExpr) [ trimmedLine ] "* "
                    else if startsWith "  " x && currContext == "* " then
                        parseBlock xs blockExprs (stick currLines (dropLeft 2 x)) currContext
                    else
                        parseBlock xs blockExprs (stick currLines trimmedLine) currContext


buildBlockExpr : List String -> String -> Maybe BlockExpr
buildBlockExpr lines context =
    if lines == [] then
        Nothing
    else if context == ">" then
        Just (BlockQuote (parseBlock (List.map (dropLeft 1) lines) [] [] ""))
    else if context == "* " then
        Just (UList (List.map (\line -> parseAll (dropLeft 2 line)) lines))
    else
        Just (InlineBlock (parseInline (concat lines)))


appendBlockExpr : List BlockExpr -> Maybe BlockExpr -> List BlockExpr
appendBlockExpr blocks block =
    case block of
        Just block ->
            List.append blocks [ block ]

        Nothing ->
            blocks


stick : List String -> String -> List String
stick lines line =
    case lines of
        [] ->
            [ line ]

        x :: [] ->
            [ x ++ "\n" ++ line ]

        x :: xs ->
            x :: (stick xs line)


parseInline : String -> InlineExpr
parseInline string =
    let
        trimmedString =
            trim string
    in
        if isValidHeader trimmedString then
            let
                hashCount =
                    getHashCount trimmedString
            in
                Header hashCount (parseHtmlText (dropLeft (hashCount + 1) trimmedString))
        else
            Paragraph (parseHtmlText trimmedString)


parseHtmlText : String -> List HtmlText
parseHtmlText string =
    let
        parsers =
            (choice (List.map createSymbolParser [ "__", "*" ])) <|> unformattedParser
    in
        case parse parsers string of
            Ok ( _, stream, result ) ->
                let
                    parsedString =
                        getSecond result
                in
                    if parsedString == "" then
                        Unformatted (slice 0 1 string) :: parseHtmlText (slice 1 (length string) string)
                    else if getFirst result == "__" then
                        Bold (parseHtmlText parsedString) :: parseHtmlText stream.input
                    else if getFirst result == "*" then
                        Italics (parseHtmlText parsedString) :: parseHtmlText stream.input
                    else
                        Unformatted parsedString :: parseHtmlText stream.input

            Err ( _, stream, errors ) ->
                [ Unformatted string ]


getFirst : List String -> String
getFirst lst =
    case List.head lst of
        Just hd ->
            hd

        Nothing ->
            ""


getSecond : List String -> String
getSecond lst =
    case List.tail lst of
        Just tl ->
            getFirst tl

        Nothing ->
            ""


createSymbolParser : String -> Parser s (List String)
createSymbolParser symbol =
    sequence
        [ string symbol
        , (manyTill anyChar (string symbol)
            |> map (\xs -> join "" (List.map (fromChar) xs))
          )
        ]


unformattedParser : Parser s (List String)
unformattedParser =
    manyTill anyChar (lookAhead ((string "__") <|> (string "*")))
        |> map (\xs -> [ "", join "" (List.map (fromChar) xs) ])


isValidHeader : String -> Bool
isValidHeader string =
    case List.head (words string) of
        Just hd ->
            isAllHash hd

        Nothing ->
            False


getHashCount : String -> Int
getHashCount string =
    case List.head (words string) of
        Just hd ->
            length hd

        Nothing ->
            0


isAllHash : String -> Bool
isAllHash string =
    List.foldr (\ele acc -> ele == '#' && acc) True (toList string)
