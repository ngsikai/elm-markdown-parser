module Parser exposing (..)

import String exposing (startsWith, dropLeft, split, lines, words, toList, length, trim, concat, indexes, slice, join, fromChar)
import Combine exposing (parse, regex, manyTill, (*>), string, Parser, map, sequence, many)
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
parseBlock lines blockExprs currBlock currContext =
    case lines of
        [] ->
            let
                blockExpr =
                    buildBlock currBlock currContext
            in
                appendBlockExpr blockExprs blockExpr

        x :: xs ->
            let
                trimmedLine =
                    trim x
            in
                if length trimmedLine == 0 then
                    let
                        blockExpr =
                            buildBlock currBlock currContext
                    in
                        parseBlock xs (appendBlockExpr blockExprs blockExpr) [] ""
                else if startsWith ">" trimmedLine then
                    if currContext == ">" then
                        parseBlock xs blockExprs (List.append currBlock [ trimmedLine ]) currContext
                    else
                        let
                            blockExpr =
                                buildBlock currBlock currContext
                        in
                            parseBlock xs (appendBlockExpr blockExprs blockExpr) [ trimmedLine ] ">"
                else if startsWith "* " x then
                    if currContext == "* " then
                        parseBlock xs blockExprs (List.append currBlock [ trimmedLine ]) currContext
                    else
                        let
                            blockExpr =
                                buildBlock currBlock currContext
                        in
                            parseBlock xs (appendBlockExpr blockExprs blockExpr) [ trimmedLine ] "* "
                else if currContext == "* " then
                    parseBlock xs blockExprs (stick currBlock (dropLeft 2 x)) currContext
                else
                    parseBlock xs blockExprs (stick currBlock trimmedLine) currContext


buildBlock : List String -> String -> Maybe BlockExpr
buildBlock lines context =
    if lines == [] then
        Nothing
    else if context == ">" then
        Just (BlockQuote (parseBlock (List.map (dropLeft 1) lines) [] [] ""))
    else if context == "* " then
        let
            listItems =
                List.map (\line -> parseAll (dropLeft 2 line)) lines
        in
            Just (UList listItems)
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
stick block line =
    case block of
        x :: [] ->
            [ x ++ "\n" ++ line ]

        x :: xs ->
            x :: (stick xs line)

        [] ->
            [ line ]


parseInline : String -> InlineExpr
parseInline string =
    let
        trimmedString =
            trim string
    in
        if isValidHeader trimmedString then
            let
                hashCount =
                    case List.head (words trimmedString) of
                        Just hd ->
                            length hd

                        Nothing ->
                            0
            in
                Header hashCount (parseHtmlText (dropLeft (hashCount + 1) trimmedString))
        else
            Paragraph (parseHtmlText trimmedString)


parseHtmlText : String -> List HtmlText
parseHtmlText string =
    let
        parsers =
            Combine.choice (List.map createSymbolParser [ "__", "*" ])
    in
        case parse parsers string of
            Ok ( _, stream, result ) ->
                if getSymbol result == "__" then
                    Bold (parseHtmlText (getMsg result)) :: parseHtmlText stream.input
                else
                    Italics (parseHtmlText (getMsg result)) :: parseHtmlText stream.input

            Err ( _, stream, errors ) ->
                [ Unformatted string ]


getSymbol : List String -> String
getSymbol lst =
    case List.head lst of
        Just hd ->
            hd

        Nothing ->
            ""


getMsg : List String -> String
getMsg lst =
    case List.tail lst of
        Just tl ->
            getSymbol tl

        Nothing ->
            ""



-- enclosing : String -> Parser String String
-- parseBetween symbol string =
--     let
--         symbolParser =
--             createSymbolParser symbol
--     in
--         case parse symbolParser string of
--             Ok ( _, stream, result ) ->
--                 Ok result
--             Err ( _, stream, errors ) ->
--                 Err (join " or " errors)


createSymbolParser : String -> Parser s (List String)
createSymbolParser symbol =
    sequence
        [ string symbol
        , (manyTill anyChar (string symbol)
            |> map (\xs -> join "" (List.map (fromChar) xs))
          )
        ]



-- parseHtmlText : String -> List HtmlText
-- parseHtmlText string =
--     let
--         boldFormatted =
--             symbolParser "__" string
--     in
--         let
--             italicParser =
--                 symbolParser "*"
--         in
--             List.concat (List.map (\htmlText -> nextParse italicParser htmlText) boldFormatted)
-- nextParse : (String -> List HtmlText) -> HtmlText -> List HtmlText
-- nextParse parser htmlText =
--     case htmlText of
--         Unformatted contents ->
--             parser contents
--         Bold contents ->
--             [ Bold (List.concat (List.map (nextParse parser) contents)) ]
--         Italics contents ->
--             [ Italics (List.concat (List.map (nextParse parser) contents)) ]
-- symbolParser : String -> String -> List HtmlText
-- symbolParser symbol string =
--     let
--         symbolIndexes =
--             getFirstTwoIndexes string symbol
--     in
--         let
--             symbolLength =
--                 length symbol
--         in
--             case symbolIndexes of
--                 Just ( a, b ) ->
--                     let
--                         front =
--                             Unformatted (slice 0 a string)
--                     in
--                         let
--                             back =
--                                 parseHtmlText (dropLeft (b + symbolLength) string)
--                         in
--                             let
--                                 middleString =
--                                     slice (a + symbolLength) b string
--                             in
--                                 let
--                                     middle =
--                                         if symbol == "__" then
--                                             Bold (parseHtmlText middleString)
--                                         else
--                                             Italics (parseHtmlText middleString)
--                                 in
--                                     front :: middle :: back
--                 Nothing ->
--                     [ Unformatted string ]
-- getFirstTwoIndexes : String -> String -> Maybe ( Int, Int )
-- getFirstTwoIndexes string symbol =
--     let
--         symbolIndexes =
--             indexes symbol string
--     in
--         if List.length symbolIndexes >= 2 then
--             let
--                 first =
--                     getIndex 0 symbolIndexes
--             in
--                 let
--                     second =
--                         getIndex 1 symbolIndexes
--                 in
--                     case ( first, second ) of
--                         ( Just a, Just b ) ->
--                             Just ( a, b )
--                         ( _, _ ) ->
--                             Nothing
--         else
--             Nothing
-- getIndex : Int -> List Int -> Maybe Int
-- getIndex n xs =
--     if n == 0 then
--         List.head xs
--     else
--         case List.tail xs of
--             Just xs ->
--                 getIndex (n - 1) xs
--             Nothing ->
--                 Nothing


isValidHeader : String -> Bool
isValidHeader string =
    case (List.head (words string)) of
        Just hd ->
            isAllHash hd

        Nothing ->
            False


isAllHash : String -> Bool
isAllHash string =
    List.foldr (\ele acc -> ele == '#' && acc) True (toList string)
