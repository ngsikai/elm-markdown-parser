module Parser exposing (..)

import String exposing (startsWith, dropLeft, split, lines, words, toList, length, trim, concat, indexes, slice)


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
    | Code String
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
                Header hashCount (parseHtmlText (dropLeft hashCount trimmedString))
        else
            Paragraph (parseHtmlText trimmedString)


parseHtmlText : String -> List HtmlText
parseHtmlText string =
    let
        codeFormatted =
            symbolParser "```" string
    in
        let
            boldParser =
                symbolParser "**"
        in
            let
                boldFormatted =
                    List.concat (List.map (\htmlText -> nextParse boldParser htmlText) codeFormatted)
            in
                let
                    italicParser =
                        symbolParser "*"
                in
                    List.concat (List.map (\htmlText -> nextParse italicParser htmlText) boldFormatted)


nextParse : (String -> List HtmlText) -> HtmlText -> List HtmlText
nextParse parser htmlText =
    case htmlText of
        Unformatted contents ->
            parser contents

        Bold contents ->
            [ Bold (List.concat (List.map (nextParse parser) contents)) ]

        Italics contents ->
            [ Italics (List.concat (List.map (nextParse parser) contents)) ]

        Code contents ->
            [ Code contents ]


symbolParser : String -> String -> List HtmlText
symbolParser symbol string =
    let
        symbolIndexes =
            getFirstTwoIndexes string symbol
    in
        let
            symbolLength =
                length symbol
        in
            case symbolIndexes of
                Just ( a, b ) ->
                    let
                        front =
                            Unformatted (slice 0 a string)
                    in
                        let
                            back =
                                parseHtmlText (dropLeft (b + symbolLength) string)
                        in
                            let
                                middleString =
                                    slice (a + symbolLength) b string
                            in
                                let
                                    middle =
                                        if symbol == "**" then
                                            Bold (parseHtmlText middleString)
                                        else if symbol == "*" then
                                            Italics (parseHtmlText middleString)
                                        else
                                            Code middleString
                                in
                                    front :: middle :: back

                Nothing ->
                    [ Unformatted string ]


getFirstTwoIndexes : String -> String -> Maybe ( Int, Int )
getFirstTwoIndexes string symbol =
    let
        symbolIndexes =
            indexes symbol string
    in
        if List.length symbolIndexes >= 2 then
            let
                first =
                    getIndex 0 symbolIndexes
            in
                let
                    second =
                        getIndex 1 symbolIndexes
                in
                    case ( first, second ) of
                        ( Just a, Just b ) ->
                            Just ( a, b )

                        ( _, _ ) ->
                            Nothing
        else
            Nothing


getIndex : Int -> List Int -> Maybe Int
getIndex n xs =
    if n == 0 then
        List.head xs
    else
        case List.tail xs of
            Just xs ->
                getIndex (n - 1) xs

            Nothing ->
                Nothing


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
