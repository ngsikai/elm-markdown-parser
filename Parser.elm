module Parser exposing (..)

import String exposing (startsWith, dropLeft, split, lines, words, toList, length, trim, concat)


type BlockExpr
    = InlineBlock InlineExpr
    | BlockQuote (List BlockExpr)
    | UList (List (List BlockExpr))


type InlineExpr
    = Header Int String
    | Plain String


parseAll : String -> List BlockExpr
parseAll string =
    parseByContext (split "\n" string) [] [] ""


parseByContext : List String -> List BlockExpr -> List String -> String -> List BlockExpr
parseByContext lines blockExprs currBlock currContext =
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
                        parseByContext xs (appendBlockExpr blockExprs blockExpr) [] ""
                else if startsWith ">" trimmedLine then
                    if currContext == ">" then
                        parseByContext xs blockExprs (List.append currBlock [ trimmedLine ]) currContext
                    else
                        let
                            blockExpr =
                                buildBlock currBlock currContext
                        in
                            parseByContext xs (appendBlockExpr blockExprs blockExpr) [ trimmedLine ] ">"
                else if startsWith "* " x then
                    if currContext == "* " then
                        parseByContext xs blockExprs (List.append currBlock [ trimmedLine ]) currContext
                    else
                        let
                            blockExpr =
                                buildBlock currBlock currContext
                        in
                            parseByContext xs (appendBlockExpr blockExprs blockExpr) [ trimmedLine ] "* "
                else if currContext == "* " then
                    parseByContext xs blockExprs (stick currBlock (dropLeft 2 x)) currContext
                else
                    parseByContext xs blockExprs (stick currBlock trimmedLine) currContext


buildBlock : List String -> String -> Maybe BlockExpr
buildBlock lines context =
    if lines == [] then
        Nothing
    else if context == ">" then
        Just (BlockQuote (parseByContext (List.map (dropLeft 1) lines) [] [] ""))
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
                Header hashCount (dropLeft hashCount trimmedString)
        else
            Plain trimmedString


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
