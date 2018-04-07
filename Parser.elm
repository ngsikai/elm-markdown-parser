module Parser exposing (..)

import String exposing (startsWith, dropLeft, split, lines, words, toList, length, trim)


type BlockExpr
    = InlineBlock InlineExpr
    | BlockQuote (List InlineExpr)



-- | List InlineExpr


type InlineExpr
    = Header Int String
    | Plain String


parseAll : String -> List BlockExpr
parseAll string =
    List.map parseBlock (split "\n\n" string)


parseBlock : String -> BlockExpr
parseBlock string =
    let
        trimmedString =
            trim string
    in
        if startsWith ">" trimmedString then
            let
                inlineExprs =
                    split ">" trimmedString
                        |> List.filter (\x -> x /= "")
                        |> List.map parseInline
            in
                BlockQuote inlineExprs
        else
            InlineBlock (parseInline trimmedString)


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
