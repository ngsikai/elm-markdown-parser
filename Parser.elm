module Parser exposing (..)

import String exposing (startsWith, dropLeft, split, lines, words, toList, length)


type MarkdownExpr
    = Header Int String
    | Plain String


parseAll : String -> List MarkdownExpr
parseAll string =
    List.map parse (lines string)


parse : String -> MarkdownExpr
parse string =
    if isValidHeader string then
        let
            hashCount =
                case List.head (words string) of
                    Just hd ->
                        length hd

                    Nothing ->
                        0
        in
            Header hashCount (dropLeft hashCount string)
    else
        Plain string


isValidHeader : String -> Bool
isValidHeader string =
    case (List.head (words string)) of
        Just hd ->
            isAllHash hd

        Nothing ->
            False


isAllHash : String -> Bool
isAllHash string =
    List.foldr (\el acc -> el == '#' && acc) True (toList string)
