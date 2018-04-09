module App exposing (..)

import Parser exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput)


-- MODEL


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "#hello", Cmd.none )



-- MESSAGES


type Msg
    = Change String



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ textarea [ onInput Change ] [ text model ]
            ]
        , let
            exprs =
                (parseAll model)
          in
            div [] (List.map showBlock exprs)
        ]


showBlock : BlockExpr -> Html Msg
showBlock expr =
    case expr of
        InlineBlock contents ->
            showInline contents

        BlockQuote contents ->
            blockquote [] (List.map showBlock contents)

        UList contents ->
            ul [] (List.map (\content -> li [] (List.map showBlock content)) contents)


showInline : InlineExpr -> Html Msg
showInline expr =
    case expr of
        Header level str ->
            let
                htmlText =
                    List.map showHtmlText str
            in
                case level of
                    1 ->
                        h1 [] htmlText

                    2 ->
                        h2 [] htmlText

                    3 ->
                        h3 [] htmlText

                    4 ->
                        h4 [] htmlText

                    5 ->
                        h5 [] htmlText

                    _ ->
                        h6 [] htmlText

        Paragraph str ->
            p [] (List.map showHtmlText str)


showHtmlText : HtmlText -> Html Msg
showHtmlText expr =
    case expr of
        Bold contents ->
            b [] (List.map showHtmlText contents)

        Italics contents ->
            i [] (List.map showHtmlText contents)

        Code contents ->
            code [] [ text contents ]

        Unformatted contents ->
            text contents



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ( newContent, Cmd.none )



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
