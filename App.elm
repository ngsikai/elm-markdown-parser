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
            case level of
                1 ->
                    h1 [] [ text str ]

                2 ->
                    h2 [] [ text str ]

                3 ->
                    h3 [] [ text str ]

                4 ->
                    h4 [] [ text str ]

                5 ->
                    h5 [] [ text str ]

                _ ->
                    h6 [] [ text str ]

        Plain str ->
            p [] [ text str ]



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
