module App exposing (..)

import Html exposing (Html, div, textarea, text, span, h1, program)
import Html.Events exposing (onInput)
import String exposing (startsWith, dropLeft)

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
        [
            div [] [
                textarea [onInput Change] [text model]
            ]
            , div [] [show (parse model)]
        ]

type Fragments
    = H1 String
    | Plain String

parse : Model -> Fragments
parse model =
    if startsWith "#" model then H1 (dropLeft 1 model) else Plain model

show : Fragments -> Html Msg
show h =
    case h of
        H1 str -> h1 [] [text str]
        Plain str -> span [] [text str]

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