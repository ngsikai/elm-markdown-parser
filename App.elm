module App exposing (..)

import Parser exposing (..)


-- import Html

import Html.Styled.Events exposing (onInput)
import Css exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled exposing (..)


-- MODEL


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "# Overview\n* Introduction to Markdown\n* Our Project - MiniMark\n* Demo of MiniMark\n  * It's happening now!\n* Critique of the Elm Language\n\n#### Here's some blockquote magic:\n> This is a blockquote\n>> This is a *nested* blockquote\n>>> This is a __really__ *nested* blockquote", Cmd.none )



-- MESSAGES


type Msg
    = Change String



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ css
            [ padding2 (px 30) (px 50)
            ]
        ]
        [ h1
            [ css
                [ marginLeft (pct 3)
                , fontFamily monospace
                , marginBottom (px 30)
                ]
            ]
            [ text "Simple Markdown Parser powered by Elm" ]
        , div
            [ css
                [ width (pct 45)
                , height (px 400)
                , float left
                , marginLeft (pct 3)
                ]
            ]
            [ textarea
                [ onInput Change
                , css
                    [ resize vertical
                    , width (pct 100)
                    , height (pct 100)
                    , fontSize (px 15)
                    , fontFamily monospace
                    ]
                ]
                [ text model ]
            ]
        , let
            exprs =
                (parseAll model)
          in
            div
                [ css
                    [ width (pct 45)
                    , height (px 400)
                    , maxHeight (px 400)
                    , float left
                    , marginLeft (pct 4)
                    , paddingLeft (px 5)
                    , fontSize (px 16)
                    , overflow scroll
                    , boxShadow3 (px 0) (px 0) (px 5)
                    ]
                ]
                (List.map showBlock exprs)
        ]


showBlock : BlockExpr -> Html Msg
showBlock expr =
    case expr of
        InlineBlock contents ->
            showInline contents

        BlockQuote contents ->
            blockquote [ css [ borderLeft3 (px 5) solid (rgb 238 238 238), padding2 (px 5) (px 10) ] ] (List.map showBlock contents)

        UList contents ->
            ul [] (List.map (\content -> li [] (List.map showBlock content)) contents)


showInline : InlineExpr -> Html Msg
showInline expr =
    case expr of
        Header level contents ->
            let
                htmlText =
                    List.map showHtmlText contents
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

        Paragraph contents ->
            p [] (List.map showHtmlText contents)


showHtmlText : HtmlText -> Html Msg
showHtmlText expr =
    case expr of
        Bold contents ->
            b [] (List.map showHtmlText contents)

        Italics contents ->
            i [] (List.map showHtmlText contents)

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
