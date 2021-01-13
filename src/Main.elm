module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Dict.Any as Dict exposing (AnyDict)
import Html as Html exposing (Html)
import Html.Attributes as Attr exposing (attribute, class)
import Html.Events


type Square
    = Orange
    | Yellow
    | Black
    | Green
    | Sand


squareToString : Square -> String
squareToString square =
    case square of
        Orange ->
            "orange"

        Yellow ->
            "yellow"

        Black ->
            "black"

        Green ->
            "green"

        Sand ->
            "sand"


type alias Model =
    { inputs : AnyDict String Square String }


type Msg
    = NoOp
    | ClickedLink Browser.UrlRequest
    | ChangeInput Square String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink request ->
            case request of
                Browser.Internal _ ->
                    ( model, Cmd.none )

                Browser.External url ->
                    ( model, Navigation.load url )

        ChangeInput square value ->
            ( { model
                | inputs =
                    Dict.insert square value model.inputs
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


initialModel : Model
initialModel =
    { inputs =
        Dict.fromList squareToString
            [ ( Orange, "" ) ]
    }


main : Program () Model Msg
main =
    Browser.application
        { init = \flags url key -> ( initialModel, Cmd.none )
        , view =
            \model ->
                { title = "z-index.elm"
                , body = [ view model ]
                }
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = ClickedLink
        , onUrlChange = \_ -> NoOp
        }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h1 []
            [ Html.a
                [ Attr.href "https://github.com/Arkham/z-index.elm"
                ]
                [ Html.text "z-index.elm" ]
            ]
        , Html.main_ [ class "container" ]
            [ Html.section [ class "editor" ]
                [ viewInputFor Orange model
                , viewInputFor Yellow model
                , viewInputFor Black model
                , viewInputFor Green model
                , viewInputFor Sand model
                ]
            , Html.section [ class "viewer" ]
                [ Html.div [ class "square orange" ] []
                , Html.div [ class "square yellow" ]
                    [ Html.div [ class "small black" ] []
                    , Html.div [ class "small green" ] []
                    ]
                , Html.div [ class "square sand" ] []
                ]
            ]
        ]


viewInputFor : Square -> Model -> Html Msg
viewInputFor square model =
    let
        label =
            squareToString square

        value =
            Dict.get square model.inputs
                |> Maybe.withDefault ""
    in
    Html.div [ class "editor__block" ]
        [ Html.div [ class "editor__label" ]
            [ Html.label [] [ Html.text label ]
            ]
        , Html.textarea
            [ Attr.value value
            , Attr.id label
            , Html.Events.onInput (ChangeInput square)
            , Attr.attribute "autocomplete" "off"
            , Attr.attribute "autocapitalize" "off"
            , Attr.attribute "spellcheck" "false"
            , Attr.attribute "enterkeyhint" "go"
            , Attr.attribute "cols" "40"
            , Attr.attribute "rows" "6"
            ]
            []
        ]
