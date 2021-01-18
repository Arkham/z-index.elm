module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Dict.Any as Dict exposing (AnyDict)
import Html as Html exposing (Html)
import Html.Attributes as Attr exposing (attribute, class)
import Html.Events
import Parser as P exposing ((|.), (|=), Parser)


type Square
    = Purple
    | Blue
    | Green
    | Yellow
    | Red


squareToString : Square -> String
squareToString square =
    case square of
        Purple ->
            "purple"

        Blue ->
            "blue"

        Green ->
            "green"

        Yellow ->
            "yellow"

        Red ->
            "red"


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
            [ ( Purple, "" ) ]
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
                [ viewInputFor Purple model
                , viewInputFor Blue model
                , viewInputFor Green model
                , viewInputFor Yellow model
                , viewInputFor Red model
                ]
            , Html.section [ class "viewer" ]
                [ viewBoxFor Purple model [ class "square" ] []
                , viewBoxFor Blue
                    model
                    [ class "square" ]
                    [ viewBoxFor Green model [ class "small" ] []
                    , viewBoxFor Yellow model [ class "small" ] []
                    ]
                , viewBoxFor Red model [ class "square" ] []
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
            [ Html.label [] [ Html.text <| "." ++ label ]
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



-- simple wrapper for a css rule


type CssRule
    = CssRule String String


cssRuleParser : Parser CssRule
cssRuleParser =
    P.succeed CssRule
        |. P.spaces
        |= (P.getChompedString <|
                P.succeed ()
                    |. P.chompIf (\c -> Char.isAlpha c)
                    |. P.chompWhile (\c -> c /= ':')
           )
        |. P.symbol ":"
        |. P.spaces
        |= (P.getChompedString <|
                P.succeed ()
                    |. P.chompIf (\c -> Char.isAlpha c)
                    |. P.chompWhile (\c -> c /= ';')
           )


viewBoxFor :
    Square
    -> Model
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
viewBoxFor square model attrs contents =
    let
        label =
            squareToString square

        styles =
            Dict.get square model.inputs
                |> Maybe.withDefault ""
                |> String.lines
                |> List.filterMap
                    (\line ->
                        P.run cssRuleParser line
                            |> Result.toMaybe
                    )
                |> List.map
                    (\(CssRule k v) ->
                        Attr.style k v
                    )
    in
    Html.div (class label :: styles ++ attrs) contents
