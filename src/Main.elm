module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Dict.Any as Dict exposing (AnyDict)
import Html as Html exposing (Html)
import Html.Attributes as Attr exposing (attribute, class)
import Html.Events
import Parser as P exposing ((|.), (|=), Parser)
import Time
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((<?>), top)
import Url.Parser.Query as Query


main : Program () Model Msg
main =
    Browser.application
        { init = \flags url key -> ( init url key, Cmd.none )
        , view =
            \model ->
                { title = "z-index.elm"
                , body = [ view model ]
                }
        , update = update
        , subscriptions = \_ -> Time.every 5000 (\_ -> UpdateUrl)
        , onUrlRequest = ClickedLink
        , onUrlChange = \_ -> NoOp
        }



-- MODEL


type Square
    = Purple
    | Blue
    | Green
    | Yellow
    | Red


allSquares : List Square
allSquares =
    [ Purple, Blue, Green, Yellow, Red ]


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


squareToAbbrev : Square -> String
squareToAbbrev square =
    square
        |> squareToString
        |> String.left 1


type alias Model =
    { inputs : AnyDict String Square String
    , key : Navigation.Key
    }



-- ROUTE


type Route
    = Root (Maybe String)


routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    Url.Parser.map Root (top <?> Query.string "css")


init : Url -> Navigation.Key -> Model
init url key =
    let
        fromUrl =
            case Url.Parser.parse routeParser url of
                Just (Root (Just value)) ->
                    Just value

                _ ->
                    Nothing

        initialInputs =
            fromUrl
                |> Maybe.andThen (P.run urlDataParser >> Result.toMaybe)
                |> Maybe.withDefault (Dict.empty squareToString)
    in
    { inputs = initialInputs
    , key = key
    }


urlDataParser : Parser (AnyDict String Square String)
urlDataParser =
    P.loop [] sectionsHelp
        |> P.map (Dict.fromList squareToString)


squareAbbrev : Parser Square
squareAbbrev =
    P.oneOf <|
        List.map
            (\square ->
                P.succeed square
                    |. P.symbol (squareToAbbrev square)
            )
            allSquares


sectionsHelp acc =
    P.oneOf
        [ P.succeed (\square value -> P.Loop (( square, value ) :: acc))
            |= squareAbbrev
            |. P.symbol "+"
            |= (P.getChompedString
                    (P.succeed ()
                        |. P.chompIf (\_ -> True)
                        |. P.chompWhile (\c -> c /= '|')
                    )
                    |> P.map
                        (\str ->
                            str
                                |> String.split ";"
                                |> List.map (\line -> String.append line ";")
                                |> String.join "\n"
                        )
               )
            |. P.oneOf
                [ P.symbol "|"
                , P.end
                ]
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse acc))
        ]



-- UPDATE


type Msg
    = NoOp
    | ClickedLink Browser.UrlRequest
    | ChangeInput Square String
    | UpdateUrl


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

        UpdateUrl ->
            let
                encoded =
                    Dict.toList model.inputs
                        |> List.map
                            (\( square, value ) ->
                                String.concat
                                    [ squareToAbbrev square
                                    , "+"
                                    , value
                                        |> String.replace ";" ""
                                        |> String.replace "\n" ";"
                                    ]
                            )
                        |> String.join "|"
            in
            ( model
            , Navigation.replaceUrl model.key
                (Url.Builder.absolute []
                    [ Url.Builder.string "css" encoded ]
                )
            )

        NoOp ->
            ( model, Cmd.none )


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
            , Html.Events.onBlur UpdateUrl
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
