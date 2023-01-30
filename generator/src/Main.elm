----------------------------------------------------------------------
--
-- Main.elm
-- Generator for `.../elm-cards-view/src/CardsView/cards.elm`.
-- Copyright (c) 2023 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Main exposing (main)

import Browser
import Cards exposing (Card(..), Face(..), Suit(..))
import CardsView
    exposing
        ( CardTextDescription
        , Size
        , cardToString
        , stringToCard
        , suitToString
        )
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , col
        , div
        , figcaption
        , figure
        , h2
        , h3
        , img
        , input
        , label
        , option
        , p
        , pre
        , select
        , span
        , table
        , td
        , text
        , textarea
        , th
        , tr
        , video
        )
import Html.Attributes
    exposing
        ( alt
        , autocomplete
        , autofocus
        , autoplay
        , checked
        , class
        , cols
        , colspan
        , controls
        , disabled
        , draggable
        , height
        , hidden
        , href
        , id
        , name
        , placeholder
        , readonly
        , rows
        , selected
        , size
        , src
        , style
        , target
        , title
        , type_
        , value
        , width
        )
import Html.Events exposing (onCheck, onClick, onInput, onMouseDown)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import SvgParser exposing (Element, SvgAttribute, SvgNode(..))


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }


type alias Index =
    Dict String String


type alias Model =
    { message : Maybe String
    , index : Maybe Index
    , cardDescriptions : Maybe (List CardTextDescription)
    , text : Maybe String
    }


type Msg
    = ReceiveIndex (Result Http.Error String)
    | ReceiveSvg Card (List CardTextDescription) (List ( String, String )) (Result Http.Error String)


init : () -> ( Model, Cmd Msg )
init _ =
    { message = Nothing
    , index = Nothing
    , cardDescriptions = Nothing
    , text = Nothing
    }
        |> withCmd (getString indexUrl ReceiveIndex)


getString : String -> (Result Http.Error String -> Msg) -> Cmd Msg
getString url wrapper =
    Http.get
        { url = url
        , expect = Http.expectString wrapper
        }


svgDirectory : String
svgDirectory =
    "svg/"


indexUrl : String
indexUrl =
    svgDirectory ++ "index.json"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveIndex result ->
            receiveIndex result model

        ReceiveSvg card res indexList result ->
            receiveSvg card res indexList result model


receiveIndex : Result Http.Error String -> Model -> ( Model, Cmd Msg )
receiveIndex result model =
    case result of
        Err err ->
            { model
                | message = Just <| Debug.toString err
            }
                |> withNoCmd

        Ok json ->
            case JD.decodeString (JD.dict JD.string) json of
                Err derr ->
                    { model | message = Just <| JD.errorToString derr }
                        |> withNoCmd

                Ok index ->
                    processIndex [] (Dict.toList index) model


processIndex : List CardTextDescription -> List ( String, String ) -> Model -> ( Model, Cmd Msg )
processIndex res indexList model =
    case indexList of
        [] ->
            { model
                | message = Just "Done."
                , cardDescriptions = Just <| List.reverse res
            }
                |> withNoCmd

        ( cardString, fileName ) :: tail ->
            case stringToCard cardString of
                Err err ->
                    { model | message = Just err } |> withNoCmd

                Ok card ->
                    let
                        url =
                            svgDirectory ++ fileName
                    in
                    { model | message = Just cardString }
                        |> withCmd
                            (getString url <| ReceiveSvg card res tail)


receiveSvg : Card -> List CardTextDescription -> List ( String, String ) -> Result Http.Error String -> Model -> ( Model, Cmd Msg )
receiveSvg card res indexList result model =
    let
        errMsg prefix errString =
            Just <| prefix ++ prettyCardToString card ++ ": " ++ errString
    in
    case result of
        Err err ->
            { model
                | message =
                    errMsg "Error loading svg for: " <|
                        Debug.toString err
            }
                |> withNoCmd

        Ok svg ->
            case parseToCardTextDescription card svg model of
                Err mdl ->
                    mdl |> withNoCmd

                Ok cardTextDescription ->
                    processIndex (cardTextDescription :: res) indexList model


isSvgElement : SvgNode -> Bool
isSvgElement node =
    case node of
        SvgElement _ ->
            True

        _ ->
            False


parseToCardTextDescription : Card -> String -> Model -> Result Model CardTextDescription
parseToCardTextDescription card svg model =
    let
        errMsg : String -> String -> Maybe String
        errMsg prefix errString =
            Just <| prefix ++ prettyCardToString card ++ ", " ++ errString

        badSvgParse : String -> Result Model CardTextDescription
        badSvgParse prefix =
            Err
                { model
                    | message = errMsg prefix "see textarea below."
                    , text = Just svg
                }

        crd =
            Debug.log "Parsing card" card
    in
    case SvgParser.parseToNodes svg of
        Err perr ->
            badSvgParse <|
                "Error parsing svg for: "
                    ++ Debug.toString perr

        Ok nodes ->
            case LE.find isSvgElement nodes of
                Nothing ->
                    Err
                        { model
                            | message =
                                Just <|
                                    "No Element node for: "
                                        ++ prettyCardToString card
                            , text = Just svg
                        }

                Just (SvgElement { name, attributes }) ->
                    if name /= "svg" then
                        badSvgParse "Non-svg outer node for: "

                    else
                        let
                            attrs =
                                Dict.fromList attributes
                        in
                        case Dict.get "width" attrs of
                            Nothing ->
                                badSvgParse "No width attribute for: "

                            Just wstr ->
                                case String.toInt wstr of
                                    Nothing ->
                                        badSvgParse "Non-numeric width for: "

                                    Just w ->
                                        case Dict.get "height" attrs of
                                            Nothing ->
                                                badSvgParse "No width attribute for: "

                                            Just hstr ->
                                                case String.toInt hstr of
                                                    Nothing ->
                                                        badSvgParse
                                                            "Non-numeric height for: "

                                                    Just h ->
                                                        Ok <|
                                                            Debug.log "CardTextDescription"
                                                                { card = card
                                                                , size = Size w h
                                                                , svg = svg
                                                                }

                _ ->
                    -- Can't happen
                    Err
                        { model
                            | message =
                                Just <|
                                    "Svg not an element after we tested that it was: "
                                        ++ prettyCardToString card
                            , text = Just <| Debug.toString nodes
                        }


view : Model -> Html Msg
view model =
    let
        area : String -> Html Msg
        area str =
            textarea
                [ rows 20
                , cols 80
                , readonly True
                , value str
                ]
                []
    in
    div []
        [ case model.message of
            Nothing ->
                text ""

            Just message ->
                p [ style "color" "red" ]
                    [ text message ]
        , case model.text of
            Just txt ->
                p []
                    [ b "Bad SVG:"
                    , br
                    , area txt
                    ]

            Nothing ->
                case model.cardDescriptions of
                    Nothing ->
                        text ""

                    Just cardDescriptions ->
                        p []
                            [ b "Code for src/CardsView/Cards.elm:"
                            , br
                            , area <| cardDescriptionsToCode cardDescriptions
                            ]
        ]


cardDescriptionsToCode : List CardTextDescription -> String
cardDescriptionsToCode cardDescriptions =
    encodeCardTextDescriptions cardDescriptions
        |> fillinCodeTemplate


codeTemplate : String
codeTemplate =
    """module CardsView.Cards exposing (cardsJson)

cardsJson : String
cardsJson =
    \"\"\"
$$$
    \"\"\"
    """


fillinCodeTemplate : String -> String
fillinCodeTemplate json =
    String.replace "$$$" json codeTemplate


escapeString : String -> String
escapeString string =
    String.replace "\"" "\\\"" string
        |> String.replace "\n" "\\n"


encodeCardTextDescriptions : List CardTextDescription -> String
encodeCardTextDescriptions cardDescriptions =
    JE.list encodeCardTextDescription cardDescriptions
        |> JE.encode 2


encodeCardTextDescription : CardTextDescription -> Value
encodeCardTextDescription { card, size, svg } =
    JE.object
        [ ( "card", encodeCard card )
        , ( "size", encodeSize size )
        , ( "svg", JE.string <| escapeString svg )
        ]


encodeSize : Size -> Value
encodeSize { width, height } =
    JE.object
        [ ( "width", JE.int width )
        , ( "height", JE.int height )
        ]


encodeCard : Card -> Value
encodeCard card =
    cardToString card
        |> JE.string


prettyCardToString : Card -> String
prettyCardToString card =
    case card of
        Back ->
            "Back"

        Card suit face ->
            faceToString face ++ " of " ++ suitToString suit


faceToString : Face -> String
faceToString face =
    case face of
        Ace ->
            "Ace"

        Two ->
            "Two"

        Three ->
            "Three"

        Four ->
            "Four"

        Five ->
            "Five"

        Six ->
            "Six"

        Seven ->
            "Seven"

        Eight ->
            "Eight"

        Nine ->
            "Nine"

        Ten ->
            "Ten"

        Jack ->
            "Jack"

        Queen ->
            "Queen"

        King ->
            "King"


b : String -> Html msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


titledButton : String -> Bool -> Msg -> String -> Html Msg
titledButton theTitle enabled msg label =
    Html.button
        [ onClick msg
        , disabled <| not enabled
        , title theTitle
        , style "border-radius" "9999px"
        , style "border-width" "1px"
        ]
        [ b label ]


enabledButton : Bool -> Msg -> String -> Html Msg
enabledButton =
    titledButton ""


button : Msg -> String -> Html Msg
button =
    enabledButton True
