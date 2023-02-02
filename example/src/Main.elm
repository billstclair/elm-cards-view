----------------------------------------------------------------------
--
-- Main.elm
-- Example for `CardsView.cardToSvg`.
-- Copyright (c) 2023 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Main exposing (main)

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Cards exposing (Card(..), Face(..), Suit(..))
import CardsView
    exposing
        ( CardDescription
        , Size
        , cardToSvg
        )
import CardsView.Cards exposing (cardsJson)
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Deck exposing (Deck, ShuffledDeck)
import Dict exposing (Dict)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , blockquote
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
import Random
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as Svga
import Task exposing (Task)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize WindowResize
        ]


type alias Model =
    { message : Maybe String
    , windowSize : ( Int, Int )
    , deck : ShuffledDeck
    , clickedCards : List Card
    }


type Msg
    = WindowResize Int Int
    | ShuffleTheDeck
    | ReceiveDeck ShuffledDeck
    | RestoreDeck
    | ClickCard Card


init : () -> ( Model, Cmd Msg )
init _ =
    { message = Just "Click on a card to move it to the pile."
    , windowSize = ( 1024, 768 )
    , deck = Deck.fullDeck |> Deck.appendCard Back
    , clickedCards = []
    }
        |> withCmds
            [ Task.perform getViewport Dom.getViewport
            ]


getViewport : Viewport -> Msg
getViewport viewport =
    let
        vp =
            viewport.viewport
    in
    WindowResize (round vp.width) (round vp.height)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize w h ->
            { model | windowSize = ( w, h ) }
                |> withNoCmd

        ShuffleTheDeck ->
            model |> withCmd shuffleDeck

        ReceiveDeck deck ->
            { model | deck = deck |> Deck.appendCard Back }
                |> withNoCmd

        RestoreDeck ->
            { model | deck = Deck.fullDeck |> Deck.appendCard Back }
                |> withNoCmd

        ClickCard card ->
            let
                cardName =
                    CardsView.cardToPrettyString card

                mdl =
                    { model | message = Just cardName }
            in
            if card == Back then
                case mdl.clickedCards of
                    [] ->
                        mdl |> withNoCmd

                    c :: rest ->
                        { mdl
                            | message = Just <| CardsView.cardToPrettyString c
                            , clickedCards = rest
                        }
                            |> withNoCmd

            else
                { mdl
                    | clickedCards = card :: mdl.clickedCards
                }
                    |> withNoCmd


shuffleDeck : Cmd Msg
shuffleDeck =
    Random.generate ReceiveDeck Deck.randomDeck


b : String -> Html msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ Char.fromCode code ]


special =
    { nbsp = stringFromCode 160 -- \u00A0
    , copyright = stringFromCode 169 -- \u00A9
    , biohazard = stringFromCode 9763 -- \u2623
    , black_star = stringFromCode 10036 -- \u2734
    , hourglass = stringFromCode 8987 -- \u231B
    , hourglass_flowing = stringFromCode 9203 -- \u23F3
    , checkmark = stringFromCode 10003 -- \u2713
    , middleDot = stringFromCode 183 -- \u00B7
    }


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


minCardHeight : Int
minCardHeight =
    100


cardsPerRow : Int
cardsPerRow =
    13


cardWidthRatio : Float
cardWidthRatio =
    let
        h =
            500

        { size } =
            CardsView.cardToSvg (Card Diamonds Jack) h
    in
    toFloat size.width
        / toFloat size.height


computeCardsPerRow : Int -> ( Int, Int ) -> { perRow : Int, cardWidth : Int, cardHeight : Int }
computeCardsPerRow spacing ( w, h ) =
    let
        minWidth =
            toFloat minCardHeight
                * cardWidthRatio
                + toFloat spacing
                |> ceiling

        perRow =
            min 13 <| w // minWidth

        width =
            w // perRow

        height =
            toFloat (width - spacing) / cardWidthRatio |> ceiling
    in
    { perRow = perRow
    , cardWidth = width
    , cardHeight = height
    }


view : Model -> Html Msg
view model =
    let
        spacing =
            6

        startX =
            spacing // 2

        ( width, height ) =
            model.windowSize

        { perRow, cardWidth, cardHeight } =
            computeCardsPerRow spacing model.windowSize

        cardSvg : ShuffledDeck -> ( Int, Int ) -> List Card -> Maybe ( ShuffledDeck, ( Int, Int ), Svg Msg )
        cardSvg deck ( x, y ) clickedCards =
            case Deck.length deck of
                0 ->
                    Nothing

                _ ->
                    let
                        ( card, nextDeck ) =
                            Deck.draw deck

                        { svg } =
                            CardsView.cardToClickableSvg (ClickCard card)
                                card
                                cardHeight

                        placedSvg =
                            Svg.g
                                [ Svga.transform <|
                                    "translate("
                                        ++ String.fromInt x
                                        ++ " "
                                        ++ String.fromInt y
                                        ++ ")"
                                ]
                                [ svg ]

                        rawNextX =
                            x + cardWidth

                        ( nextX, nextY ) =
                            if rawNextX + cardWidth > width then
                                ( startX, y + cardHeight + spacing )

                            else
                                ( rawNextX, y )
                    in
                    Just
                        ( nextDeck
                        , ( nextX, nextY )
                        , if List.member card clickedCards then
                            Svg.text ""

                          else
                            placedSvg
                        )

        loop : ShuffledDeck -> ( Int, Int ) -> List (Svg Msg) -> Svg Msg
        loop deck position svgs =
            case cardSvg deck position model.clickedCards of
                Nothing ->
                    Svg.g [] <|
                        List.reverse svgs

                Just ( nextDeck, nextPosition, svg ) ->
                    loop nextDeck nextPosition <| svg :: svgs

        rows =
            (toFloat <| Deck.length model.deck)
                / toFloat perRow
                |> ceiling

        svgHeight =
            rows * (cardHeight + spacing)
    in
    div []
        [ div [ style "margin" "auto" ]
            [ button ShuffleTheDeck "Shuffle"
            , text " "
            , button RestoreDeck "Restore"
            , br
            , case model.message of
                Nothing ->
                    text special.nbsp

                Just message ->
                    text message
            ]
        , Svg.svg
            [ Svga.width <| String.fromInt width
            , Svga.height <| String.fromInt svgHeight
            ]
            [ loop model.deck ( startX, 0 ) [] ]
        ]
