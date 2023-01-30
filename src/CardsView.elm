----------------------------------------------------------------------
--
-- CardsView.elm
-- SVG for playing cards.
-- Copyright (c) 2023 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module CardsView exposing
    ( cardToSvg, SvgAndSize
    , Size, CardDescription
    , cardToString, stringToCard, suitToString
    )

{-| Turn a playing card into an `Svg` instance.


# Convert a `Card` to `Svg`

This is likely all that you'll use from this module.

@docs cardToSvg, SvgAndSize


# Types

@docs Size, CardDescription


# Utilities

@docs cardToString, stringToCard, suitToString

-}

import Cards exposing (Card(..), Suit(..))
import CardsView.Cards exposing (cardsJson)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import Svg exposing (Svg)
import Svg.Attributes as Svga
import SvgParser


{-| Width and height.
-}
type alias Size =
    { width : Int
    , height : Int
    }


{-| Information about one card.
-}
type alias CardDescription =
    { card : Card
    , size : Size
    , svg : String
    }


type alias CardSvgDescription msg =
    { card : Card
    , size : Size
    , svg : Svg msg
    }


{-| Parse a `String` into a `Card`.
-}
stringToCard : String -> Result String Card
stringToCard cardString =
    case String.split "+" cardString of
        [ numString, suit ] ->
            case String.toInt numString of
                Nothing ->
                    Err <| "Non-numeric card: " ++ cardString

                Just num ->
                    Ok <| Cards.defaultNew Back suit num

        _ ->
            Err <| "Not 'int+suit' string: " ++ cardString


{-| Turn a `Card` into a string.
-}
cardToString : Card -> String
cardToString card =
    case card of
        Back ->
            "0+blank"

        Card suit face ->
            (Cards.defaultFace face |> String.fromInt)
                ++ "+"
                ++ suitToString suit


{-| Turn a `Suit` into a string.
-}
suitToString : Suit -> String
suitToString suit =
    case suit of
        Clubs ->
            "clubs"

        Diamonds ->
            "diamonds"

        Hearts ->
            "hearts"

        Spades ->
            "spades"


{-| The cards.
-}
cards : Dict String (CardSvgDescription msg)
cards =
    case JD.decodeString (JD.list cardSvgDescriptionDecoder) cardsJson of
        Err err ->
            let
                err2 =
                    Debug.log "Error decoding 'cardsJson'" err
            in
            Dict.empty

        Ok cardDescriptions ->
            List.map (\cd -> ( cardToString cd.card, cd )) cardDescriptions
                |> Dict.fromList


cardSvgDescriptionDecoder : Decoder (CardSvgDescription msg)
cardSvgDescriptionDecoder =
    JD.succeed CardSvgDescription
        |> required "card" cardDecoder
        |> required "size" sizeDecoder
        |> required "svg" svgDecoder


svgDecoder : Decoder (Svg msg)
svgDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case SvgParser.parseToNode s of
                    Err err ->
                        JD.fail err

                    Ok node ->
                        SvgParser.nodeToSvg node
                            |> JD.succeed
            )


cardDecoder : Decoder Card
cardDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case stringToCard s of
                    Err err ->
                        JD.fail err

                    Ok card ->
                        JD.succeed card
            )


sizeDecoder : Decoder Size
sizeDecoder =
    JD.succeed Size
        |> required "width" JD.int
        |> required "height" JD.int


{-| Bundle up the Svg and Size of card
-}
type alias SvgAndSize msg =
    { svg : Svg msg
    , size : Size
    }


{-| Convert a `Card` and a height into `Svg`.
-}
cardToSvg : Card -> Int -> SvgAndSize msg
cardToSvg card height =
    case Dict.get (cardToString card) cards of
        Nothing ->
            SvgAndSize (Svg.text "") <| Size 0 0

        Just { size, svg } ->
            let
                factor =
                    toFloat height / toFloat size.height

                width =
                    toFloat size.width
                        * factor
                        |> round
            in
            { svg =
                Svg.g
                    [ Svga.height <| String.fromInt height
                    , Svga.viewBox <|
                        "0,0,"
                            ++ String.fromInt width
                            ++ ","
                            ++ String.fromInt height
                    ]
                    [ svg ]
            , size = Size width height
            }
