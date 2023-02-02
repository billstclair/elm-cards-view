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
    ( cardToSvg, cardToClickableSvg
    , CardDescription, Size, CardTextDescription
    , cardToPrettyString, cardToString, stringToCard, suitToString
    )

{-| Turn a playing card into an `Svg` instance.


# Convert a `Card` to `Svg`

This is likely all that you'll use from this module.

@docs cardToSvg, cardToClickableSvg


# Types

@docs CardDescription, Size, CardTextDescription


# Utilities

@docs cardToPrettyString, cardToString, stringToCard, suitToString

-}

import Cards exposing (Card(..), Face(..), Suit(..))
import CardsView.Cards exposing (cardsJson)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import Svg exposing (Svg)
import Svg.Attributes as Svga
import Svg.Events as Svge
import SvgParser exposing (SvgNode(..))


{-| Width and height.
-}
type alias Size =
    { width : Int
    , height : Int
    }


{-| Low-level information about a card.
-}
type alias CardTextDescription =
    { card : Card
    , size : Size
    , svg : String
    }


{-| Information about one card.
-}
type alias CardDescription msg =
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


{-| Convert a `Face` to a `String`.
-}
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


{-| Turn a `Card` into a pretty string.

e.g. `cardToPretyString(Card Spades Queen) -> "Queen of Spades"`

-}
cardToPrettyString : Card -> String
cardToPrettyString card =
    case card of
        Back ->
            "Back"

        Card suit face ->
            faceToString face ++ " of " ++ suitToString suit


{-| The cards.
-}
cards : Dict String (CardDescription msg)
cards =
    case JD.decodeString (JD.list cardDescriptionDecoder) cardsJson of
        Err err ->
            let
                err2 =
                    Debug.log "Error decoding 'cardsJson'" err
            in
            Dict.empty

        Ok cardDescriptions ->
            List.map (\cd -> ( cardToString cd.card, cd )) cardDescriptions
                |> Dict.fromList


cardDescriptionDecoder : Decoder (CardDescription msg)
cardDescriptionDecoder =
    JD.succeed CardDescription
        |> required "card" cardDecoder
        |> required "size" sizeDecoder
        |> required "svg" svgDecoder


isSvgElement : SvgNode -> Bool
isSvgElement node =
    case node of
        SvgElement _ ->
            True

        _ ->
            False


svgDecoder : Decoder (Svg msg)
svgDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case SvgParser.parseToNodes s of
                    Err err ->
                        JD.fail err

                    Ok nodes ->
                        case LE.find isSvgElement nodes of
                            Nothing ->
                                JD.fail "No Svg Element."

                            Just node ->
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


{-| Convert a `Card` and a height into `Svg`.
-}
cardToSvg : Card -> Int -> CardDescription msg
cardToSvg =
    cardToSvgMaybeClick Nothing


{-| Convert a `Card` and a height into a clickable `Svg`, which invokes `msg`
when clicked.
-}
cardToClickableSvg : msg -> Card -> Int -> CardDescription msg
cardToClickableSvg msg =
    cardToSvgMaybeClick <| Just msg


cardToSvgMaybeClick : Maybe msg -> Card -> Int -> CardDescription msg
cardToSvgMaybeClick maybeMsg card height =
    case Dict.get (cardToString card) cards of
        Nothing ->
            { card = card
            , size = Size 0 0
            , svg =
                Svg.text_
                    []
                    [ Svg.text <| "Didn't find card: " ++ Debug.toString card ]
            }

        Just { size, svg } ->
            let
                factor =
                    toFloat height / toFloat size.height

                width =
                    toFloat size.width
                        * factor
                        |> round

                maybeClickable : Svg msg -> Svg msg
                maybeClickable svg2 =
                    case maybeMsg of
                        Nothing ->
                            svg2

                        Just msg ->
                            Svg.g [ Svge.onClick msg ]
                                [ svg2 ]
            in
            { card = card
            , size = Size width height
            , svg =
                Svg.g
                    [ Svga.transform <| "scale(" ++ String.fromFloat factor ++ ")"
                    ]
                    [ svg ]
                    |> maybeClickable
            }
