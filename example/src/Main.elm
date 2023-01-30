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
import Cards exposing (Card(..), Face(..), Suit(..))
import CardsView
    exposing
        ( CardDescription
        , Size
        , cardToSvg
        )
import CardsView.Cards exposing (cardsJson)
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
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
import Svg exposing (Svg)
import Svg.Attributes as Svga


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }


type alias Model =
    { message : Maybe String
    }


type Msg
    = Noop


init : () -> ( Model, Cmd Msg )
init _ =
    { message = Nothing
    }
        |> withNoCmd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model |> withNoCmd


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


view : Model -> Html Msg
view model =
    let
        { svg } =
            Debug.log "Jack of Diamonds" <|
                CardsView.cardToSvg (Card Diamonds Jack) 250
    in
    div []
        [ Svg.svg
            [ Svga.width "250"
            , Svga.height "250"
            ]
            [ svg ]
        , if not debug then
            text ""

          else
            blockquote []
                (String.replace " " special.nbsp cardsJson
                    |> String.replace "\\\n" "****"
                    |> String.split "\n"
                    |> List.map (\s -> String.replace "****" "\\n" s)
                    |> List.map text
                    |> List.intersperse br
                )
        ]


debug : Bool
debug =
    False
