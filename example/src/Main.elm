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


view : Model -> Html Msg
view model =
    let
        { svg } =
            Debug.log "Jack of Diamonds" <|
                CardsView.cardToSvg (Card Diamonds Jack) 500
    in
    Svg.svg
        [ Svga.width "500"
        , Svga.height "500"
        ]
        [ svg ]
