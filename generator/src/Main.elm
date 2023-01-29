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
import Cards exposing (Card)
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import File exposing (File)
import File.Select
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


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }


type alias Index =
    Dict String String


type alias CardIndex =
    Dict Card String


type alias Model =
    { message : Maybe String
    , index : Maybe Index
    , cardIndex : Maybe CardIndex
    }


type Msg
    = ReceiveIndex (Result Http.Error String)
    | ReceiveSvg Card (Result Http.Error String)


init : () -> ( Model, Cmd Msg )
init _ =
    { message = Nothing
    , index = Nothing
    , cardIndex = Nothing
    }
        |> withCmd (getString indexUrl ReceiveIndex)


getString : String -> (Result Http.Error String -> Msg) -> Cmd Msg
getString url wrapper =
    Http.get
        { url = url
        , expect = Http.expectString wrapper
        }


indexUrl : String
indexUrl =
    "svg/index.json"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveIndex result ->
            receiveIndex result model

        ReceiveSvg card result ->
            receiveSvg card result model


receiveIndex : Result Http.Error String -> Model -> ( Model, Cmd Msg )
receiveIndex result model =
    -- TODO
    model |> withNoCmd


receiveSvg : Card -> Result Http.Error String -> Model -> ( Model, Cmd Msg )
receiveSvg card result model =
    -- TODO
    model |> withNoCmd


view : Model -> Html Msg
view model =
    case model.message of
        Nothing ->
            text ""

        Just message ->
            p [ style "color" "red" ]
                [ text message ]


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
