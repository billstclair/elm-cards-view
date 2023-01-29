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
import Cards exposing (Card(..))
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


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }


type alias Index =
    Dict String String


type alias Size =
    { width : Int
    , height : Int
    }


type alias CardDescription =
    { card : Card
    , size : Size
    , svg : String
    }


type alias Model =
    { message : Maybe String
    , index : Maybe Index
    , cardDescriptions : Maybe (List CardDescription)
    }


type Msg
    = ReceiveIndex (Result Http.Error String)
    | ReceiveSvg Card (List CardDescription) (List ( String, String )) (Result Http.Error String)


init : () -> ( Model, Cmd Msg )
init _ =
    { message = Nothing
    , index = Nothing
    , cardDescriptions = Nothing
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


processIndex : List CardDescription -> List ( String, String ) -> Model -> ( Model, Cmd Msg )
processIndex res indexList model =
    case indexList of
        [] ->
            { model
                | message = Nothing
                , cardDescriptions = Just res
            }
                |> withNoCmd

        ( cardString, fileName ) :: tail ->
            case cardStringToCard cardString of
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


cardStringToCard : String -> Result String Card
cardStringToCard cardString =
    case String.split "+" cardString of
        [ numString, suit ] ->
            case String.toInt numString of
                Nothing ->
                    Err <| "Non-numeric card: " ++ cardString

                Just num ->
                    Ok <| Debug.log "card" <| Cards.defaultNew Back suit num

        _ ->
            Err <| "Not 'int+suit' string: " ++ cardString


receiveSvg : Card -> List CardDescription -> List ( String, String ) -> Result Http.Error String -> Model -> ( Model, Cmd Msg )
receiveSvg card res indexList result model =
    -- TODO
    model |> withNoCmd


view : Model -> Html Msg
view model =
    div []
        [ case model.message of
            Nothing ->
                text ""

            Just message ->
                p [ style "color" "red" ]
                    [ text message ]
        , case model.cardDescriptions of
            Nothing ->
                text ""

            Just cardDescriptions ->
                p []
                    [ b "Code for src/CardsView/cards.elm:"
                    , br
                    , textarea
                        [ rows 20
                        , cols 80
                        , readonly True
                        , value <| cardDescriptionsToCode cardDescriptions
                        ]
                        []
                    ]
        ]


cardDescriptionsToCode : List CardDescription -> String
cardDescriptionsToCode cardDescriptions =
    -- TODO
    Debug.toString cardDescriptions


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
