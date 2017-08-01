module DiagsView exposing (..)

import Html exposing (Html, Attribute, div, fieldset, input, label, text)
import Html.Attributes exposing (name, style, type_)
import Html.Events exposing (onClick)
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
--import TableTestView exposing (..)
--import PlayerView exposing (playerView)
--import PlayerViewTable exposing (playerView)
import ViewStyles exposing (..)
import ViewCommon exposing (..)
import MyShotsView exposing (myshotsView)
import MyShipsView exposing (..)
import UserSetupView exposing (userSetupView)
import CpuSetupView exposing (cpuSetupView)


--tableView : List String -> (List String -> Html Msg) -> (List a -> Html Msg) -> Html Msg
tableView : List String -> (List a -> List (Html Msg)) -> List a -> Html Msg
tableView cols trowmap rows =
    table [] [
        thead [] <| List.map (\s -> th [] [text s]) cols
        ,tbody [] <| trowmap rows
    ]


shipsTable : PlayerSide -> List Ship -> Html Msg
shipsTable side ships =
    let
        colnames = ["type","coords"]
    in
    div [] [
        div [] [text <| toString side]
        ,table [] [
            thead [] <| List.map (\s -> th [] [text s]) colnames
            ,tbody [] <| List.map
                (\ship -> tr [] [
                    td [] [text <| toString ship.shiptype]
                    ,td [] [text <| toString ship.coords]
                ])
                ships
        ]
    ]

diagsDiv : Model -> Html Msg
diagsDiv model =
    div [] [
        shipsTable PlayerSide1 model.p1ships
        ,shipsTable PlayerSide2 model.p2ships
        ]


diagShotTd : String -> Html Msg
diagShotTd str =
    td [] [text str]


