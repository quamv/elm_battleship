module MyShipsView exposing (myshipsView, manualOpponentView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Json.Decode as Decode
import ViewStyles exposing (..)
import ViewCommon exposing (..)


myshipsView : Model -> Html Msg
myshipsView model =
    div
        [style styles.containerStyle]
        [
            tableGen cellGen model styles.myshipsTableStyle
            --,manualOpponentView model
        ]


manualOpponentView model =
    if model.gameState == GameOver then
        div [] []
    else
        div []
        [
            button
                [onClick ToggleCpuDemo]
                [text "toggle cpu demo"]
            ,case model.cpuDemo of
                True -> span [] []
                False ->
                    button
                        [onClick TriggerOpponentShot]
                        [text "trigger opponent shot"]
    --        ,ul []
    --            <| List.map (\shot -> li [] [text <| toString shot]) model.p2shots
        ]


getShotResultStyle : Int -> Model -> List (String,String)
getShotResultStyle idx model =
    case previousShotAtIdx idx model.p2shots of
        Nothing -> []
        Just shot ->
            case shot.result of
                Miss -> styles.missStyle
                Hit -> styles.hitShotStyle


cellGen : Int -> Model -> Html Msg
cellGen idx model  =
    let
        cellStyle =
            styles.tdstyle
            ++ optionalStyle (isOccupied idx model.p1ships) styles.occupiedStyle
            ++ optionalStyle (isSank idx model.p1ships) styles.sankStyle
            ++ getShotResultStyle idx model
    in
        td
            [style cellStyle]
            [text <| toString idx]


