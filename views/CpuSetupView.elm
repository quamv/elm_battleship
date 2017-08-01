module CpuSetupView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (..)
import Json.Decode as Decode
import Mouse exposing (Position)

cpuSetupView : Model -> Html Msg
cpuSetupView model =
    let
        viewstyles = [
            ("border","0")
            ,("padding","0")
            ,("margin","0 auto")
            ,("background","azure")
            ]
    in
        div [] [
            text <| "setting up cpu board"
            --,button [onClick BeginGame] [text "Begin"]
            ,ul []
                <| List.map (\ship -> li [] [text <| toString ship]) model.p2ships
        ]