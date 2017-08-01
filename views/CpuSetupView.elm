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
            ,("margin-top","50px")
            ,("background","azure")
            ,("width","500px")
            ]

        numleft =
            model.p2ships
            |> List.filter (\ship -> List.length ship.coords == 0)
            |> List.length
    in
        div [style viewstyles] [
            div [] [text <| "setting up cpu board"]
            ,div [] [text <| "ships left: " ++ toString numleft]
        ]