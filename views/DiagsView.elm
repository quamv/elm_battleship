module DiagsView exposing (..)

--import TableTestView exposing (..)
--import PlayerView exposing (playerView)
--import PlayerViewTable exposing (playerView)

import CpuSetupView exposing (cpuSetupView)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (..)
import MyShipsView exposing (..)
import MyShotsView exposing (myshotsView)
import UserSetupView exposing (userSetupView)
import ViewCommon exposing (..)
import ViewStyles exposing (..)
import Helpers exposing (playerSideToString)
import Helpers exposing (shipTypeToString)
import Helpers exposing (listOfIntsToString)



--tableView : List String -> (List String -> Html Msg) -> (List a -> Html Msg) -> Html Msg


tableView : List String -> (List a -> List (Html Msg)) -> List a -> Html Msg
tableView cols trowmap rows =
    table []
        [ thead [] <| List.map (\s -> th [] [ text s ]) cols
        , tbody [] <| trowmap rows
        ]


shipsTable : PlayerSide -> List Ship -> Html Msg
shipsTable side ships =
    let
        colnames =
            [ "type", "coords" ]
    in
    div []
        [ div [] [ text <| playerSideToString side ]
        , table []
            [ thead [] <| List.map (\s -> th [] [ text s ]) colnames
            , tbody [] <|
                List.map
                    (\ship ->
                        tr []
                            [ td [] [ text <| shipTypeToString ship.shiptype ]
                            , td [] [ text <| listOfIntsToString ship.coords ]
                            ]
                    )
                    ships
            ]
        ]


diagsDiv : Model -> Html Msg
diagsDiv model =
    div []
        [ shipsTable PlayerSide1 model.p1ships
        , shipsTable PlayerSide2 model.p2ships
        ]


diagShotTd : String -> Html Msg
diagShotTd str =
    td [] [ text str ]
