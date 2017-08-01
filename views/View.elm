module View exposing (..)

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
import DiagsView exposing (..)



view : Model -> Html Msg
view model =
    div
        [style styles.masterContainerStyle]
        [
            div
                [] --[style [("margin","0 auto"),("width","500px")]]
                [selectedView model]
            --,diagsDiv model
        ]

selectedView : Model -> Html Msg
selectedView model =
    case model.gameState of
        UserSetup->
            userSetupView model
        CPUSetup->
            cpuSetupView model
        Playing->
            sidebysideView model
        GameOver->
            gameOverView model


gameplayView : (Model -> Html Msg) -> Model -> Html Msg
gameplayView childView model =
    div []
    [
        viewSelector model
        ,childView model
    ]

numLiveShips : List Ship -> Int
numLiveShips ships =
    ships
    |> List.filter (\s -> not s.sank)
    |> List.length


cellStyle = [
    ("padding","0px 10px 0px 10px")
    ,("border","1px solid black")
    ]

tableStyle = [
    ("border","1px solid black")
    ]

shipsTable : PlayerSide -> List Ship -> Html Msg
shipsTable side ships =
    let
        colnames = ["shiptype","coords"]
    in
    div [] [
        div [] [text <| toString side ++ " Ships"]
        ,table [style tableStyle] [
            thead [] <| List.map (\s -> th [] [text s]) colnames
            ,tbody [] <| List.map
                (\ship -> tr [] [
                    td [style cellStyle] [text <| toString ship.shiptype]
                    ,td [style cellStyle] [text <| toString ship.coords]
                ])
                ships
        ]
    ]


gameOverView : Model -> Html Msg
gameOverView model =
    let
        p1shipsleft =
            numLiveShips model.p1ships

        p2shipsleft =
            numLiveShips model.p2ships

        winner =
            if p1shipsleft == 0 then
                PlayerSide2
            else -- if p2shipsleft == 0 then
                PlayerSide1

        divstyle = [
            ("margin","0 auto")
            ,("width","600px")
            ]
    in
        div [style divstyle] [
            div [] [text "Game Over!" ]
            ,div [] [text <| "Winner: " ++ toString winner]
            ,shipsTable PlayerSide1 model.p1ships
            ,shipsTable PlayerSide2 model.p2ships
        ]


getSbsStyle : PlayerSide -> PlayerSide -> List (String, String)
getSbsStyle activeside side =
    case activeside == side of
        True -> styles.sidebysideViewStyle ++ styles.activeView
        False -> styles.sidebysideViewStyle ++ styles.inactiveView


sidebysideView : Model -> Html Msg
sidebysideView model =
    div [] [
        ul [ style [("text-align","center")] ] [
            li
                [style <| getSbsStyle model.playerTurn PlayerSide1]
                --[style <| styles.sidebysideViewStyle ++ styles.activeView]
                [myshotsdiv model]
            ,li
                [style <| getSbsStyle model.playerTurn PlayerSide2]
                --[style <| styles.sidebysideViewStyle ++ styles.activeView]
                --[myshipsView model]
                [myshipsdiv model]
        ]
        --,manualOpponentView model
    ]
    --            let
    --                subview = stackedView
    --            in
    --            case model.playerTurn of
    --                PlayerSide1 -> dualView model
    --                PlayerSide2 -> dualView model

    --                PlayerSide1 ->
    --                     case model.currentView of
    --                         MyShots -> gameplayView myshotsView model
    --                         MyShips -> gameplayView myshipsView model
    --                         PerspectiveDual -> gameplayView dualView model
    --                         SideBySide -> gameplayView sidebysideView model
    --                         Stacked -> gameplayView stackedView model
    --
    --                    --gameplayView myshotsview model
    --
    --                PlayerSide2 -> gameplayView myshipsView model


myshotsdiv : Model -> Html Msg
myshotsdiv model =
    div
        []
        [
            h2 [] [text "My Shots/Cpu Ships"]
            ,myshotsView model
        ]

myshipsdiv : Model -> Html Msg
myshipsdiv model =
    div
        []
        [
            h2 [] [text "My Ships/Cpu Shots"]
            ,myshipsView model
        ]


viewSelector : Model -> Html Msg
viewSelector model =
  div [ style [("margin-bottom","10px")] ]
    [ fieldset [ style [("display","table"),("background","grey")]]
        [ radio "My Ships" (ChangeView MyShips)
        , radio "My Shots" (ChangeView MyShots)
        , radio "Perspective" (ChangeView PerspectiveDual)
        , radio "Stacked" (ChangeView Stacked)
        , radio "Side By Side" (ChangeView SideBySide)
        ]
    ]


dualView : Model -> Html Msg
dualView model =
    div [style styles.dualViewStyle]
        [
            myshotsView model
            ,myshipsView model
        ]



stackedView : Model -> Html Msg
stackedView model =
    div []
        [
            myshotsView model
            ,myshipsView model
        ]


