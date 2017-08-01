module Main exposing (..)

{-
system imports
-}
import Array exposing (..)
import Html exposing (..)
import Time exposing (Time, millisecond)
import Mouse exposing (..)
import MouseEvents exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Decode
import Random exposing (..)

{-
local imports
-}
import Model exposing (..)
import Seeds exposing (..)
import Helpers exposing (..)
import ShipHelpers exposing (..)
import AIHelpers exposing (..)
import SetupHelpers exposing (..)
import View exposing (..)


main =
    Html.program {
        init = init,
        view = view,
        update = update,
        --subscriptions = \_ -> Sub.none
        subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        GameOver -> Sub.none
        UserSetup -> Sub.none
        CPUSetup ->
            Sub.batch [Time.every (200 * millisecond) AutoTriggerCpuPlaceShip]
        Playing ->
            case model.cpuDemo of
                True ->
                    Sub.batch [Time.every (200 * millisecond) AutoTriggerCpuShot]

                False ->
                    case model.playerTurn of
                        PlayerSide2 -> Sub.batch [Time.every (200 * millisecond) AutoTriggerCpuShot]
                        PlayerSide1 -> Sub.none


init : (Model, Cmd Msg)
init =
    (
        {
            p1ships = defaultShipList3
            ,p2ships = defaultShipList --defaultShipList3
            ,p1shots = []
            ,p2shots = []
            ,diags = {msg="", clickPos=MouseEvents.Position 0 0}
            ,placingShip = Nothing
            ,rotateShip = False
            ,position = MouseEvents.Position 1 1
            ,drag = Nothing
            ,currentView = MyShips
            ,gameState = UserSetup
            ,cpuDemo = False
            ,playerTurn = PlayerSide1
        }
        , Cmd.none
    )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =

    case msg of

        {- setup
         TODO: organize messages into classes/submessages
        -}
        AutoTriggerCpuPlaceShip diff ->
            update TriggerCpuPlaceShip model

        TriggerCpuPlaceShip ->
            -- kick off a randomly-generated opponent (cpu) ship placement
            (
                {model | playerTurn = PlayerSide2},
                Random.generate CpuPlaceShip (Random.int 0 Random.maxInt)
            )

        CpuPlaceShip rand ->
            (cpuTryPlaceShip rand model, Cmd.none)

        PlaceShipAtIdx shipplacementop ->
            (tryPlaceShip shipplacementop model, Cmd.none)

        SelectShipForPlacement ship ->
            -- a ship has been chosen for placement
            ({model | placingShip = Just ship }, Cmd.none)

        ConfirmPlacement ->
            -- confirm ship placements. move to cpu setup state
            ({model |
                placingShip = Nothing
                ,gameState = CPUSetup
                ,currentView = MyShips
                ,playerTurn = PlayerSide2
                }
            , Cmd.none)

        ToggleRotateShip ->
            -- toggle model.rotateShip
            -- clear any current coordinates
            -- retry at previous idx (if exists) with new orientation
            let
                newmodel = toggleRotateShip model
            in
                case rotateLastOp model.placingShip model.playerTurn of
                    Nothing -> (newmodel, Cmd.none)
                    Just op -> update (PlaceShipAtIdx op) newmodel


        {- shot-related messages -}

        AutoTriggerCpuShot diff ->
            -- timer tick to automate cpu shots
            -- just re-use existing message route
            update TriggerOpponentShot model

        TriggerOpponentShot ->
            -- kick off a randomly-generated opponent (cpu) shot
            ({model | playerTurn = PlayerSide2}, Random.generate TakeSmahtahShawt (Random.int 0 Random.maxInt))

        TakeSmahtahShawt rand ->
            -- take a random number and use it to randomize the selection
            -- of potential target indexes
            case chooseTargetIdx rand model.p1ships model.p2shots of
                Nothing -> (fatalError model "chooseTargetIdx returned Nothing", Cmd.none)
                Just spotIdx -> update (TakeShot spotIdx) model

        TakeShot idx ->
            -- take a shot at idx
            (tryShotIfNotDuplicate model idx, Cmd.none)


        {- misc messages -}

        ToggleCpuDemo ->
            -- toggle cpu demo mode
            ({model | cpuDemo = not model.cpuDemo}, Cmd.none)

        ChangeView newview ->
            -- change the current view to newview
            ({model | currentView = newview}, Cmd.none)


