module Model exposing (..)

import MouseEvents exposing (..)
import Time exposing (..)


type PlayerSide =
    PlayerSide1
    | PlayerSide2

type GameState =
    UserSetup
    | CPUSetup
    | Playing
    | GameOver

type ShipType =
    Battleship
    | Destroyer

type alias Ship = {
    id: Int
    ,shiptype: ShipType
    ,length: Int
    ,coords: List Int
    ,hits: List Int
    ,sank: Bool
    }

type ShotResult =
    Hit
    | Miss

type alias Shot = {
    idx: Int
    ,player: PlayerSide
    ,result: ShotResult
    }

type alias Diagnostics = {
    msg: String
    ,clickPos: Position
    }

type alias Drag =
    { start : Position
    , current : Position
    }

type GamePlayView =
    MyShots
    | MyShips
    | PerspectiveDual
    | SideBySide
    | Stacked

type alias Model = {
    p1ships : List Ship
    ,p2ships : List Ship
    ,p1shots : List Shot
    ,p2shots : List Shot
    ,placingShip: Maybe Ship
    ,rotateShip : Bool
    ,diags: Diagnostics
    ,position : Position
    ,drag : Maybe Drag
    ,currentView : GamePlayView
    ,gameState : GameState
    ,cpuDemo : Bool
    ,playerTurn : PlayerSide
    }


type alias ShipPlacementOp = {
    ship : Ship
    ,side : PlayerSide
    ,idx : Int
    }

type Msg =
    TakeShot Int
    | SelectShipForPlacement Ship
    | PlaceShipAtIdx ShipPlacementOp
    | ToggleRotateShip
    | ConfirmPlacement
    | ChangeView GamePlayView
    | TriggerOpponentShot
    | TakeSmahtahShawt Int
    | ToggleCpuDemo
    | AutoTriggerCpuShot Time
    | AutoTriggerCpuPlaceShip Time
    | TriggerCpuPlaceShip
    | CpuPlaceShip Int

boardWidth = 10
boardHeight = 10
cellWidthPx = 50
cellHeightPx = 50


