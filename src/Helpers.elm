module Helpers exposing (..)

import Model exposing (..)
import Mouse exposing (Position)
import MouseEvents exposing (..)

{-
general application helper functions
-}


-- add a hit point to a ship's list of hits
-- update ship's 'sank' flag based on new hit list
-- returns updated ship
addHitToShip : Int -> Ship -> Ship
addHitToShip idx ship =
    let
        newhits =
            idx :: ship.hits
    in
        {ship |
            hits = newhits
            ,sank = List.length newhits == ship.length
            }


-- returns True if a shot at 'idx' is a new hit on 'ship'
-- otherwise False
isNewHit : Int -> Ship -> Bool
isNewHit idx ship =
    (List.member idx ship.coords)
    && (not <| List.member idx ship.hits)


-- helper to grab the current player's shots list
currentShots : Model -> List Shot
currentShots model =
    case model.playerTurn of
        PlayerSide1 -> model.p1shots
        PlayerSide2 -> model.p2shots


-- wrapper to verify no previous shots at idx
-- if previous shot exists, ignore this request
-- theoretically this should never happen, so we
-- could also utilize fatalError
tryShotIfNotDuplicate : Model -> Int -> Model
tryShotIfNotDuplicate model idx =
    let
        isduplicateshot =
            currentShots model
            |> List.any (\shot -> shot.idx == idx)
    in
        case isduplicateshot of
            True -> model
            False -> tryShotAtIdx idx model 


-- helper to grab the appropriate ship list and shot list
-- attempt a shot at idx
-- update model settings
tryShotAtIdx : Int -> Model -> Model
tryShotAtIdx idx model =
    let
        (originalships, originalshots) =
            case model.playerTurn of
                PlayerSide1 -> (model.p2ships, model.p1shots)
                PlayerSide2 -> (model.p1ships, model.p2shots)

        (newships, shotresult) =
            case findHit idx originalships of
                Nothing -> (originalships, Miss)
                Just ship -> (recordHit idx ship originalships, Hit)

        newshots =
            originalshots ++ [Shot idx model.playerTurn shotresult]

        newmodel =
            case model.playerTurn of
                PlayerSide1 -> {model | p2ships = newships, p1shots = newshots}
                PlayerSide2 -> {model | p1ships = newships, p2shots = newshots}

        newplayerturn =
            case model.cpuDemo of
                True -> PlayerSide2
                False ->
                    case model.playerTurn of
                        PlayerSide1 -> PlayerSide2
                        PlayerSide2 -> PlayerSide1
    in
        {newmodel |
            gameState = nextGameState newmodel
            ,playerTurn = newplayerturn
            }


-- record a hit on ship's hit list, update the list of ships
recordHit : Int -> Ship -> List Ship -> List Ship
recordHit idx ship ships =
    addHitToShip idx ship
    |> replaceShip ships
    |> List.sortBy .id


-- replace ship in ships (based on id) with updated record
replaceShip : List Ship -> Ship -> List Ship
replaceShip ships ship  =
    ships
    |> List.map (\s -> if s.id == ship.id then ship else s)


-- check for a hit at idx among ships
findHit : Int -> List Ship -> Maybe Ship
findHit idx ships =
    ships
    |> List.filter (\s -> isNewHit idx s)
    |> List.head


-- map a shiptype to a length
shipLen : ShipType -> Int
shipLen shiptype =
    case shiptype of
        Destroyer  -> 4
        Battleship -> 5


-- create a new Ship record
newShip : ShipType -> Int -> Ship
newShip shiptype id =
    {
    id = id
    ,shiptype = shiptype
    ,length = shipLen shiptype
    ,coords = []
    ,hits = []
    ,sank = False
    }


-- helper to check if any member of ships is still alive
anyShipsAlive : List Ship -> Bool
anyShipsAlive ships =
    ships
    |> List.any (\ship -> not ship.sank)


-- get the hit coords of unsunken ships
-- once a ship is sunk, we don't care about its coords any
-- more for targeting. this routine filters sunken ships out
-- keeping only hits on unsunken ships
activeShipHits : List Ship -> List Int
activeShipHits ships =
    ships
    |> List.filter (\ship -> not ship.sank)
    |> List.map (\ship -> ship.hits)
    |> List.concat


-- the full list of spots on a board
fullSpotsList : List Int
fullSpotsList =
    List.range 0 99


-- generate the full list of possible target indexes
-- [0..99] minus all previous shots
getOpenSpots : List Shot -> List Int
getOpenSpots shots =
    fullSpotsList
    |> List.filter (\idx -> not <| List.any (\shot -> shot.idx == idx) shots)


-- determine if the game is over
-- for the game to continue, both players must have live ships
nextGameState : Model -> GameState
nextGameState model =
    let
        liveships =
            anyShipsAlive model.p1ships
            && anyShipsAlive model.p2ships
    in
        case liveships of
            True -> model.gameState
            False -> GameOver


-- end the game with diag message
fatalError : Model -> String -> Model
fatalError model str =
    let
        newdiags = {
            msg = str
            ,clickPos = MouseEvents.Position 1 1
            }
    in
        ({
        model |
            diags = newdiags
            ,gameState = GameOver
        })


-- get nth member of list, or nothing if out of range
getNth : Int -> List a -> Maybe a
getNth idx values =
    if idx < 0 then
        Nothing --List.head values
    else
        values
        |> List.drop idx
        |> List.head


nextUnplacedShip : List Ship -> Maybe Ship
nextUnplacedShip ships =
    ships
    |> List.filter (\ship -> List.length ship.coords == 0)
    |> List.head


allShipsPlaced : List Ship -> Bool
allShipsPlaced ships =
    List.all
        (\ship -> List.length ship.coords == ship.length)
        ships

