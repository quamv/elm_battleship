module ShipHelpers exposing (..)

import Model exposing (..)


-- see if there is overlap between coords and ship.coords
-- (ie. the ship is located at one or more of the coords)
coordsOccupiedByShip : List Int -> Ship -> Bool
coordsOccupiedByShip coords2Test ship =
    ship.coords
    |> List.any (\i -> List.member i coords2Test)


-- see if any of the coordinates in coords are occupied by any ship
coordsOccupiedByAnyShip : List Int -> List Ship -> Bool
coordsOccupiedByAnyShip coords ships =
    ships
    |> List.any (\ship -> coordsOccupiedByShip coords ship)


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


-- record a hit on ship's hit list
-- returns updated list of ships
recordHit : Int -> Ship -> List Ship -> List Ship
recordHit idx ship ships =
    addHitToShip idx ship
    |> replaceShipById ships
    |> List.sortBy .id


-- replace ship 'id' in ships with newship
replaceShipById : List Ship -> Ship -> List Ship
replaceShipById ships newship =
    ships
    |> List.map (\s -> if s.id == newship.id then newship else s)


-- retry the last ship placement (if any)
-- this is triggered by toggling the rotate flag
-- if we had already tried placing a ship, we need to retry
-- that placement with the new rotate flag (managed in the
-- model by the calling function and not used here directly).
-- TODO: redo this by maintaining 'lastidx' in the model
rotateLastOp : Maybe Ship -> PlayerSide -> Maybe ShipPlacementOp
rotateLastOp placingShip side =
    case placingShip of
        Just ship ->
            -- there was a previous attempt
            case List.head ship.coords of
                Nothing ->
                    -- ship was selected but not placed yet
                    Nothing

                Just idx ->
                    -- trigger retry attempt at previous origin
                    Just (ShipPlacementOp ship side idx)

        Nothing ->
            -- no 'active' ship being placed
            Nothing


-- helper to grab the appropriate ship list and shot list
-- attempt a shot at idx
-- update model settings
tryShotAtIdx : Int -> Model -> Model
tryShotAtIdx idx model =
    let
        (originalships, originalshots) =
             -- get opponent's ships, and current player's shots
             case model.playerTurn of
                 PlayerSide1 -> (model.p2ships, model.p1shots)
                 PlayerSide2 -> (model.p1ships, model.p2shots)

        (newships, shotresult) =
            -- attempt a shot at location 'idx'
            let
                liveopponentships =
                    -- only 'live' ships are eligible for hit testing
                    List.filter (\ship -> not ship.sank) originalships
            in
            case takeShot idx liveopponentships of
                Nothing ->
                    -- no hit, record this as a miss
                    (originalships, Miss)

                Just ship ->
                    -- this is a hit, return updated list of ships
                    (recordHit idx ship originalships, Hit)

        newmodel =
            -- update model w/new opponent's ships and current player's shots
            -- if the shot was a miss, the ship list will remain unchanged
            let
                newshots =
                    -- record this shot in the current player's shots history
                    originalshots ++ [Shot idx model.playerTurn shotresult]
            in
            case model.playerTurn of
                PlayerSide1 -> {model | p2ships = newships, p1shots = newshots}
                PlayerSide2 -> {model | p1ships = newships, p2shots = newshots}

        nextplayerturn =
            -- whose turn is it next?
            if model.cpuDemo then
                PlayerSide2
            else
                case model.playerTurn of
                    PlayerSide1 -> PlayerSide2
                    PlayerSide2 -> PlayerSide1

        nextgamestate =
            -- determine if the game is over
            let
                keepplaying =
                    -- both players must have live ships to continue
                    List.any (\ship -> not ship.sank) newmodel.p1ships
                    && List.any (\ship -> not ship.sank) newmodel.p2ships
            in
                case keepplaying of
                    True -> model.gameState
                    False -> GameOver
    in
        {newmodel |
            gameState = nextgamestate
            ,playerTurn = nextplayerturn
            }


-- wrapper to verify no previous shots at idx
-- if no previous shots at idx, try a shot
-- if previous shot exists, ignore this request
-- theoretically this should never happen, so we
-- could also utilize fatalError
tryShot : Model -> Int -> Model
tryShot model idx =
    let
        shots =
            case model.playerTurn of
                PlayerSide1 -> model.p1shots
                PlayerSide2 -> model.p2shots
    in
        case List.any (\shot -> shot.idx == idx) shots of
            True ->
                -- duplicate shot attempt. ignore
                model

            False ->
                -- not duplicate. try shot
                tryShotAtIdx idx model



-- check for a hit at idx among ships
takeShot : Int -> List Ship -> Maybe Ship
takeShot idx ships =
    ships
    |> List.filter (\s -> isNewHit idx s)
    |> List.head


-- returns True if a shot at 'idx' is a new hit on 'ship'
-- otherwise False
isNewHit : Int -> Ship -> Bool
isNewHit idx ship =
    (List.member idx ship.coords)
    && (not <| List.member idx ship.hits)


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

-- determine if the game is over
-- for the game to continue, both players must have live ships
nextGameState : Model -> GameState
nextGameState model =
    let
        keepplaying =
            List.any (\ship -> not ship.sank) model.p1ships
            && List.any (\ship -> not ship.sank) model.p2ships
    in
        case keepplaying of
            True -> model.gameState
            False -> GameOver


allShipsPlaced : List Ship -> Bool
allShipsPlaced ships =
    ships
    |> List.all (\ship -> List.length ship.coords /= 0)
