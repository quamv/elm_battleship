module ShipHelpers exposing (..)

import Model exposing (..)


-- helper to clear the coordinate set for ship 'id' (ship.id == id)
-- used as a List.map processor, hence the id check
clearShipCoords : Int -> Ship -> Ship
clearShipCoords id ship =
    case ship.id == id of
        True -> {ship | coords = []}
        False -> ship


-- see if there is overlap between coords and ship.coords
-- (ie. the ship is located at one or more of the coords)
coordsOccupiedByShip : List Int -> Ship -> Bool
coordsOccupiedByShip coords ship =
    ship.coords
    |> List.any (\coord -> List.member coord coords)


-- see if any of the coordinates in coords are occupied by any ship
coordsOccupiedByAnyShip : List Int -> List Ship -> Bool
coordsOccupiedByAnyShip coords ships =
    ships
    |> List.any (\ship -> coordsOccupiedByShip coords ship)


-- replace ship 'id' in ships with newship
replaceShipById : Ship -> List Ship -> List Ship
replaceShipById newship ships =
    let
        replaceById newship oldship =
            case newship.id == oldship.id of
                True -> newship
                False -> oldship
    in
    ships
    |> List.map (\s -> replaceById newship s)


-- retry the last ship placement (if any)
-- this is triggered by toggling the rotate flag
-- if we had already tried placing a ship, we need to retry
-- that placement with the new rotate flag which is set in the
-- model by the calling function and not used here directly.
-- TODO: redo this by maintaining 'lastidx' in the model
rotateLastOp : Maybe Ship -> PlayerSide -> Maybe ShipPlacementOp
rotateLastOp placingShip side =
    case placingShip of
        Just ship ->
            -- there was a previous attempt
            case List.head ship.coords of
                Nothing ->
                    -- this probably should never happen
                    -- maybe call fatalerror here
                    Nothing

                Just idx ->
                    -- trigger retry attempt at previous origin
                    Just (ShipPlacementOp ship side idx)

        Nothing ->
            -- no previous placement
            Nothing

