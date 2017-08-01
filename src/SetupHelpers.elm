module SetupHelpers exposing (
    ..
----    tryPlaceShip
--    tryPlaceShip2
--    ,toggleRotateShip
----    ,mbPlaceShip
    )

import Model exposing (..)
import Mouse exposing (Position)
import MouseEvents exposing (..)
import Helpers exposing (..)
import ShipHelpers exposing (..)

{-
------------------------------
application logic helpers
------------------------------
-}


{-
returns Bool indicating if a specific ship placement would
be valid based on current model state and ship size
-}
shipFits : Ship -> Bool -> Int -> List Int -> Bool
shipFits ship rotate idx usedspots =
    case getCoordsIfValidPlacement ship idx rotate usedspots of
        Nothing -> False
        _ -> True


{-
if the requested location is valid, returns a set of coords
else returns Nothing
-}
getCoordsIfValidPlacement : Ship -> Int -> Bool -> List Int -> Maybe (List Int)
getCoordsIfValidPlacement ship idx rotate usedspots =
    case generateCoords rotate idx ship.length of
        Just coords ->
            -- coords are at least on the board. check if occupied...
            if List.any (\idx -> List.member idx usedspots) coords then
                -- occupied, return Nothing to indicate failure
                Nothing
            else
                -- not occupied, return these coordinates
                Just coords

        Nothing ->
            -- coords fell outside the board or across rows for horizontal
            -- placements
            Nothing


{-
attempt to save the ship placement at 'idx'
-}
tryPlaceShip : ShipPlacementOp -> Model -> Model
tryPlaceShip {idx,side,ship} model =
    let
        -- the current player's ships
        ships = case side of
            PlayerSide1 -> model.p1ships
            PlayerSide2 -> model.p2ships

        -- spots already in use by other ships
        usedspots =
            ships
            |> List.filter (\s -> s.id /= ship.id)
            |> List.map (\s -> s.coords)
            |> List.concat
    in
        -- if the location is valid, returns a set of coordinates,
        -- otherwise returns Nothing
        case getCoordsIfValidPlacement ship idx model.rotateShip usedspots of

            Just coords ->
                -- the placement was valid, meaning there was enough space on
                -- the board and none of the requested locations are occupied.
                -- save this placement, return updated model
                saveShipUpdateModel side ship coords model

            Nothing ->
                -- something failed when attempting to allocate coordinates
                -- either they fell outside the board or were occupied
                -- return current model with no changes
                model


{-
use a coordinates generator to create a set of coordinates
returns a set of coordinates or Nothing if the attempt fails
-}
generateCoords : Bool -> Int -> Int -> Maybe (List Int)
generateCoords isvertical idx length =
    let
        -- determine the coordinates generation helper to use
        -- based on the isvertical flag
        -- horizontal generators give [x, x+1, x+2..]
        -- vertical generators give [x, x+10, x+20..]
        coordsGenerator =
            (getCoordsGenerator isvertical)
    in
        -- return a set of coordinates using the generator
        -- if the attempt fails because:
        --   the coordinates fall outside the board, or
        --   the coords span multiple rows for horizontal placements
        -- return Nothing
        -- TODO: maybe utilize Elm Results for these sorts of sitches?
        coordsGenerator idx length


{-
assign 'coords' to ship
update the appropriate list of ships based on current player
return an updated model
-}
saveShipUpdateModel : PlayerSide -> Ship -> List Int -> Model -> Model
saveShipUpdateModel side ship coords model =
    let
        oldships =
            case side of
                PlayerSide1 -> model.p1ships
                PlayerSide2 -> model.p2ships

        newship =
            {ship | coords = coords}

        newships =
            replaceShipById newship oldships

        newmodel =
            updatePlayerShips model side newships
    in
        -- update the most recently placed ship with the new coords
        -- this allows for responding to 'isvertical' toggles
        -- allowing us to know which ship was placed
        {newmodel | placingShip = Just newship}


{-
respond to a toggle of the 'vertical" aka "rotate" flag
clear any current placement
-}
toggleRotateShip : Model -> Model
toggleRotateShip model =
    let
        newmodel = clearCurrentPlacement model
    in
        {newmodel | rotateShip = not model.rotateShip}


{-
helper to update a player's list of ships
-}
updatePlayerShips : Model -> PlayerSide -> List Ship -> Model
updatePlayerShips model side ships =
    case side of
        PlayerSide1 -> { model | p1ships = ships }
        PlayerSide2 -> { model | p2ships = ships }


{-
get rid of any coords for the current 'placingShip'
-}
clearCurrentPlacement : Model -> Model
clearCurrentPlacement model =
    case model.placingShip of
        Just ship ->
            { model |
                p1ships = List.map (\s2 -> clearShipCoords ship.id s2) model.p1ships
                ,placingShip = Just { ship | coords = []}
            }

        Nothing ->
            model


{-
save the current ship/coordinates assignment and update the model
appropriately depending on the current player
this function assumes all checks have already been performed and
the coordinates are valid and available
-}
updateShip : Model -> List Ship -> Ship -> List Int -> Model
updateShip model ships ship coords =
    let
        newship =
            {ship | coords = coords}

        newships =
            replaceShipById newship ships

        newmodel =
            case model.playerTurn of
                PlayerSide1 -> {model | p1ships = newships}
                PlayerSide2 -> {model | p2ships = newships}
    in
        {newmodel | placingShip = Just newship}


{-
coordinates helpers
-}

{-
generate a set of vertical coordinates
-}
generateVerticalCoords : Int -> Int -> List Int
generateVerticalCoords start length =
    List.range 0 (length - 1)
    |> List.map (\i -> start + (i * 10))


{-
attempt to generate valid coordinates for vertically oriented placement
of 'length' spaces starting at 'idx'
if any coords fall outside the game board, return Nothing
-}
mbGenVerticalCoords : Int -> Int -> Maybe (List Int)
mbGenVerticalCoords idx length =
    let
        coords = generateVerticalCoords idx length
    in
        case List.all (\s -> s >= 0 && s < 100) coords of
            True -> Just coords
            False -> Nothing


{-
constrain the coordinates to a single row for horizontal
-}
mbGenHorizCoords : Int -> Int -> Maybe (List Int)
mbGenHorizCoords idx length =
    let
        coords = List.range idx (idx + length - 1)
        row = idx // 10 -- boardWidth
    in
        case List.all (\i -> i // 10 == row) coords of
            True -> Just coords
            False -> Nothing


{-
simple helpers
-}


{-
get the current set of ships based on playerTurn
-}
currentShipSet : Model -> List Ship
currentShipSet model =
    case model.playerTurn of
        PlayerSide1 -> model.p1ships
        PlayerSide2 -> model.p2ships


{-
returns an appropriate coordinates generator function based
on the isvertical param
-}
getCoordsGenerator : Bool -> (Int -> Int -> Maybe (List Int))
getCoordsGenerator isvertical =
    case isvertical of
        True -> mbGenVerticalCoords
        False -> mbGenHorizCoords


