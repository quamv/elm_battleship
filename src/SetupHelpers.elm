module SetupHelpers exposing (..)

import Helpers exposing (..)
import Model exposing (..)
-- import Mouse exposing (Position)
-- import MouseEvents exposing (..)
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
        Nothing ->
            False

        _ ->
            True



{-
   if the requested location is valid, returns a set of coords
   else returns Nothing
-}


getCoordsIfValidPlacement : Ship -> Int -> Bool -> List Int -> Maybe (List Int)
getCoordsIfValidPlacement ship idx rotate usedspots =
    let
        mbcoords =
            case rotate of
                True ->
                    mbGenVerticalCoords idx ship.length

                False ->
                    mbGenHorizCoords idx ship.length
    in
    case mbcoords of
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
   if succesful, returns an updated model
   if the attempt fails, returns the previous model
-}


tryPlaceShip : ShipPlacementOp -> Model -> Model
tryPlaceShip { idx, side, ship } model =
    let
        -- the current player's ships
        ships =
            case side of
                PlayerSide1 ->
                    model.p1ships

                PlayerSide2 ->
                    model.p2ships

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
   assign 'coords' to ship
   update the appropriate list of ships based on current player
   return an updated model
-}


saveShipUpdateModel : PlayerSide -> Ship -> List Int -> Model -> Model
saveShipUpdateModel side ship coords model =
    let
        oldships =
            case side of
                PlayerSide1 ->
                    model.p1ships

                PlayerSide2 ->
                    model.p2ships

        newship =
            { ship | coords = coords }

        newships =
            replaceShipById oldships newship

        newmodel =
            case side of
                PlayerSide1 ->
                    { model | p1ships = newships }

                PlayerSide2 ->
                    { model | p2ships = newships }
    in
    -- update the most recently placed ship with the new coords
    -- this allows for responding to 'isvertical' toggles
    -- allowing us to know which ship was placed
    { newmodel
        | placingShip = Just newship
    }



{-
   respond to a toggle of the 'vertical" aka "rotate" flag
   clear any current placement
-}


toggleRotateShip : Model -> Model
toggleRotateShip model =
    case model.placingShip of
        Just ship ->
            let
                newship =
                    { ship | coords = [] }
            in
            { model
                | p1ships = replaceShipById model.p1ships newship
                , placingShip = Just newship
                , rotateShip = not model.rotateShip
            }

        Nothing ->
            { model | rotateShip = not model.rotateShip }



{-
   coordinates helpers
-}
{-
   attempt to generate valid coordinates for vertically oriented placement
   of 'length' spaces starting at 'idx'
   if any coords fall outside the game board, return Nothing
-}


mbGenVerticalCoords : Int -> Int -> Maybe (List Int)
mbGenVerticalCoords idx length =
    let
        coords =
            List.range 0 (length - 1)
                |> List.map (\i -> idx + (i * 10))
    in
    case List.all (\s -> s >= 0 && s < 100) coords of
        True ->
            Just coords

        False ->
            Nothing



{-
   constrain the coordinates to a single row for horizontal
-}


mbGenHorizCoords : Int -> Int -> Maybe (List Int)
mbGenHorizCoords idx length =
    let
        coords =
            List.range idx (idx + length - 1)

        row =
            idx // 10

        -- boardWidth
    in
    case List.all (\i -> i // 10 == row) coords of
        True ->
            Just coords

        False ->
            Nothing



{-
   simple helpers
-}
{-
   get the current set of ships based on playerTurn
-}


currentShipSet : Model -> List Ship
currentShipSet model =
    case model.playerTurn of
        PlayerSide1 ->
            model.p1ships

        PlayerSide2 ->
            model.p2ships
