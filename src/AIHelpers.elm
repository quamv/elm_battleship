module AIHelpers exposing (..)
--    chooseTargetIdx
--    )

import Model exposing (..)
import Mouse exposing (Position)
import MouseEvents exposing (..)
import Helpers exposing (..)
import SetupHelpers exposing (..)
import ShipHelpers exposing (..)


{-
--------------------------------
CPU setup/ship placement helpers
--------------------------------
-}


{-
gets a list of valid indexes for placing 'ship'.
filters out used indexes and invalid indexes based on ship dimensions
and existing board state (other ship coords)
-}
getValidIdxs : List Int -> List Ship -> Ship -> Bool -> List Int
getValidIdxs spots ships ship rotate =
    let
        -- the list of spots already used by other ships
        -- we could exclude the current ship, but for cpu purposes
        -- it doesn't matter. the 'current ship' should never have
        -- any coordinates prior to this step for cpu.
        usedspots =
            ships
            |> List.map (\ship -> ship.coords)
            |> List.concat
    in
        -- start with the full list of spots
        spots
        -- filter used spots
        |> List.filter (\spot -> not <| List.member spot usedspots)
        -- filter spots that would fall outside the board
        |> List.filter (\spot -> enoughSpace ship.length rotate spot)
        -- filter spots that fail because there is another ship in the way
        |> List.filter (\spot -> shipFits ship rotate spot usedspots)


{-
premature optimization to eliminate invalid coordinates
-}
enoughSpace : Int -> Bool -> Int -> Bool
enoughSpace length rotate idx =
    if rotate then
        -- vertical placement, requires at least 'length' rows to fit
        -- idx // 10 gets the row number for 'idx'
        -- alternative (idx + (10*length)) < 100
        (10 - (idx // 10)) >= length
    else
        -- horizontal placement. ensure all coords are on the same row
        (10 - (rem idx 10)) >= length


{-
get a list of valid board indexes for ship placement
index into that list via the random number passed
call tryPlaceShip to attempt to place the ship at the randomized idx
-}
cpuTryPlaceShipCore : Int -> Ship -> Model -> Model
cpuTryPlaceShipCore rand ship model =
    let
        -- 'randomly' set the rotate flag for this placement
        -- TODO: determine right way to randomize another flag
        rotate =
            ((rem rand 37) > 19)

        -- get the list of valid potential indexes for this ship
        valididxs =
            getValidIdxs fullSpotsList model.p2ships ship rotate

        -- randomly select one of the valid indexes
        idx =
            rem rand (List.length valididxs)
    in
        case getNth idx valididxs of
            Nothing ->
                Debug.crash "cpuPlaceShip2 getNth returned Nothing"

            Just idx ->
                let
                    op =
                        ShipPlacementOp ship PlayerSide2 idx

                    newmodel = {model |
                        placingShip = Just ship
                        , rotateShip = rotate
                        }
                in
                    tryPlaceShip op newmodel


{-
get the next cpu ship to place
calculate a valid placement location
save the ship at that location
-}
cpuTryPlaceShip : Int -> Model -> Model
cpuTryPlaceShip rand model =

    case nextUnplacedShip model.p2ships of

        Nothing ->
            model

        Just ship ->
            let
                newmodel =
                    cpuTryPlaceShipCore rand ship model

                nextstate =
                    case allShipsPlaced newmodel.p2ships of
                        True -> Playing
                        False -> CPUSetup
            in
                {newmodel | gameState = nextstate}



{-
--------------------------------
CPU targeting/shooting helpers
--------------------------------
-}


{-
use random number for indexing into list of potential target indexes
-}
chooseTargetIdx : Int -> List Ship -> List Shot -> Maybe Int
chooseTargetIdx rand ships shots =
    let
        viabletargets = getPreferableSpots ships shots
        randIdx = rem rand (List.length viabletargets)
    in
        getNth randIdx viabletargets


{-
determine some indexes to shoot at
start with all open spaces and then use some filter functions
to improve the set of targets (if possible) down to those
which have higher probability based on their proximity to
previous hit locations and especially to streaks of hits
-}
getPreferableSpots : List Ship -> List Shot -> List Int
getPreferableSpots ships shots =
    let
        activehits = activeShipHits ships
        filterfuncs = [findHitNeighbors, findStreaks]
    in
        getOpenSpots shots
        |> improveGuess activehits filterfuncs


{-
use 'filter functions' to reduce the set of potential target indexes
-}
improveGuess : List Int -> List (List Int -> List Int -> List Int) -> List Int -> List Int
improveGuess activehits filterfuncs openspots =
    case filterfuncs of

        filterFunc::tail ->
            let
                filteredresults = filterFunc activehits openspots
            in
                case List.length filteredresults > 0 of
                    True -> improveGuess activehits tail filteredresults
                    False -> openspots

        [] ->
            openspots


{-
find potential target indexes which are neighbors with existing
hits on unsunken ships
-}
findHitNeighbors : List Int -> List Int -> List Int
findHitNeighbors activehits openspots =
    List.filter
        (\idx ->
            List.member (idx - 10) activehits
            || List.member (idx + 10) activehits
            || ((rem idx 10 > 0) && (List.member (idx - 1) activehits ))
            || ((rem idx 10 < 9) && (List.member (idx + 1) activehits ))
        )
        openspots


{-
find potential target indexes which are adjacent to existing
hit streaks on unsunken ships of at least length 2
-}
findStreaks : List Int -> List Int -> List Int
findStreaks activehits openspots =
    List.filter
        (\idx ->
            (List.member (idx+1) activehits && List.member (idx+2) activehits)
            || (List.member (idx-1) activehits && List.member (idx-2) activehits)
            || (List.member (idx-10) activehits && List.member (idx-20) activehits)
            || (List.member (idx+10) activehits && List.member (idx+20) activehits
            )
        )
        openspots


