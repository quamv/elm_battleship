module Helpers exposing (..)

import Model exposing (..)
import Mouse exposing (Position)
import MouseEvents exposing (..)



{-
   general application helper functions
-}
-- map a shiptype to a length


shipLen : ShipType -> Int
shipLen shiptype =
    case shiptype of
        Destroyer ->
            4

        Battleship ->
            5



-- create a new Ship record


newShip : ShipType -> Int -> Ship
newShip shiptype id =
    { id = id
    , shiptype = shiptype
    , length = shipLen shiptype
    , coords = []
    , hits = []
    , sank = False
    }



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



-- end the game with diag message


fatalError : Model -> String -> Model
fatalError model str =
    let
        newdiags =
            { msg = str
            , clickPos = MouseEvents.Position 1 1
            }
    in
    { model
        | diags = newdiags
        , gameState = GameOver
    }



-- get nth member of list, or nothing if out of range


getNth : Int -> List a -> Maybe a
getNth idx values =
    if idx < 0 then
        Nothing
        --List.head values

    else
        values
            |> List.drop idx
            |> List.head
