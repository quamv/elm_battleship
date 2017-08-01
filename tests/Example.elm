module Example exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, string)

import Model exposing (..)
import Seeds exposing (..)
import ShipHelpers exposing (..)

import Helpers exposing (..)
import AIHelpers exposing (..)

openBoard = List.range 0 99


test_getNth : Test
test_getNth =
    let
        arr = [10,100,1000,1001]
    in
    describe "test_getNth" <| [
        test "0" <| \() -> Expect.equal (Just 10) <| getNth 0 arr
        ,test "1" <| \() -> Expect.equal (Just 100) <| getNth 1 arr
        ,test "2" <| \() -> Expect.equal (Just 1000) <| getNth 2 arr
        ,test "3" <| \() -> Expect.equal (Just 1001) <| getNth 3 arr
        ,test "4" <| \() -> Expect.equal Nothing <| getNth 4 arr
        ,test "4b" <| \() -> Expect.equal Nothing <| getNth -1 arr
        ,test "4b" <| \() -> Expect.equal Nothing <| getNth -2 arr
        ,test "5" <| \() ->
            Expect.equal
                arr
                (List.filterMap
                    (\val -> val)
                    (List.map (\idx -> getNth idx arr) (List.range -2 1002))
                    )
        ,test "5b" <| \() ->
            Expect.equal
                []
                (List.filterMap
                    (\val -> val)
                    (List.map (\idx -> getNth idx arr) (List.range -2 -1))
                    )

    ]

test_getShip2Place : Test
test_getShip2Place =
    let
        dest2 = newShip Destroyer 0
        destroyer2 = { dest2 | coords = [0,1,2,3]}
        bs2 = newShip Battleship 1
        battleship2 = { bs2 | coords = [] }
        ships = [destroyer2,battleship2]
        ships1 = [destroyer2]
    in
    describe "getShip2Place tests" <| [
        test "0" <| \() -> Expect.equal (Just battleship2) <| getShip2Place ships
        ,test "1" <| \() -> Expect.equal Nothing <| getShip2Place ships1
    ]



--isUsedSpot : Int -> List Ship -> Bool
--isUsedSpot idx ships =
--    coordsOccupiedByAnyShip [idx] ships
isusedspot : Test
isusedspot =
    let
        dest2 = newShip Destroyer 0
        destroyer2 = { dest2 | coords = [0,1,2,3]}
        bs2 = newShip Battleship 1
        battleship2 = { bs2 | coords = [10,11,12,13,14] }
        ships = [destroyer2,battleship2]
    in
    describe "isUsedSpottests" <| [
        test "0" <| \() -> Expect.true "" (isUsedSpot 0 ships)
        ,test "5" <| \() -> Expect.false "" (isUsedSpot 5 ships)
        ,test "6" <| \() ->
            Expect.true
                "all coords detected"
                <| List.all (\idx -> isUsedSpot idx ships) (List.range 0 3)
        ,test "6b" <| \() ->
            Expect.false
                "mixed coords detected"
                <| List.all (\idx -> isUsedSpot idx ships) <| (List.range 0 3) ++ [4]
        ,test "7" <| \() ->
            Expect.false
                "all non-coords detected"
                <| List.any (\idx -> isUsedSpot idx ships) <| (List.range 15 99) ++ [4,5,6,7,8,9]
        ,test "8" <| \() ->
            Expect.true
                "mixed detected"
                <| List.any (\idx -> isUsedSpot idx ships) (List.range 14 99)
        ,test "9" <| \() ->
            Expect.true
                "mixed detected"
                <| List.any (\idx -> isUsedSpot idx ships) <| 2 :: (List.range 15 99)
        ,test "9b" <| \() ->
            Expect.true
                "mixed detected"
                <| List.any (\idx -> isUsedSpot idx ships) <| 1 :: [15]
        ,test "10" <| \() ->
            Expect.false
                "no matches detected"
                <| List.any (\idx -> isUsedSpot idx ships) <| -1 :: (List.range 15 99)
    ]


---- see if any of the coordinates in coords are occupied by any ship
--coordsOccupiedByAnyShip : List Int -> List Ship -> Bool
--coordsOccupiedByAnyShip coords ships =
--    ships
--    |> List.any (\ship -> coordsOccupiedByShip coords ship)
--

coordsoccupiedbyanyship : Test
coordsoccupiedbyanyship =
    let
        dest2 = newShip Destroyer 0
        destroyer2 = { dest2 | coords = [0,1,2,3]}
        bs2 = newShip Battleship 1
        battleship2 = { bs2 | coords = [10,11,12,13,14] }
        ships = [destroyer2,battleship2]
    in
    describe "coordsoccupiedbyanyshiptests" <| [
        test "0" <| \() -> Expect.true "" (coordsOccupiedByAnyShip [0] ships)
        ,test "5" <| \() -> Expect.false "" (coordsOccupiedByAnyShip [5] ships)
        ,test "6" <| \() ->
            Expect.true
                "all coords detected"
                <| List.all (\idx -> coordsOccupiedByAnyShip [idx] ships) (List.range 0 3)
        ,test "6b" <| \() ->
            Expect.false
                "mixed coords detected"
                <| List.all (\idx -> coordsOccupiedByAnyShip [idx] ships) <| (List.range 0 3) ++ [4]
        ,test "7" <| \() ->
            Expect.false
                "all non-coords detected"
                <| List.any (\idx -> coordsOccupiedByAnyShip [idx] ships) <| (List.range 15 99) ++ [4,5,6,7,8,9]
        ,test "8" <| \() ->
            Expect.true
                "mixed detected"
                <| List.any (\idx -> coordsOccupiedByAnyShip [idx] ships) (List.range 14 99)
        ,test "9" <| \() ->
            Expect.true
                "mixed detected"
                <| List.any (\idx -> coordsOccupiedByAnyShip [idx] ships) <| 2 :: (List.range 15 99)
        ,test "9b" <| \() ->
            Expect.true
                "mixed detected"
                <| List.any (\idx -> coordsOccupiedByAnyShip [idx] ships) <| 1 :: [15]
        ,test "10" <| \() ->
            Expect.false
                "no matches detected"
                <| List.any (\idx -> coordsOccupiedByAnyShip [idx] ships) <| -1 :: (List.range 15 99)
    ]

--
--
---- see if there is overlap between coords and ship.coords
---- (ie. the ship is located at one or more of the coords)
--coordsOccupiedByShip : List Int -> Ship -> Bool
--coordsOccupiedByShip coords ship =
--    ship.coords
--    |> List.any (\coord -> List.member coord coords)
--

coordsoccupied : Test
coordsoccupied =
    let
        dest = newShip Destroyer 0
        destroyer = { dest | coords = [0,1,2,3]}
    in
    describe "coordsoccupiedtests" <| [
        test "0" <| \() -> Expect.true "" (coordsOccupiedByShip [0] destroyer)
        ,test "5" <| \() -> Expect.false "" (coordsOccupiedByShip [5] destroyer)
        ,test "6" <| \() ->
            Expect.true
                "all coords detected"
                <| List.all (\idx -> coordsOccupiedByShip [idx] destroyer) (List.range 0 3)
        ,test "6b" <| \() ->
            Expect.false
                "mixed coords detected"
                <| List.all (\idx -> coordsOccupiedByShip [idx] destroyer) <| (List.range 0 3) ++ [4]
        ,test "7" <| \() ->
            Expect.false
                "all non-coords detected"
                <| List.any (\idx -> coordsOccupiedByShip [idx] destroyer) (List.range 4 99)
        ,test "8" <| \() ->
            Expect.true
                "mixed detected"
                <| List.any (\idx -> coordsOccupiedByShip [idx] destroyer) (List.range 3 99)
        ,test "9" <| \() ->
            Expect.true
                "mixed detected"
                <| List.any (\idx -> coordsOccupiedByShip [idx] destroyer) <| 2 :: (List.range 4 99)
        ,test "9b" <| \() ->
            Expect.true
                "mixed detected"
                <| List.any (\idx -> coordsOccupiedByShip [idx] destroyer) <| 1 :: [4]
        ,test "10" <| \() ->
            Expect.false
                "no matches detected"
                <| List.any (\idx -> coordsOccupiedByShip [idx] destroyer) <| -1 :: (List.range 4 99)
    ]


shipavail4placement : Test
shipavail4placement =
    let
        -- get list of ships, check for some still placing
        ships = []
        var1 = 101

        f_getship2place ships =
            ships
            |> List.filter (\ship -> List.length ship.coords == 0)
            |> List.head
    in
    describe "getShip2Place" <| [
        test "1" <| \() -> Expect.equal var1 <| (List.length openBoard)
        ,test "2" <| \() -> Expect.notEqual Nothing <| f_getship2place defaultShipList
        ,test "3" <| \() -> Expect.notEqual Nothing <| f_getship2place defaultShipList2
        ,test "4" <| \() -> Expect.equal Nothing <| f_getship2place defaultShipList3
    ]



aihelpers2 : Test
aihelpers2 =
    let
        var1 = 100
    in
    test "test1" <| \() ->
        Expect.equal
            (List.length openBoard)
            var1



