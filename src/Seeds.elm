module Seeds exposing (..)

import Helpers exposing (newShip)
import Model exposing (..)


defaultShipList : List Ship
defaultShipList =
    [ newShip Destroyer 0
    , newShip Battleship 1
    ]


defaultShipList2 : List Ship
defaultShipList2 =
    let
        dest =
            newShip Destroyer 0

        destroyer =
            { dest | coords = [ 0, 1, 2, 3 ] }

        bs =
            newShip Battleship 1

        battleship =
            { bs | coords = [] }
    in
    [ destroyer
    , battleship
    ]


defaultShipList3 : List Ship
defaultShipList3 =
    let
        dest2 =
            newShip Destroyer 0

        destroyer2 =
            { dest2 | coords = [ 0, 1, 2, 3 ] }

        bs2 =
            newShip Battleship 1

        battleship2 =
            { bs2 | coords = [ 10, 11, 12, 13, 14 ] }
    in
    [ destroyer2
    , battleship2
    ]
