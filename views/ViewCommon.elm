module ViewCommon exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Model exposing (..)
-- import Mouse exposing (Position)


isOccupied : Int -> List Ship -> Bool
isOccupied idx ships =
    ships
        |> List.any (\n -> List.member idx n.coords)


isPreviousHit : Int -> List Ship -> Bool
isPreviousHit idx ships =
    ships
        |> List.any (\n -> List.member idx n.hits)


previousShotAtIdx : Int -> List Shot -> Maybe Shot
previousShotAtIdx idx shots =
    shots
        |> List.filter (\shot -> shot.idx == idx)
        |> List.head


isSank : Int -> List Ship -> Bool
isSank idx ships =
    ships
        |> List.filter (\s -> s.sank)
        |> List.any (\s -> List.member idx s.hits)


checkbox : msg -> String -> Html msg
checkbox msg name =
    label
        []
        [ input [ type_ "checkbox", onClick msg ] []
        , text name
        ]


type alias CellGenerator =
    Int -> Model -> Html Msg


tableGen : CellGenerator -> Model -> List ( String, String ) -> Html Msg
tableGen cellGen model tableStyle =
    table
        [ style tableStyle ]
    <|
        List.map (\n -> rowGen cellGen n model) (List.range 0 9)


rowGen : CellGenerator -> Int -> Model -> Html Msg
rowGen cellGen row model =
    tr
        []
    <|
        List.map (\n -> cellGen ((row * 10) + n) model) (List.range 0 9)


radio : String -> msg -> Html msg
radio value msg =
    label
        [ style "padding" "2px"
        , style "display" "table-cell"
        ]
        [ input [ type_ "radio", name "font-size", onClick msg ] []
        , text value
        ]



-- VIEW
{-
   r = random nbr
   r mod 2 is vertical flag
   get length of next ship to place
   spaces = list of viable spots
   n = rem r (length of the list)
   convert spaces to array
   get the nth element
-}


(=>) =
    \a b -> ( a, b )


px : Int -> String
px number =
    toString number ++ "px"


getPosition : Model -> Position
getPosition { position, drag } =
    case drag of
        Nothing ->
            position

        Just { start, current } ->
            Position
                (position.x + current.x - start.x)
                (position.y + current.y - start.y)
