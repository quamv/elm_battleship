module UserSetupView exposing (userSetupView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Model exposing (..)
import ViewCommon exposing (..)
import ViewStyles exposing (..)


userSetupView : Model -> Html Msg
userSetupView model =
    div
        [ style <| styles.containerStyle ++ [ ( "width", "500px" ) ] ]
        [ availableShipsView model
        , tableGen cellGen model styles.myshipsTableStyle
        , btnsView model
        ]


cellGen : Int -> Model -> Html Msg
cellGen idx model =
    let
        cellStyle =
            styles.tdstyle
                ++ optionalStyle (isOccupied idx model.p1ships) styles.occupiedStyle
                ++ optionalStyle (isSank idx model.p1ships) styles.sankStyle

        --        tdattrs = [
        --            onDragEnter idx
        --            ,attribute "ondragover" "return false"
        --            ]
        --            ++ case model.placingShip of
        --                Just _ -> [onClickCell (PlaceShip idx)]
        --                Nothing -> []
        tdattrs =
            [ attribute "ondragover" "return false"
            ]
                ++ (case model.placingShip of
                        Just ship ->
                            let
                                op =
                                    ShipPlacementOp ship PlayerSide1 idx
                            in
                            [ onDragEnter2 idx model ship, onClickCell (PlaceShipAtIdx op) ]

                        Nothing ->
                            []
                   )
    in
    td
        ([ style cellStyle ] ++ tdattrs)
        [ text <| toString idx ]


btnsView : Model -> Html Msg
btnsView model =
    let
        allShipsReady =
            model.p1ships
                |> List.all (\s -> List.length s.coords == s.length)
    in
    if allShipsReady then
        div []
            [ button [ onClick ConfirmPlacement ] [ text "done" ]
            ]

    else
        div [] []


availableShipListStyle =
    [ ( "list-style-type", "none" )
    , ( "padding", "0.2em" )
    , ( "min-height", "2em" )
    , ( "background", "tan" )
    ]


currentlyPlacingShipStyle =
    [ ( "border", "1px solid black" )
    , ( "background", "grey" )
    ]


possibleCurrentlyPlacing model ship =
    case model.placingShip of
        Just placingship ->
            case placingship.id == ship.id of
                True ->
                    [ style currentlyPlacingShipStyle ]

                False ->
                    []

        Nothing ->
            []


availableShipsView : Model -> Html Msg
availableShipsView model =
    let
        shipsRemaining =
            model.p1ships

        --List.filter (\n -> List.length n.coords == 0) model.p1ships
    in
    div
        []
        [ h2 [] [ text "available ships" ]
        , div [] [ text "click ship to select, then desired index to place (or drag)" ]
        , ul
            [ style availableShipListStyle ]
            (List.map
                (\n ->
                    li
                        ([ attribute "draggable" "true"
                         , onDragStart (SelectShipForPlacement n)
                         , onClickShip (SelectShipForPlacement n)
                         ]
                            ++ possibleCurrentlyPlacing model n
                        )
                        [ div [] [ text <| toString n.shiptype ]

                        --.[button [onClick (SelectShipForPlacement n)] [text <| toString n.shiptype]]
                        ]
                )
                shipsRemaining
            )

        --                    div
        --                        []
        --                        [text <| toString model.placingShip]
        , checkbox ToggleRotateShip "vertical"
        ]



-- helpers


onHelper : String -> msg -> Attribute msg
onHelper eventName message =
    onWithOptions
        eventName
        { preventDefault = False
        , stopPropagation = False
        }
        (Decode.succeed message)


onClickCell : msg -> Attribute msg
onClickCell message =
    onHelper "click" message


onClickShip : msg -> Attribute msg
onClickShip message =
    onHelper "click" message


onClickShip2 : Ship -> Attribute Msg
onClickShip2 ship =
    on "click" (Decode.map SelectShipForPlacement (Decode.succeed ship))



-- when drag of element starts


onDragStart : msg -> Attribute msg
onDragStart message =
    onDragHelper "dragstart" message



-- helpers


onDragHelper : String -> msg -> Attribute msg
onDragHelper eventName message =
    onWithOptions
        eventName
        { preventDefault = False
        , stopPropagation = False
        }
        (Decode.succeed message)



--onDragEnter : Int -> Attribute Msg
--onDragEnter idx =
--    on "dragenter" (Decode.map PlaceShip (Decode.succeed idx))


onDragEnter2 : Int -> Model -> Ship -> Attribute Msg
onDragEnter2 idx model ship =
    let
        shipplacementop =
            ShipPlacementOp ship PlayerSide1 idx
    in
    on "dragenter" (Decode.map PlaceShipAtIdx (Decode.succeed shipplacementop))


checkbox : msg -> String -> Html msg
checkbox msg name =
    label []
        [ input [ type_ "checkbox", onClick msg ] []
        , text name
        ]
