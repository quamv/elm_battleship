module SetupView exposing (setupView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Model exposing (..)


setupView : Model -> Html Msg
setupView model =
    let
        viewstyles =
            [ ( "border", "0" )
            , ( "padding", "0" )
            , ( "margin", "0 auto" )
            , ( "background", "azure" )
            ]
    in
    div [ style viewstyles ]
        [ availableShipsView model

        --myshipsTable model
        ]


btnsView : Model -> Html Msg
btnsView model =
    case model.placingShip of
        Nothing ->
            div [] []

        Just ship ->
            let
                confirm =
                    not <|
                        List.any
                            (\s -> List.length s.coords /= s.length)
                            model.p1ships
            in
            if confirm then
                div []
                    [ button [ onClick ConfirmPlacement ] [ text "confirm" ]
                    ]

            else
                div [] []


availableShipsView : Model -> Html Msg
availableShipsView model =
    let
        shipsRemaining =
            List.filter (\n -> List.length n.coords == 0) model.p1ships
    in
    div
        []
        [ btnsView model
        , checkbox ToggleRotateShip "vertical"
        , ul
            [ style "min-height" "2em", style "background" "tan" ]
            (List.map
                (\n ->
                    li
                        [ attribute "draggable" "true"
                        , style "height" "200px"

                        --,onClickShip (SelectShipForPlacement n)
                        , onClickShip2 n
                        , onDragStart (SelectShipForPlacement n)
                        ]
                        [ --                                div [] [text <| toString n.shiptype]
                          button [ onClickShip2 n ] [ text "click me" ]

                        --.[button [onClick (SelectShipForPlacement n)] [text <| toString n.shiptype]]
                        ]
                )
                shipsRemaining
            )

        --                    div
        --                        []
        --                        [text <| toString model.placingShip]
        ]



-- when drag of element starts


onDragStart : msg -> Attribute msg
onDragStart message =
    onHelper "dragstart" message



-- helpers


onHelper : String -> msg -> Attribute msg
onHelper eventName message =
    onWithOptions
        eventName
        { preventDefault = False
        , stopPropagation = False
        }
        (Decode.succeed message)


onDragEnter : Int -> Attribute Msg
onDragEnter idx =
    on "dragenter" (Decode.map PlaceShip (Decode.succeed idx))


checkbox : msg -> String -> Html msg
checkbox msg name =
    label []
        [ input [ type_ "checkbox", onClick msg ] []
        , text name
        ]


myshipsTableStyle =
    [ ( "text-align", "center" )
    ]


myshipsTable model =
    table [ style myshipsTableStyle ] <| List.map (\n -> tablerow n model) (List.range 0 9)


tablerow row model =
    tr [] <| List.map (\n -> tablecell ((row * 10) + n) model) (List.range 0 9)


cellsquaredim =
    "50px"


tablecell idx model =
    let
        tdstyle =
            [ ( "height", cellsquaredim )
            , ( "width", "50px" )
            , ( "border", "1px solid black" )
            ]

        tdattrs =
            [ onDragEnter idx
            , attribute "ondragover" "return false"
            ]
                ++ (case model.placingShip of
                        Nothing ->
                            []

                        Just ship ->
                            [ onClickCell idx ]
                   )

        isoccupied =
            List.any (\n -> List.member idx n.coords) model.p1ships

        ishit =
            List.any (\n -> List.member idx n.hits) model.p1ships

        addlStyle =
            case isoccupied of
                True ->
                    [ ( "background-color", "black" ) ]

                False ->
                    [ ( "background-color", "maroon" ) ]

        issank =
            case isoccupied of
                False ->
                    False

                True ->
                    case List.filter (\n -> List.member idx n.coords) model.p1ships of
                        [] ->
                            False

                        head :: tail ->
                            head.sank

        addlStyle2 =
            case issank of
                True ->
                    [ ( "opacity", "0.5" ) ]

                False ->
                    []

        hitlayer =
            case ishit of
                True ->
                    div [ style hitStyle ] [ text "X" ]

                False ->
                    div [] [ text (toString idx) ]
    in
    td
        ([ style <| tdstyle ++ addlStyle ++ addlStyle2 ] ++ tdattrs)
        [ text <| toString idx ]


hitStyle =
    [ ( "width", "50%" )
    , ( "height", "50%" )
    , ( "background", "red" )
    , ( "margin", "0 auto" )
    ]



--dragtestdiv =
--    div [
--        style [
--            ("height","50px"),
--            ("width","50px"),
--            ("background","blue")
--        ]
--        ,attribute "draggable" "true"
--    ][]
--
--dragdropdiv =
--    div [
--        style [
--            ("height","250px"),
--            ("width","250px"),
--            ("background","beige")
--        ]
--        ,onDragEnter 10
--        ,attribute "ondragover" "return false"
--    ][]
--
--
