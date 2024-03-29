module MyShotsView exposing (myshotsView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Model exposing (..)
import ViewCommon exposing (..)
import ViewStyles exposing (..)
import Helpers


viewstyles =
    [ style "transform" "rotateX(-14deg)" ]


myshotsView : Model -> Html Msg
myshotsView model =
    div
        viewstyles
        [ tableGen cellGen model styles.myshotsTableStyle ]


cellGen : Int -> Model -> Html Msg
cellGen idx model =
    let
        cellstate =
            case List.filter (\shot -> shot.idx == idx) model.p1shots of
                [] ->
                    Nothing

                head :: _ ->
                    Just head.result
    in
    case cellstate of
        Just shotresult ->
            -- there was a previous shot on this cell. format based on result
            case shotresult of
                Hit ->
                    td ( Helpers.listOfStringTuplesToStyle <| styles.tdstyle ++ styles.hitStyle ) [ text "hit" ]

                Miss ->
                    td ( Helpers.listOfStringTuplesToStyle <| styles.tdstyle ++ styles.missStyle ) [ text "miss" ]

        Nothing ->
            let
                -- if it's our turn, allow clicking on this cell
                attrs =
                    case model.playerTurn of
                        PlayerSide1 ->
                            [ onClickShip (TakeShot idx) ]

                        _ ->
                            []
            in
            td
                ( Helpers.listOfStringTuplesToStyle styles.tdstyle ++ attrs)
                [ text <| String.fromInt idx ]


onClickShip : msg -> Attribute msg
onClickShip message =
    onHelper "click" message


onHelper : String -> msg -> Attribute msg
onHelper eventName message =
    onWithOptions
        eventName
        { preventDefault = False
        , stopPropagation = False
        }
        (Decode.succeed message)
