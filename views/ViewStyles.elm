module ViewStyles exposing (..)

import Model exposing (..)

styles = {
    masterContainerStyle = [
        --("width","800px")
        ("margin","0 auto")
        ,("background","beige")
        ,("border","0")
        ,("padding","0")
        ]
    ,containerStyle = [
        ("border","0")
        ,("padding","0")
        ,("margin","0 auto")
--        ,("transform","rotateX(14deg)")
        ,("background","azure")
        ]
    ,myshotsTableStyle = [
        ("background","olive")
        ,("text-align","center")
        ]
    ,myshipsTableStyle = [
        ("text-align","center")
        ]
    ,tdstyle = [
        ("height","50px")
        ,("width","50px")
        ,("border","1px solid black")
        ]
    ,sankStyle = [
        ("opacity","0.5")
        ]
    ,occupiedStyle = [
        ("background-color","black")
        ]
    ,hitStyle = [
        --        ("width","50%")
        --        ,("height","50%")
        --        ,("margin","0 auto")
        ("background","red")
        ]
    ,missStyle = [
        --        ("width","50%")
        --        ,("height","50%")
        --        ,("margin","0 auto")
        ("background","blue")
        ]
    ,hitShotStyle = [
        --        ("width","50%")
        --        ,("height","50%")
        --        ,("margin","0 auto")
        ("background","red")
        ]
    ,dualViewStyle = [
        ("perspective","2000px")
        ,("padding","50px")
        ]
    ,sidebysideViewStyle = [
        ("margin", "5px")
        ,("display","inline-block")
        ]
    ,activeView = [
        ("border", "2px solid black")
        ,("margin", "3px")
        ]
    ,inactiveView = [
        ("opacity","0.5")
        ]
    ,gameoverDiv = [

        ]
    }



optionalStyle : Bool -> List (String,String) -> List (String,String)
optionalStyle bool styles =
    case bool of
        True -> styles
        False -> []


shotResultToStyle : ShotResult -> List (String,String)
shotResultToStyle shotResult =
    case shotResult of
        Hit -> styles.hitShotStyle
        Miss -> styles.missStyle


shotCellStyle : Int -> Model -> List (String,String)
shotCellStyle idx model  =
    case List.filter (\shot -> shot.idx == idx) model.p1shots of
        [] -> []
        head::_ -> shotResultToStyle head.result


