module Functions.Animations exposing (..)

import Dict exposing (Dict)
import Functions.Basic exposing (makePointBetweenPoints)
import Functions.DictFunctions.RoomDict exposing (getGridCellFromRoomDict)
import Models.BaseModel exposing (AnimationType(..))
import Models.LevelState exposing (GridCell, MapCoordinate, Room, RoomCoordinate)
import Models.Others exposing (Point)


makeJumpAnimation : MapCoordinate -> MapCoordinate -> Dict Int Room -> Result String AnimationType
makeJumpAnimation start end roomDict =
    let
        startGridCellResult =
            getGridCellFromRoomDict start roomDict
    in
    case startGridCellResult of
        Err err ->
            Err err

        Ok startGridCell ->
            let
                endGridCellResult =
                    getGridCellFromRoomDict end roomDict
            in
            case endGridCellResult of
                Err err ->
                    Err err

                Ok endGridCell ->
                    let
                        startPoint =
                            makePointFromGridCell startGridCell

                        endPoint =
                            makePointFromGridCell endGridCell

                        middlePoint =
                            makePointBetweenPoints startPoint endPoint
                    in
                    Ok (AnimationJump startPoint middlePoint endPoint)


makePointFromGridCell : GridCell -> Point
makePointFromGridCell gridCell =
    { x = toFloat gridCell.startX, y = toFloat gridCell.startY }


makeMoveAnimation : MapCoordinate -> List MapCoordinate -> Dict Int Room -> Result String AnimationType
makeMoveAnimation heroSpot restOfPath roomDict =
    if List.isEmpty restOfPath then
        Err "We had a changed cell list, but there was nothing in it for move animation"

    else
        let
            heroGridCellResult =
                getGridCellFromRoomDict heroSpot roomDict
        in
        case heroGridCellResult of
            Err err ->
                Err err

            Ok heroGridCell ->
                let
                    gridCellListResult =
                        makePointList restOfPath roomDict
                in
                case gridCellListResult of
                    Err err ->
                        Err err

                    Ok pointList ->
                        Ok (AnimationMove (makePointFromGridCell heroGridCell) pointList)


makePointList : List MapCoordinate -> Dict Int Room -> Result String (List Point)
makePointList coordinatesList roomDict =
    List.foldr (getGridCellAndMakePoint roomDict) (Ok []) coordinatesList


getGridCellAndMakePoint : Dict Int Room -> MapCoordinate -> Result String (List Point) -> Result String (List Point)
getGridCellAndMakePoint roomDict spot result =
    case result of
        Err err ->
            Err err

        Ok pointList ->
            let
                getGridCellResult =
                    getGridCellFromRoomDict spot roomDict
            in
            case getGridCellResult of
                Err err ->
                    Err err

                Ok gridCell ->
                    Ok (makePointFromGridCell gridCell :: pointList)
