module Functions.Animations exposing (..)

import Dict exposing (Dict)
import Functions.DictFunctions.RoomDict exposing (getGridCellFromRoomDict)
import Models.BaseModel exposing (AnimationType(..))
import Models.LevelState exposing (MapCoordinate, Room, RoomCoordinate)


makeMoveAnimation : MapCoordinate -> MapCoordinate -> Dict Int Room -> Result String AnimationType
makeMoveAnimation heroSpot nextSpot roomDict =
    let
        startGridCellResult =
            getGridCellFromRoomDict heroSpot roomDict
    in
    case startGridCellResult of
        Err err ->
            Err err

        Ok startGridCell ->
            let
                endGridCellResult =
                    getGridCellFromRoomDict nextSpot roomDict
            in
            case endGridCellResult of
                Err err ->
                    Err err

                Ok endGridCell ->
                    Ok (Walk startGridCell endGridCell)
