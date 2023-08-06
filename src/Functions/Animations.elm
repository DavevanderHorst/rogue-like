module Functions.Animations exposing (..)

import Dict exposing (Dict)
import Functions.DictFunctions.RoomDict exposing (getGridCellFromRoomDict)
import Models.BaseModel exposing (AnimationType(..))
import Models.LevelState exposing (GridCell, MapCoordinate, Room, RoomCoordinate)


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
                        makeGridCellList restOfPath roomDict
                in
                case gridCellListResult of
                    Err err ->
                        Err err

                    Ok gridCellList ->
                        Ok (Walk heroGridCell gridCellList)


makeGridCellList : List MapCoordinate -> Dict Int Room -> Result String (List GridCell)
makeGridCellList coordinatesList roomDict =
    List.foldr (getAndAddGridCell roomDict) (Ok []) coordinatesList


getAndAddGridCell : Dict Int Room -> MapCoordinate -> Result String (List GridCell) -> Result String (List GridCell)
getAndAddGridCell roomDict spot result =
    case result of
        Err err ->
            Err err

        Ok gridCellList ->
            let
                getGridCellResult =
                    getGridCellFromRoomDict spot roomDict
            in
            case getGridCellResult of
                Err err ->
                    Err err

                Ok gridCell ->
                    Ok (gridCell :: gridCellList)
