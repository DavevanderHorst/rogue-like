module Functions.DictFunctions.RoomDict exposing (..)

import Dict exposing (Dict)
import Functions.DictFunctions.GridCellDict exposing (getStepsFromGridCellForClickedCell)
import Models.LevelState exposing (GridCell, MapCoordinate, Room, RoomCoordinate)


getRoomFromRoomDict : Int -> Dict Int Room -> Result String Room
getRoomFromRoomDict roomNumber roomDict =
    let
        maybeRoom =
            Dict.get roomNumber roomDict
    in
    case maybeRoom of
        Nothing ->
            Err ("No room with number " ++ String.fromInt roomNumber ++ " in this level yet.")

        Just room ->
            Ok room


setGridCellsForRoomInRoomDictUnSafe : Int -> Dict String GridCell -> Dict Int Room -> Dict Int Room
setGridCellsForRoomInRoomDictUnSafe roomNumber gridCells roomDict =
    updateRoomInRoomDictUnSafe roomNumber (setGridCells gridCells) roomDict


updateRoomInRoomDictUnSafe : Int -> (Maybe Room -> Maybe Room) -> Dict Int Room -> Dict Int Room
updateRoomInRoomDictUnSafe roomNumber updateFunction roomDict =
    -- Only use this when your 100% sure room already exists.
    -- If room does not exists, nothing happens.
    Dict.update roomNumber updateFunction roomDict


setGridCells : Dict String GridCell -> Maybe Room -> Maybe Room
setGridCells gridCells =
    Maybe.map
        (\room -> { room | gridCells = gridCells })


getStepsToMoveTowardsClickedCell : MapCoordinate -> Dict Int Room -> Result String Int
getStepsToMoveTowardsClickedCell spot roomDict =
    let
        getRoomResult =
            getRoomFromRoomDict spot.roomNumber roomDict
    in
    case getRoomResult of
        Err err ->
            Err err

        Ok activeRoom ->
            getStepsFromGridCellForClickedCell spot.roomCoordinate activeRoom.gridCells
