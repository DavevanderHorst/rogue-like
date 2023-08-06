module Functions.DictFunctions.RoomDict exposing (..)

import Dict exposing (Dict)
import Functions.DictFunctions.GridCellDict exposing (getGridCellFromGridCellDict, getStepsFromGridCellForClickedCell)
import Models.LevelState exposing (GridCell, Level, MapCoordinate, Room, RoomCoordinate)


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


getGridCellFromRoomDict : MapCoordinate -> Dict Int Room -> Result String GridCell
getGridCellFromRoomDict spot roomDict =
    let
        getRoomResult =
            getRoomFromRoomDict spot.roomNumber roomDict
    in
    case getRoomResult of
        Err err ->
            Err err

        Ok room ->
            getGridCellFromGridCellDict spot.roomCoordinate room.gridCells


addRoomToRoomDictUnSafe : Room -> Dict Int Room -> Dict Int Room
addRoomToRoomDictUnSafe room roomDict =
    Dict.insert room.roomNumber room roomDict


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


getStepsToMoveTowardsClickedCell : MapCoordinate -> Level -> Result String Int
getStepsToMoveTowardsClickedCell spot level =
    case level.tempUpdatedRooms of
        Nothing ->
            Err "Cant get steps to move, cause temp rooms are empty"

        Just tempRooms ->
            let
                getRoomResult =
                    getRoomFromRoomDict spot.roomNumber tempRooms
            in
            case getRoomResult of
                Err err ->
                    Err err

                Ok activeRoom ->
                    getStepsFromGridCellForClickedCell spot.roomCoordinate activeRoom.gridCells
