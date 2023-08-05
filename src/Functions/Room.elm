module Functions.Room exposing (..)

import Dict exposing (Dict)
import Functions.DictFunctions.GridCellDict exposing (setHeroToEmptyInGridCellDictUnSafe)
import Functions.DictFunctions.RoomDict exposing (getRoomFromRoomDict, setGridCellsForRoomInRoomDictUnSafe)
import Functions.Movement exposing (setCanBeMovedTooForMovement)
import Models.LevelState exposing (MapCoordinate, Room, RoomCoordinate)


updateRoomsForCanBeMovedTo : Dict Int Room -> Int -> MapCoordinate -> Result String (Dict Int Room)
updateRoomsForCanBeMovedTo roomDict steps heroSpot =
    let
        getRoomResult =
            getRoomFromRoomDict heroSpot.roomNumber roomDict
    in
    case getRoomResult of
        Err err ->
            Err err

        Ok room ->
            let
                newGridCells =
                    setCanBeMovedTooForMovement steps heroSpot.roomCoordinate room.gridCells
            in
            Ok (setGridCellsForRoomInRoomDictUnSafe heroSpot.roomNumber newGridCells roomDict)


removeHeroFromRoomUnsafe : RoomCoordinate -> Room -> Room
removeHeroFromRoomUnsafe heroSpot room =
    let
        gridCellsWithoutHero =
            setHeroToEmptyInGridCellDictUnSafe heroSpot room.gridCells
    in
    { room | gridCells = gridCellsWithoutHero }
