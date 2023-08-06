module Functions.Level exposing (..)

import Dict exposing (Dict)
import Functions.DictFunctions.GridCellDict exposing (setGridCellFromPartOfPathToCanBeMovedTo)
import Functions.DictFunctions.RoomDict exposing (addRoomToRoomDictUnSafe, getRoomFromRoomDict, setGridCellsForRoomInRoomDictUnSafe)
import Functions.Room exposing (addHeroToRoomUnsafe, removeHeroFromRoomUnsafe, updateRoomsForCanBeMovedTo)
import Models.CardState exposing (CardAbility(..))
import Models.LevelState exposing (Level, MapCoordinate, Room, RoomCoordinate)


removeHeroFromLevel : MapCoordinate -> Level -> Result String Level
removeHeroFromLevel heroSpot level =
    changeRoomInLevel heroSpot level removeHeroFromRoomUnsafe


setHeroInLevel : MapCoordinate -> Level -> Result String Level
setHeroInLevel heroSpot level =
    changeRoomInLevel heroSpot level addHeroToRoomUnsafe


changeRoomInLevel : MapCoordinate -> Level -> (RoomCoordinate -> Room -> Room) -> Result String Level
changeRoomInLevel heroSpot level updateFunction =
    let
        roomDict =
            level.rooms

        getHeroRoomResult =
            getRoomFromRoomDict heroSpot.roomNumber roomDict
    in
    case getHeroRoomResult of
        Err err ->
            Err err

        Ok heroRoom ->
            let
                updatedRoom =
                    updateFunction heroSpot.roomCoordinate heroRoom

                updatedRoomDict =
                    addRoomToRoomDictUnSafe updatedRoom roomDict
            in
            Ok { level | rooms = updatedRoomDict }


resetMovementPathInTempRoomDictForLevel : Level -> Result String Level
resetMovementPathInTempRoomDictForLevel level =
    case level.tempUpdatedRooms of
        Nothing ->
            Err "No temp rooms found in level for : resetMovementPathInTempRoomDictForLevel"

        Just tempRoomDict ->
            case level.changedMapCoordinatesForTempRooms of
                Nothing ->
                    Err "No changed map coordinates found in level for : resetMovementPathInTempRoomDictForLevel"

                Just changedMapCoordinates ->
                    let
                        updatedTempRoomDictResult =
                            List.foldl setCoordinatesToCanBeMovedToo (Ok tempRoomDict) changedMapCoordinates
                    in
                    case updatedTempRoomDictResult of
                        Err err ->
                            Err err

                        Ok updatedTempRoomDict ->
                            Ok { level | tempUpdatedRooms = Just updatedTempRoomDict }


setCoordinatesToCanBeMovedToo : MapCoordinate -> Result String (Dict Int Room) -> Result String (Dict Int Room)
setCoordinatesToCanBeMovedToo spot roomDictResult =
    case roomDictResult of
        Err err ->
            Err err

        Ok roomDict ->
            let
                getRoomResult =
                    getRoomFromRoomDict spot.roomNumber roomDict
            in
            case getRoomResult of
                Err err ->
                    Err err

                Ok activeRoom ->
                    let
                        updatedGridCells =
                            setGridCellFromPartOfPathToCanBeMovedTo spot.roomCoordinate activeRoom.gridCells
                    in
                    Ok (setGridCellsForRoomInRoomDictUnSafe spot.roomNumber updatedGridCells roomDict)


updateLevelForAbility : CardAbility -> MapCoordinate -> Level -> Result String Level
updateLevelForAbility ability heroSpot level =
    case ability of
        Move steps ->
            if steps == 0 then
                -- empty ability has zero steps
                Err "Steps is zero, something went wrong"

            else
                let
                    updatedRoomsResult =
                        updateRoomsForCanBeMovedTo level.rooms steps heroSpot
                in
                case updatedRoomsResult of
                    Ok updatedRoomDict ->
                        Ok { level | tempUpdatedRooms = Just updatedRoomDict }

                    Err err ->
                        Err err

        Attack _ ->
            -- TODO
            Err "To be implemented"
