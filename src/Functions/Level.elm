module Functions.Level exposing (..)

import Dict exposing (Dict)
import Functions.DictFunctions.GridCellDict exposing (setGridCellFromMovableToClickedUnsafe, setGridCellFromPartOfPathToCanBeMovedTo)
import Functions.DictFunctions.RoomDict exposing (addRoomToRoomDictUnSafe, getRoomFromRoomDict, setGridCellsForRoomInRoomDictUnSafe)
import Functions.Movement exposing (setCanBeJumpedToForAbility, setCanBeMovedToForMoveAbility)
import Functions.Room exposing (addHeroToRoomUnsafe, removeHeroFromRoomUnsafe)
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


resetChangedCoordinatesInTempRoomDictForLevel : Level -> Result String Level
resetChangedCoordinatesInTempRoomDictForLevel level =
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


setCanBeJumpedToToIsClickedForLevelUnSafe : MapCoordinate -> Level -> Result String Level
setCanBeJumpedToToIsClickedForLevelUnSafe spot level =
    case level.tempUpdatedRooms of
        Nothing ->
            -- temp rooms should be set to make a path, cells need to be in state : CanBeMovedToo
            Err "Temp rooms are Nothing in : setCanBeMovedToToIsClickedForLevelUnSafe"

        Just tempRooms ->
            let
                getRoomResult =
                    getRoomFromRoomDict spot.roomNumber tempRooms
            in
            case getRoomResult of
                Err err ->
                    Err err

                Ok activeRoom ->
                    let
                        updatedGridCells =
                            -- we assume that grid cell exists and is in state clicked
                            setGridCellFromMovableToClickedUnsafe spot.roomCoordinate activeRoom.gridCells

                        updatedRoomDict =
                            setGridCellsForRoomInRoomDictUnSafe spot.roomNumber updatedGridCells tempRooms
                    in
                    Ok
                        { level
                            | tempUpdatedRooms = Just updatedRoomDict
                            , changedMapCoordinatesForTempRooms = Just [ spot ]
                        }


updateLevelForAbility : CardAbility -> MapCoordinate -> Level -> Result String Level
updateLevelForAbility ability heroSpot level =
    case ability of
        Move steps ->
            handleMovementAbility steps heroSpot level False

        Jump jump ->
            handleMovementAbility jump heroSpot level True

        Attack _ ->
            -- TODO
            Err "To be implemented"


handleMovementAbility : Int -> MapCoordinate -> Level -> Bool -> Result String Level
handleMovementAbility steps heroSpot level isJump =
    if steps == 0 then
        -- empty ability has zero steps
        Err "Movement is zero, something went wrong"

    else
        let
            movementFunction =
                if isJump then
                    setCanBeJumpedToForAbility

                else
                    setCanBeMovedToForMoveAbility

            updatedRoomsResult =
                movementFunction steps heroSpot level.rooms
        in
        case updatedRoomsResult of
            Ok updatedRoomDict ->
                Ok { level | tempUpdatedRooms = Just updatedRoomDict }

            Err err ->
                Err err
