module Functions.LevelState exposing (..)

import Functions.DictFunctions.DoorDict exposing (addDoorToDoorDictUnSafe, getDoorFromDoorDict)
import Functions.DictFunctions.RoomDict exposing (openGridCellDoorsForOpenedDoor)
import Functions.Level exposing (removeHeroFromLevel, setHeroInLevel)
import Models.LevelState exposing (LevelState, MapCoordinate)


makeLevelStateReadyForMoveAnimation : LevelState -> Result String LevelState
makeLevelStateReadyForMoveAnimation levelState =
    -- Remove hero, hero will be animated
    -- We remove the temp rooms
    let
        levelWithoutHeroResult =
            removeHeroFromLevel levelState.heroSpot levelState.level
    in
    case levelWithoutHeroResult of
        Err err ->
            Err err

        Ok levelWithoutHero ->
            let
                readyLevel =
                    { levelWithoutHero | tempUpdatedRooms = Nothing }
            in
            Ok { levelState | level = readyLevel, formerClickedCell = Nothing }


openDoorInLevelState : Int -> LevelState -> Result String LevelState
openDoorInLevelState doorNumber state =
    -- open the door in the door dict
    -- open room in both temp and normal room dict.
    -- both connected cells from the door need to be opened also
    let
        oldLevel =
            state.level

        doorResult =
            getDoorFromDoorDict doorNumber oldLevel.doors
    in
    case doorResult of
        Err err ->
            Err err

        Ok door ->
            let
                openedDoor =
                    { door | isOpen = True }

                newDoorDict =
                    addDoorToDoorDictUnSafe openedDoor state.level.doors

                newRoomDictResult =
                    openGridCellDoorsForOpenedDoor openedDoor oldLevel.rooms
            in
            case newRoomDictResult of
                Err err ->
                    Err err

                Ok newRoomDict ->
                    let
                        updatedLevel =
                            { oldLevel | rooms = newRoomDict, doors = newDoorDict }
                    in
                    case oldLevel.tempUpdatedRooms of
                        Nothing ->
                            Ok { state | level = updatedLevel }

                        Just tempRooms ->
                            let
                                newTempRoomDictResult =
                                    openGridCellDoorsForOpenedDoor openedDoor tempRooms
                            in
                            case newTempRoomDictResult of
                                Err err ->
                                    Err err

                                Ok newTempRoomDict ->
                                    let
                                        newLevel =
                                            { updatedLevel | tempUpdatedRooms = Just newTempRoomDict }
                                    in
                                    Ok { state | level = newLevel }
