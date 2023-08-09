module Functions.Room exposing (..)

import Functions.DictFunctions.GridCellDict exposing (setHeroInGridCellDictUnSafe, setHeroToEmptyInGridCellDictUnSafe, setShapeInGridCellDictUnSafe)
import Models.LevelState exposing (MapCoordinate, Room, RoomCoordinate, RoomDoorDetails)


tryGetClosedDoorNumber : RoomCoordinate -> Room -> ( Bool, Int )
tryGetClosedDoorNumber roomCoordinate room =
    List.foldl (hasClosedDoor roomCoordinate) ( False, 0 ) room.roomDoors


hasClosedDoor : RoomCoordinate -> RoomDoorDetails -> ( Bool, Int ) -> ( Bool, Int )
hasClosedDoor roomCoordinate roomDoor ( hasDoor, doorNumber ) =
    if hasDoor then
        ( hasDoor, doorNumber )

    else if roomDoor.roomCoordinate == roomCoordinate then
        ( True, roomDoor.doorNumber )

    else
        ( hasDoor, doorNumber )


removeHeroFromRoomUnsafe : RoomCoordinate -> Room -> Room
removeHeroFromRoomUnsafe heroSpot room =
    let
        gridCellsWithoutHero =
            setHeroToEmptyInGridCellDictUnSafe heroSpot room.gridCells
    in
    { room | gridCells = gridCellsWithoutHero }


addHeroToRoomUnsafe : RoomCoordinate -> Room -> Room
addHeroToRoomUnsafe heroSpot room =
    let
        gridCellsWithHero =
            setHeroInGridCellDictUnSafe heroSpot room.gridCells
    in
    { room | gridCells = gridCellsWithHero }


setShapeForGridCellInRoom : String -> RoomCoordinate -> Room -> Room
setShapeForGridCellInRoom shape spot room =
    let
        gridCellsWithShape =
            setShapeInGridCellDictUnSafe shape spot room.gridCells
    in
    { room | gridCells = gridCellsWithShape }
