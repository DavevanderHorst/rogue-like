module Functions.Door exposing (..)

import Models.LevelState exposing (Door, MapCoordinate)


getOtherMapCoordinateFromDoor : Int -> Door -> MapCoordinate
getOtherMapCoordinateFromDoor roomNumber door =
    if door.connectedMapCoordinateOne.roomNumber == roomNumber then
        door.connectedMapCoordinateTwo

    else
        door.connectedMapCoordinateOne
