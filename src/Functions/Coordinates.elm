module Functions.Coordinates exposing (..)

import Functions.Basic exposing (isEven)
import Models.LevelState exposing (GridDirection(..), MapCoordinate, Room, RoomCoordinate)



-- Comparing


isSameMapCoordinateWithMaybe : MapCoordinate -> Maybe MapCoordinate -> Bool
isSameMapCoordinateWithMaybe mapCoordinate maybeMapCoordinate =
    case maybeMapCoordinate of
        Nothing ->
            False

        Just coordinate ->
            isSameMapCoordinate mapCoordinate coordinate


isSameMapCoordinate : MapCoordinate -> MapCoordinate -> Bool
isSameMapCoordinate mapCoordinateOne mapCoordinateTwo =
    if mapCoordinateOne.roomNumber == mapCoordinateTwo.roomNumber then
        isSameRoomCoordinate mapCoordinateOne.roomCoordinate mapCoordinateTwo.roomCoordinate

    else
        False


isSameRoomCoordinate : RoomCoordinate -> RoomCoordinate -> Bool
isSameRoomCoordinate roomCoordinateOne roomCoordinateTwo =
    roomCoordinateOne.columnNumber
        == roomCoordinateTwo.columnNumber
        && roomCoordinateOne.rowNumber
        == roomCoordinateTwo.rowNumber



-- for adding all coordinates to a list


makeMapCoordinateList : Room -> List MapCoordinate
makeMapCoordinateList room =
    List.foldl (addCoordsToListForRow room) [] (List.range 1 room.rows)


addCoordsToListForRow : Room -> Int -> List MapCoordinate -> List MapCoordinate
addCoordsToListForRow room rowNumber gridList =
    let
        columnNumberList =
            if isEven rowNumber then
                List.range 1 (room.columns - 1)

            else
                List.range 1 room.columns
    in
    List.foldl (addCoordinateToList rowNumber room.roomNumber) gridList columnNumberList


addCoordinateToList : Int -> Int -> Int -> List MapCoordinate -> List MapCoordinate
addCoordinateToList rowNumber roomNumber columnNumber list =
    { roomNumber = roomNumber, roomCoordinate = { columnNumber = columnNumber, rowNumber = rowNumber } } :: list



-- To string methods


roomCoordinateToString : RoomCoordinate -> String
roomCoordinateToString coordinate =
    "(" ++ String.fromInt coordinate.columnNumber ++ "," ++ String.fromInt coordinate.rowNumber ++ ")"



-- create coordinates methods


createMapCoordinate : Int -> Int -> Int -> MapCoordinate
createMapCoordinate roomNumber columnNumber rowNumber =
    MapCoordinate roomNumber (RoomCoordinate columnNumber rowNumber)


createMapCoordinateAlt : Int -> RoomCoordinate -> MapCoordinate
createMapCoordinateAlt roomNumber roomCoordinate =
    MapCoordinate roomNumber roomCoordinate



-- for fast changing room coordinates


goUp : RoomCoordinate -> RoomCoordinate
goUp coordinate =
    { coordinate | rowNumber = coordinate.rowNumber - 1 }


goUpRight : RoomCoordinate -> RoomCoordinate
goUpRight coordinate =
    { coordinate | rowNumber = coordinate.rowNumber - 1, columnNumber = coordinate.columnNumber + 1 }


goRight : RoomCoordinate -> RoomCoordinate
goRight coordinate =
    { coordinate | columnNumber = coordinate.columnNumber + 1 }


goDownRight : RoomCoordinate -> RoomCoordinate
goDownRight coordinate =
    { coordinate | rowNumber = coordinate.rowNumber + 1, columnNumber = coordinate.columnNumber + 1 }


goDown : RoomCoordinate -> RoomCoordinate
goDown coordinate =
    { coordinate | rowNumber = coordinate.rowNumber + 1 }


goDownLeft : RoomCoordinate -> RoomCoordinate
goDownLeft coordinate =
    { coordinate | rowNumber = coordinate.rowNumber + 1, columnNumber = coordinate.columnNumber - 1 }


goLeft : RoomCoordinate -> RoomCoordinate
goLeft coordinate =
    { coordinate | columnNumber = coordinate.columnNumber - 1 }


goUpLeft : RoomCoordinate -> RoomCoordinate
goUpLeft coordinate =
    { coordinate | rowNumber = coordinate.rowNumber - 1, columnNumber = coordinate.columnNumber - 1 }


getNextCoordinate : GridDirection -> RoomCoordinate -> RoomCoordinate
getNextCoordinate direction current =
    let
        isEvenRow =
            isEven current.rowNumber
    in
    case direction of
        Left ->
            goLeft current

        UpLeft ->
            if isEvenRow then
                goUp current

            else
                goUpLeft current

        UpRight ->
            if isEvenRow then
                goUpRight current

            else
                goUp current

        Right ->
            goRight current

        DownRight ->
            if isEvenRow then
                goDownRight current

            else
                goDown current

        DownLeft ->
            if isEvenRow then
                goDown current

            else
                goDownLeft current
