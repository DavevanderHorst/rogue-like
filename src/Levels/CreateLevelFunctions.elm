module Levels.CreateLevelFunctions exposing (..)

import Constants
    exposing
        ( cellMargin
        , cellWidth
        , halfCellMargin
        , halfCellWidth
        , halfRoomPadding
        , horizontalGridPolygon
        , horizontalGridPolygonWithDoorDown
        , horizontalGridPolygonWithDoorUp
        , oneEightCellWidth
        , polygonDoorHeight
        , polygonDoorWidth
        , quarterCellWidth
        , rectDoorHeight
        , rectDoorWidth
        , roomPadding
        )
import Dict exposing (Dict)
import Functions.Basic exposing (isEven)
import Functions.Coordinates exposing (createMapCoordinateAlt, isSameRoomCoordinate, makeMapCoordinateList)
import Functions.DictFunctions.GridCellDict exposing (addGridCellTooGridCellDictUnSafe, getGridCellFromGridCellDict)
import Functions.DictFunctions.Level exposing (addMonstersAndRoomToLevelSafe)
import Functions.DictFunctions.RoomDict exposing (getRoomFromRoomDict)
import Levels.LevelCreationModels exposing (DoorData, DoorDirection(..), FigureCreationType(..), MapCreationFigure)
import Models.LevelState
    exposing
        ( CellState(..)
        , Door
        , FigureType(..)
        , GridCell
        , GridDoorDetails
        , Level
        , MapCoordinate
        , Measurements
        , MonsterDetails
        , MonsterType(..)
        , Room
        , RoomCoordinate
        )


emptyLevel : Level
emptyLevel =
    Level Dict.empty Nothing Nothing Dict.empty Dict.empty


createStartRoomAndLevel : Int -> Int -> List MapCreationFigure -> Result String Level
createStartRoomAndLevel columns rows figures =
    let
        roomMeasurements =
            createBaseMeasurementsForRoom columns rows

        newRoom =
            Room 1 columns rows roomMeasurements Dict.empty True

        readyRoomOne =
            generateGridCellsForRoom newRoom
    in
    let
        createAndAddFiguresResult =
            createAndAddFiguresToRoom figures readyRoomOne 1
    in
    case createAndAddFiguresResult of
        Ok ( roomWithFigures, monsterList ) ->
            addMonstersAndRoomToLevelSafe monsterList roomWithFigures emptyLevel

        Err err ->
            Err err


createRoom : Int -> Int -> Int -> List MapCreationFigure -> DoorData -> Level -> Result String Level
createRoom roomNumber columns rows monsters doorData level =
    let
        roomMeasurements =
            createBaseMeasurementsForRoom columns rows

        newRoom =
            Room roomNumber columns rows roomMeasurements Dict.empty False

        getRoomFromLevelResult =
            getRoomFromRoomDict doorData.baseRoomNumber level.rooms
    in
    case getRoomFromLevelResult of
        Err err ->
            Err err

        Ok baseRoom ->
            let
                xyResult =
                    case doorData.baseRoomDoorDirection of
                        DoorRight ->
                            Ok ( baseRoom.measurements.startX + baseRoom.measurements.width, calculateBaseY doorData baseRoom )

                        DoorLeft ->
                            Ok ( baseRoom.measurements.startX - roomMeasurements.width, calculateBaseY doorData baseRoom )

                        DoorUp ->
                            Ok ( calculateBaseX doorData baseRoom, baseRoom.measurements.startY - roomMeasurements.height )

                        DoorDown ->
                            Ok ( calculateBaseX doorData baseRoom, baseRoom.measurements.startY + baseRoom.measurements.height )
            in
            case xyResult of
                Err err ->
                    Err err

                Ok ( x, y ) ->
                    let
                        updatedMeasurements =
                            { roomMeasurements | startX = x, startY = y }

                        updatedRoom =
                            { newRoom | measurements = updatedMeasurements }

                        newRoomWithGridCells =
                            generateGridCellsForRoom updatedRoom

                        doorNumber =
                            Dict.size level.doors + 1

                        updateRoomsAndGenerateDoorResult =
                            updateRoomsAndGenerateDoor doorData baseRoom newRoomWithGridCells doorNumber
                    in
                    case updateRoomsAndGenerateDoorResult of
                        Ok ( baseRoomWithDoor, newRoomWithDoor, newDoor ) ->
                            let
                                firstMonsterNumber =
                                    Dict.size level.monsters + 1

                                createAndAddMonstersResult =
                                    createAndAddFiguresToRoom monsters newRoomWithDoor firstMonsterNumber
                            in
                            case createAndAddMonstersResult of
                                Ok ( newRoomWithMonsters, monsterList ) ->
                                    Ok (addAllToLevel baseRoomWithDoor newRoomWithMonsters newDoor monsterList level)

                                Err err ->
                                    Err err

                        Err err ->
                            Err err


addAllToLevel : Room -> Room -> Door -> List MonsterDetails -> Level -> Level
addAllToLevel room1 room2 door monsters level =
    let
        monstersAdded =
            List.foldl addMonsterToLevel level monsters

        roomsAdded =
            List.foldl addRoomToLevel monstersAdded [ room1, room2 ]
    in
    addDoorToLevel door roomsAdded


addMonsterToLevel : MonsterDetails -> Level -> Level
addMonsterToLevel monster level =
    { level | monsters = Dict.insert monster.monsterNumber monster level.monsters }


addRoomToLevel : Room -> Level -> Level
addRoomToLevel room level =
    { level | rooms = Dict.insert room.roomNumber room level.rooms }


addDoorToLevel : Door -> Level -> Level
addDoorToLevel door level =
    { level | doors = Dict.insert door.doorNumber door level.doors }


updateRoomsAndGenerateDoor : DoorData -> Room -> Room -> Int -> Result String ( Room, Room, Door )
updateRoomsAndGenerateDoor doorData baseRoom connectedRoom doorNumber =
    let
        checkDoorDataResult =
            checkDoorData doorData baseRoom connectedRoom
    in
    case checkDoorDataResult of
        Err err ->
            Err err

        Ok _ ->
            let
                baseRoomDoorMapCoordinate =
                    createMapCoordinateAlt baseRoom.roomNumber doorData.baseRoomDoorRoomCoordinate

                connectedRoomDoorMapCoordinate =
                    createMapCoordinateAlt connectedRoom.roomNumber doorData.connectedRoomDoorRoomCoordinate
            in
            case doorData.baseRoomDoorDirection of
                DoorRight ->
                    let
                        rightDoor =
                            Door
                                doorNumber
                                False
                                baseRoomDoorMapCoordinate
                                connectedRoomDoorMapCoordinate
                                (Measurements
                                    rectDoorWidth
                                    rectDoorHeight
                                    (baseRoom.measurements.startX + baseRoom.measurements.width - halfRoomPadding + cellMargin)
                                    (calculateDoorY doorData baseRoom + halfRoomPadding + quarterCellWidth)
                                )
                    in
                    Ok
                        ( updateRoomWithGridCellDoor doorNumber baseRoom doorData.baseRoomDoorRoomCoordinate connectedRoomDoorMapCoordinate
                        , updateRoomWithGridCellDoor doorNumber connectedRoom doorData.connectedRoomDoorRoomCoordinate baseRoomDoorMapCoordinate
                        , rightDoor
                        )

                DoorLeft ->
                    let
                        leftDoor =
                            Door
                                doorNumber
                                False
                                baseRoomDoorMapCoordinate
                                connectedRoomDoorMapCoordinate
                                (Measurements
                                    rectDoorWidth
                                    rectDoorHeight
                                    (baseRoom.measurements.startX - halfRoomPadding + cellMargin)
                                    (calculateDoorY doorData baseRoom + halfRoomPadding + quarterCellWidth)
                                )
                    in
                    Ok
                        ( updateRoomWithGridCellDoor doorNumber baseRoom doorData.baseRoomDoorRoomCoordinate connectedRoomDoorMapCoordinate
                        , updateRoomWithGridCellDoor doorNumber connectedRoom doorData.connectedRoomDoorRoomCoordinate baseRoomDoorMapCoordinate
                        , leftDoor
                        )

                DoorUp ->
                    let
                        upDoor =
                            Door
                                doorNumber
                                False
                                baseRoomDoorMapCoordinate
                                connectedRoomDoorMapCoordinate
                                (Measurements
                                    polygonDoorWidth
                                    polygonDoorHeight
                                    (calculateBaseX doorData baseRoom + halfRoomPadding + quarterCellWidth)
                                    (baseRoom.measurements.startY - halfRoomPadding + cellMargin)
                                )
                    in
                    Ok
                        ( updateRoomWithGridCellDoorAndPolygonShape doorNumber baseRoom doorData.baseRoomDoorRoomCoordinate horizontalGridPolygonWithDoorUp connectedRoomDoorMapCoordinate
                        , updateRoomWithGridCellDoorAndPolygonShape doorNumber connectedRoom doorData.connectedRoomDoorRoomCoordinate horizontalGridPolygonWithDoorDown baseRoomDoorMapCoordinate
                        , upDoor
                        )

                DoorDown ->
                    let
                        downDoor =
                            Door
                                doorNumber
                                False
                                baseRoomDoorMapCoordinate
                                connectedRoomDoorMapCoordinate
                                (Measurements
                                    polygonDoorWidth
                                    polygonDoorHeight
                                    (calculateBaseX doorData baseRoom + halfRoomPadding + quarterCellWidth)
                                    (baseRoom.measurements.startY
                                        + baseRoom.measurements.height
                                        - halfRoomPadding
                                        - quarterCellWidth
                                        + cellMargin
                                        + oneEightCellWidth
                                    )
                                )
                    in
                    Ok
                        ( updateRoomWithGridCellDoorAndPolygonShape doorNumber baseRoom doorData.baseRoomDoorRoomCoordinate horizontalGridPolygonWithDoorDown connectedRoomDoorMapCoordinate
                        , updateRoomWithGridCellDoorAndPolygonShape doorNumber connectedRoom doorData.connectedRoomDoorRoomCoordinate horizontalGridPolygonWithDoorUp baseRoomDoorMapCoordinate
                        , downDoor
                        )


updateRoomWithGridCellDoor : Int -> Room -> RoomCoordinate -> MapCoordinate -> Room
updateRoomWithGridCellDoor doorNumber room roomDoorMapCoordinate connectedMapCoordinate =
    let
        updatedGridCells =
            Dict.map (setGridCellDoor roomDoorMapCoordinate doorNumber Nothing connectedMapCoordinate) room.gridCells
    in
    { room | gridCells = updatedGridCells }


updateRoomWithGridCellDoorAndPolygonShape : Int -> Room -> RoomCoordinate -> String -> MapCoordinate -> Room
updateRoomWithGridCellDoorAndPolygonShape doorNumber room roomDoorMapCoordinate polygonShape connectedMapCoordinate =
    let
        updatedGridCells =
            Dict.map (setGridCellDoor roomDoorMapCoordinate doorNumber (Just polygonShape) connectedMapCoordinate) room.gridCells
    in
    { room | gridCells = updatedGridCells }


setGridCellDoor : RoomCoordinate -> Int -> Maybe String -> MapCoordinate -> String -> GridCell -> GridCell
setGridCellDoor roomCoordinate doorNumber maybeShape connectedMapCoordinate _ value =
    if isSameRoomCoordinate roomCoordinate value.mapCoordinate.roomCoordinate then
        let
            updatedGridCell =
                { value | maybeGridDoorDetails = Just (GridDoorDetails doorNumber False connectedMapCoordinate) }
        in
        case maybeShape of
            Nothing ->
                updatedGridCell

            Just shape ->
                { updatedGridCell | polygonShape = shape }

    else
        value


calculateDoorY : DoorData -> Room -> Int
calculateDoorY doorData baseRoom =
    let
        baseY =
            calculateBaseY doorData baseRoom

        heightPerRow =
            cellWidth - quarterCellWidth + cellMargin
    in
    baseY + (heightPerRow * (doorData.connectedRoomDoorRoomCoordinate.rowNumber - 1))


calculateBaseX : DoorData -> Room -> Int
calculateBaseX doorData baseRoom =
    let
        columnDif =
            doorData.baseRoomDoorRoomCoordinate.columnNumber - doorData.connectedRoomDoorRoomCoordinate.columnNumber
    in
    -- Base x - dif in rows * cellwidth - shifting
    baseRoom.measurements.startX + (columnDif * (cellWidth + cellMargin))


calculateBaseY : DoorData -> Room -> Int
calculateBaseY doorData baseRoom =
    let
        rowDif =
            doorData.baseRoomDoorRoomCoordinate.rowNumber - doorData.connectedRoomDoorRoomCoordinate.rowNumber
    in
    -- Base y - dif in rows * cellWidth - shifting
    baseRoom.measurements.startY + (rowDif * (cellWidth + cellMargin - quarterCellWidth))


checkDoorData : DoorData -> Room -> Room -> Result String DoorData
checkDoorData data baseRoom connectedRoom =
    -- TODO
    Ok data


createBaseMeasurementsForRoom : Int -> Int -> Measurements
createBaseMeasurementsForRoom columns rows =
    let
        ( stepsX, stepsY ) =
            ( columns - 1, rows - 1 )

        rectColumnWidth =
            -- the rect width for all the columns, including margin between grid cells + padding for both sides
            columns * cellWidth + stepsX * cellMargin + roomPadding

        rectRowHeight =
            -- the rect height for all the rows, including margin between grid cells + padding for both sides
            -- - the shifting off the grid cells
            rows * cellWidth + stepsY * cellMargin + roomPadding - stepsY * quarterCellWidth
    in
    { width = rectColumnWidth
    , height = rectRowHeight
    , startX = 0
    , startY = 0
    }


generateGridCellsForRoom : Room -> Room
generateGridCellsForRoom room =
    let
        gridCells =
            List.foldl (generateGridCell room.measurements) Dict.empty (makeMapCoordinateList room)
    in
    { room | gridCells = gridCells }


generateGridCell : Measurements -> MapCoordinate -> Dict String GridCell -> Dict String GridCell
generateGridCell measurements mapCoordinate gridCellDict =
    let
        ( columnNumber, rowNumber ) =
            ( mapCoordinate.roomCoordinate.columnNumber, mapCoordinate.roomCoordinate.rowNumber )

        ( stepsX, stepsY ) =
            ( columnNumber - 1, rowNumber - 1 )

        baseX =
            -- start + cellMargin + number of column that went before + padding from the base rect
            measurements.startX + (cellWidth + cellMargin) * stepsX + halfRoomPadding

        gridX =
            if isEven rowNumber then
                -- the even rows we need to shift right, so that they fall nice together
                baseX + halfCellWidth + halfCellMargin

            else
                baseX

        baseY =
            -- start + number of rows that went before + padding from the base rect
            measurements.startY + (cellWidth + cellMargin) * stepsY + halfRoomPadding

        gridY =
            -- for every row after first, we need to move them up, so that they fall nice together
            baseY - (stepsY * quarterCellWidth)

        gridCell : GridCell
        gridCell =
            { startX = gridX
            , startY = gridY
            , mapCoordinate = mapCoordinate
            , polygonShape = horizontalGridPolygon
            , maybeGridDoorDetails = Nothing
            , cellState = Empty
            }
    in
    addGridCellTooGridCellDictUnSafe gridCell gridCellDict


createAndAddFiguresToRoom : List MapCreationFigure -> Room -> Int -> Result String ( Room, List MonsterDetails )
createAndAddFiguresToRoom figures room firstMonsterNumber =
    List.foldl (createAndAddFigureToRoom firstMonsterNumber) (Ok ( room, [] )) figures


createAndAddFigureToRoom : Int -> MapCreationFigure -> Result String ( Room, List MonsterDetails ) -> Result String ( Room, List MonsterDetails )
createAndAddFigureToRoom firstMonsterNumber figure result =
    case result of
        Err err ->
            Err err

        Ok ( room, monsterList ) ->
            let
                monsterNumber =
                    List.length monsterList + firstMonsterNumber

                addMonsterToRoomResult =
                    addFigureToRoom monsterNumber figure room
            in
            case addMonsterToRoomResult of
                Err err ->
                    Err err

                Ok ( roomWithFigure, maybeMonster ) ->
                    let
                        newMonsterList =
                            case maybeMonster of
                                Nothing ->
                                    monsterList

                                Just newMonster ->
                                    newMonster :: monsterList
                    in
                    Ok ( roomWithFigure, newMonsterList )


addFigureToRoom : Int -> MapCreationFigure -> Room -> Result String ( Room, Maybe MonsterDetails )
addFigureToRoom monsterNumber figure room =
    let
        getGridCellResult =
            getGridCellFromGridCellDict figure.roomCoordinate room.gridCells
    in
    case getGridCellResult of
        Err err ->
            Err err

        Ok gridCell ->
            case gridCell.cellState of
                Empty ->
                    let
                        ( gridCellWithFigure, maybeNewMonster ) =
                            case figure.figureType of
                                HeroFigure ->
                                    ( { gridCell | cellState = FigureType Hero }, Nothing )

                                DummyFigure ->
                                    ( { gridCell | cellState = FigureType (Monster Dummy monsterNumber) }
                                    , Just (MonsterDetails monsterNumber Dummy)
                                    )
                    in
                    Ok ( { room | gridCells = addGridCellTooGridCellDictUnSafe gridCellWithFigure room.gridCells }, maybeNewMonster )

                ClickedForMovement _ ->
                    Err "GridCell is in state : 'Clicked' in addFigureToRoom"

                CanBeMovedTo _ ->
                    Err "Grid cell is in state : 'CanBeMovedToo' in addFigureToRoom"

                FigureType figureType ->
                    Err ("There is already an figure : " ++ figureTypeToString figureType ++ " in addFigureToRoom")

                IsPartOfMovePath _ ->
                    Err "GridCell is in state : 'IsPartOfMovePath' in addFigureToRoom"


figureTypeToString : FigureType -> String
figureTypeToString figure =
    case figure of
        Hero ->
            "Hero"

        Monster monsterType number ->
            case monsterType of
                Dummy ->
                    "Dummy, with number " ++ String.fromInt number
