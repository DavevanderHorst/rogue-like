module Levels.LevelOne exposing (..)

import Constants
    exposing
        ( cellMargin
        , cellWidth
        , halfCellMargin
        , halfCellWidth
        , halfRoomPadding
        , horizontalGridPolygon
        , quarterCellWidth
        , roomPadding
        )
import Dict exposing (Dict)
import Functions.Basic exposing (isEven)
import Functions.Coordinates exposing (createMapCoordinateAlt, makeMapCoordinateList)
import Functions.DictFunctions.GridCellDict exposing (addGridCellTooGridCellDictUnSafe, getGridCellFromGridCellDict)
import Functions.DictFunctions.Level exposing (addMonstersAndRoomToLevelSafe)
import Levels.LevelCreationModels exposing (DoorDirection(..), FigureCreationType(..), MapCreationFigure)
import Models.LevelState
    exposing
        ( CellState(..)
        , FigureType(..)
        , GridCell
        , Level
        , MapCoordinate
        , Measurements
        , MonsterDetails
        , MonsterType(..)
        , Room
        , RoomCoordinate
        )


levelOneResult : Result String Level
levelOneResult =
    let
        createStartRoomResult =
            createStartRoom 10 10 roomOneFigures emptyLevel
    in
    case createStartRoomResult of
        Ok levelWithOneRoom ->
            Ok levelWithOneRoom

        Err err ->
            Err err


roomOneFigures : List MapCreationFigure
roomOneFigures =
    [ heroFigure
    , { figureType = DummyFigure
      , roomCoordinate = RoomCoordinate 4 4
      }
    , { figureType = DummyFigure
      , roomCoordinate = RoomCoordinate 5 4
      }
    , { figureType = DummyFigure
      , roomCoordinate = RoomCoordinate 6 4
      }
    ]


heroFigure : MapCreationFigure
heroFigure =
    { figureType = HeroFigure
    , roomCoordinate = heroStartSpot
    }


heroStartSpot : RoomCoordinate
heroStartSpot =
    RoomCoordinate 5 5


heroStartMapCoordinate : MapCoordinate
heroStartMapCoordinate =
    createMapCoordinateAlt 1 heroStartSpot


emptyLevel : Level
emptyLevel =
    Level Dict.empty Nothing Nothing Dict.empty


createStartRoom : Int -> Int -> List MapCreationFigure -> Level -> Result String Level
createStartRoom columns rows figures level =
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
            addMonstersAndRoomToLevelSafe monsterList roomWithFigures level

        Err err ->
            Err err


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
