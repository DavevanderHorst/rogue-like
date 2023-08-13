module Levels.LevelOne exposing (..)

import Functions.Coordinates exposing (createMapCoordinate, createMapCoordinateAlt)
import Levels.CreateLevelFunctions exposing (createRoom, createStartRoomAndLevel)
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
            createStartRoomAndLevel 6 5 roomOneFigures
    in
    case createStartRoomResult of
        Ok levelWithOneRoom ->
            let
                roomNumberTwo =
                    2

                connectedRoomOneAndTwo =
                    { baseRoomNumber = 1
                    , baseRoomDoorRoomCoordinate = RoomCoordinate 1 5
                    , baseRoomDoorDirection = DoorDown
                    , connectedRoomDoorRoomCoordinate = RoomCoordinate 3 1
                    }
            in
            -- createRoomTwoResult =
            createRoom roomNumberTwo 3 7 roomTwoFigures connectedRoomOneAndTwo levelWithOneRoom

        --case createRoomTwoResult of
        --    Err err ->
        --        Err err
        --
        --    Ok levelWithTwoRooms ->
        --        let
        --            roomNumberThree =
        --                3
        --
        --            connectedRoomOneAndThree =
        --                { baseRoomNumber = 1
        --                , baseRoomDoorRoomCoordinate = RoomCoordinate 3 1
        --                , baseRoomDoorDirection = DoorUp
        --                , connectedRoomDoorRoomCoordinate = RoomCoordinate 3 5
        --                }
        --        in
        --        createRoom roomNumberThree 6 5 [] connectedRoomOneAndThree levelWithTwoRooms
        Err err ->
            Err err


roomOneFigures : List MapCreationFigure
roomOneFigures =
    [ heroFigure
    , { figureType = DummyFigure
      , roomCoordinate = RoomCoordinate 2 2
      }
    , { figureType = DummyFigure
      , roomCoordinate = RoomCoordinate 2 3
      }
    , { figureType = DummyFigure
      , roomCoordinate = RoomCoordinate 4 2
      }
    ]


roomTwoFigures : List MapCreationFigure
roomTwoFigures =
    [ { figureType = DummyFigure
      , roomCoordinate = RoomCoordinate 2 4
      }
    ]


heroFigure : MapCreationFigure
heroFigure =
    { figureType = HeroFigure
    , roomCoordinate = heroStartSpot
    }


heroStartSpot : RoomCoordinate
heroStartSpot =
    RoomCoordinate 4 1


heroStartMapCoordinate : MapCoordinate
heroStartMapCoordinate =
    createMapCoordinateAlt 1 heroStartSpot
