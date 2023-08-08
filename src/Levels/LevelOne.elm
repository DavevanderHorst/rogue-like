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
            createStartRoomAndLevel 6 4 roomOneFigures
    in
    case createStartRoomResult of
        Ok levelWithOneRoom ->
            let
                nextRoomNumber =
                    2

                connectedRoomOneAndTwo =
                    { baseRoomNumber = 1
                    , baseRoomDoorRoomCoordinate = RoomCoordinate 6 1
                    , baseRoomDoorDirection = DoorRight
                    , connectedRoomDoorRoomCoordinate = RoomCoordinate 1 7
                    }
            in
            createRoom nextRoomNumber 3 7 [] connectedRoomOneAndTwo levelWithOneRoom

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


heroFigure : MapCreationFigure
heroFigure =
    { figureType = HeroFigure
    , roomCoordinate = heroStartSpot
    }


heroStartSpot : RoomCoordinate
heroStartSpot =
    RoomCoordinate 6 1


heroStartMapCoordinate : MapCoordinate
heroStartMapCoordinate =
    createMapCoordinateAlt 1 heroStartSpot
