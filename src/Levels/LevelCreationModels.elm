module Levels.LevelCreationModels exposing (..)

import Models.LevelState exposing (RoomCoordinate)


type alias MapCreationFigure =
    { figureType : FigureCreationType
    , roomCoordinate : RoomCoordinate
    }


type FigureCreationType
    = HeroFigure
    | DummyFigure


type alias DoorData =
    { baseRoomNumber : Int
    , baseRoomDoorRoomCoordinate : RoomCoordinate
    , baseRoomDoorDirection : DoorDirection
    , connectedRoomDoorRoomCoordinate : RoomCoordinate
    }


type DoorDirection
    = DoorUp
    | DoorDown
    | DoorLeft
    | DoorRight
