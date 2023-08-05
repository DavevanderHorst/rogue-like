module Models.LevelState exposing (..)

import Dict exposing (Dict)
import Models.CardState exposing (CardAbility)


type alias LevelState =
    { level : Level
    , gameMode : GameMode
    , formerClickedCell : Maybe MapCoordinate
    , clickedCard : Maybe Int
    , heroSpot : MapCoordinate
    , maybeHeroSpotClosedDoorNumber : Maybe Int

    --, animation : Animation
    }


type Animation
    = None
    | Moving


type alias Level =
    { rooms : Dict Int Room
    , tempUpdatedRooms : Maybe (Dict Int Room)
    , changedMapCoordinatesForTempRooms : Maybe (List MapCoordinate)
    , monsters : Dict Int MonsterDetails
    }


type alias Room =
    { roomNumber : Int
    , columns : Int
    , rows : Int
    , measurements : Measurements
    , gridCells : Dict String GridCell
    , isOpen : Bool
    }


type alias GridCell =
    { startX : Int
    , startY : Int
    , mapCoordinate : MapCoordinate
    , polygonShape : String
    , cellState : CellState
    }


type CellState
    = Empty
    | FigureType FigureType
    | ClickedForMovement Int
    | IsPartOfMovePath Int
    | CanBeMovedTo Int


type FigureType
    = Hero
    | Monster MonsterType Int


type MonsterType
    = Dummy


type alias Measurements =
    { width : Int
    , height : Int
    , startX : Int
    , startY : Int
    }


type GameMode
    = CardAction CardAbility
    | ChooseCard


type alias MapCoordinate =
    { roomNumber : Int
    , roomCoordinate : RoomCoordinate
    }


type alias RoomCoordinate =
    { columnNumber : Int
    , rowNumber : Int
    }


type alias MonsterDetails =
    { monsterNumber : Int
    , monsterType : MonsterType
    }


type GridDirection
    = Left
    | UpLeft
    | UpRight
    | Right
    | DownRight
    | DownLeft
