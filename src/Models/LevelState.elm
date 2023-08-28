module Models.LevelState exposing (..)

import Dict exposing (Dict)
import Models.CardState exposing (CardAbility)


type alias LevelState =
    { level : Level
    , gameMode : GameMode
    , formerClickedCell : Maybe MapCoordinate
    , maybeClickedCard : Maybe Int
    , heroSpot : MapCoordinate
    }


type alias Level =
    { rooms : Dict Int Room
    , tempUpdatedRooms : Maybe (Dict Int Room)
    , changedMapCoordinatesForTempRooms : Maybe (List MapCoordinate)
    , doors : Dict Int Door
    , monsters : Dict Int MonsterDetails
    }


type alias Room =
    { roomNumber : Int
    , columns : Int
    , rows : Int
    , measurements : Measurements
    , gridCells : Dict String GridCell
    , isOpen : Bool
    , roomDoors : List RoomDoorDetails
    }


type alias Door =
    { doorNumber : Int
    , isOpen : Bool
    , connectedMapCoordinateOne : MapCoordinate
    , connectedMapCoordinateTwo : MapCoordinate
    , measurements : Measurements
    }


type alias GridCell =
    { startX : Int
    , startY : Int
    , mapCoordinate : MapCoordinate
    , polygonShape : String
    , cellState : CellState
    }


type alias RoomDoorDetails =
    { doorNumber : Int
    , doorIsOpen : Bool
    , roomCoordinate : RoomCoordinate
    , connectedMapCoordinate : MapCoordinate
    }


type CellState
    = Empty
    | CellMovement MovementType
    | FigureType FigureType


type MovementType
    = ClickedForMovement Int
    | CanBeMovedTo Int
    | IsPartOfMovePath Int
    | CanBeJumpedTo Int


type FigureType
    = Hero
    | Monster MonsterType Int Bool


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
