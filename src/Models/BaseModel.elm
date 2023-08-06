module Models.BaseModel exposing (..)

import Draggable
import Math.Vector2 exposing (Vec2)
import Models.CardState exposing (CardState)
import Models.LevelState exposing (GridCell, LevelState)


type BaseModel
    = OkModel Model
    | ErrorModel String


type alias Model =
    -- Main model
    { zoom : Float
    , center : Vec2
    , mapSize : Size
    , windowSize : Size
    , drag : Draggable.State ()
    , levelState : LevelState
    , cardState : CardState
    , animation : AnimationType
    }


type AnimationType
    = NoAnimation
    | Walk GridCell (List GridCell)


type alias Size =
    { width : Float
    , height : Float
    }
