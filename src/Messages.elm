module Messages exposing (..)

import Browser.Dom exposing (Viewport)
import Draggable
import Math.Vector2 exposing (Vec2)
import Models.LevelState exposing (MapCoordinate)


type Msg
    = DragMsg (Draggable.Msg ())
    | OnDragBy Vec2
    | Zoom Float
    | GotViewport Viewport
    | GotNewSize Int Int
    | ResetMap
    | MapIsClicked MapCoordinate
    | CardIsClicked Int
    | SkipMovement
    | SkipAttack
    | OpenDoor Int
    | NextMoveAnimation MapCoordinate (List MapCoordinate)
    | StopMoveAnimation MapCoordinate
