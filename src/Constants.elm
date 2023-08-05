module Constants exposing (..)

--Room Sizes

import Math.Vector2 as Vector2 exposing (Vec2)
import Models.BaseModel exposing (Size)


roomPadding : Int
roomPadding =
    -- Needs to be divisible by 2
    20


halfRoomPadding : Int
halfRoomPadding =
    roomPadding // 2



-- Cell


cellWidth : Int
cellWidth =
    -- Needs to be divisible by 8
    56


halfCellWidth : Int
halfCellWidth =
    cellWidth // 2


cellWidthString : String
cellWidthString =
    String.fromInt cellWidth ++ "px"


quarterCellWidth : Int
quarterCellWidth =
    cellWidth // 4


oneEightCellWidth : Int
oneEightCellWidth =
    cellWidth // 8


cellMargin : Int
cellMargin =
    -- Needs to be divisible by 2
    2


halfCellMargin : Int
halfCellMargin =
    cellMargin // 2



-- GridPolygons


horizontalGridPolygon : String
horizontalGridPolygon =
    "polygon(0% 25%, 0% 75%, 50% 100%, 100% 75%, 100% 25%, 50% 0%)"


horizontalGridPolygonWithDoorUp : String
horizontalGridPolygonWithDoorUp =
    "polygon(0% 25%, 0% 75%, 50% 100%, 100% 75%, 100% 25%, 75% 12.5%, 75% 0%, 25% 0%, 25% 12.5%)"


horizontalGridPolygonWithDoorDown : String
horizontalGridPolygonWithDoorDown =
    "polygon(0% 25%, 0% 75%, 25% 87.5%, 25% 100%, 75% 100%, 75% 87.5%, 100% 75%, 100% 25%, 50% 0%)"


verticalGridPolygon : String
verticalGridPolygon =
    "polygon(25% 0%, 75% 0%, 100% 50%, 75% 100%, 25% 100%, 0% 50%)"


doorPolygon : String
doorPolygon =
    "polygon(0% 0%, 50% 25%, 100% 0%, 100% 100%, 50% 75%, 0% 100%)"



--Map and zoom


startZoom : Float
startZoom =
    1


startCenter : Vec2
startCenter =
    Vector2.vec2 300 50


startSize : Size
startSize =
    Size 0 0



-- Screen sizes


mapSizeWidth : Float
mapSizeWidth =
    -- 60%
    0.6


mapSizeHeight : Float
mapSizeHeight =
    -- 70%
    0.73


cardRowHeight : String
cardRowHeight =
    "24%"



-- Animation


moveWaitTime : Float
moveWaitTime =
    2000
