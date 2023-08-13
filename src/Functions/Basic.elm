module Functions.Basic exposing (..)

import Models.Others exposing (Point)


isEven : Int -> Bool
isEven number =
    if modBy 2 number == 0 then
        True

    else
        False


makePointBetweenPoints : Point -> Point -> Point
makePointBetweenPoints start end =
    let
        middleX =
            calculateAverage start.x end.x

        middleY =
            calculateAverage start.y end.y
    in
    { x = middleX, y = middleY }


calculateAverage : Float -> Float -> Float
calculateAverage number1 number2 =
    (number1 + number2) / 2
