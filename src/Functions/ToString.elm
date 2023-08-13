module Functions.ToString exposing (..)

import Models.Others exposing (Point)


pointToString : Point -> String
pointToString point =
    "(" ++ String.fromFloat point.x ++ "," ++ String.fromFloat point.y ++ ")"
