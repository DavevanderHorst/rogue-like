module Functions.ToString exposing (..)

import Models.LevelState exposing (FigureType(..), MonsterType(..))
import Models.Others exposing (Point)


figureTypeToString : FigureType -> String
figureTypeToString figure =
    case figure of
        Hero ->
            "Hero"

        Monster monsterType number _ ->
            case monsterType of
                Dummy ->
                    "Dummy, with number " ++ String.fromInt number


pointToString : Point -> String
pointToString point =
    "(" ++ String.fromFloat point.x ++ "," ++ String.fromFloat point.y ++ ")"
