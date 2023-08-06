module Functions.LevelState exposing (..)

import Functions.Level exposing (removeHeroFromLevel, setHeroInLevel)
import Models.LevelState exposing (LevelState, MapCoordinate)


makeLevelStateReadyForMoveAnimation : LevelState -> Result String LevelState
makeLevelStateReadyForMoveAnimation levelState =
    -- Remove hero, hero will be animated
    -- We remove the temp rooms
    let
        levelWithoutHeroResult =
            removeHeroFromLevel levelState.heroSpot levelState.level
    in
    case levelWithoutHeroResult of
        Err err ->
            Err err

        Ok levelWithoutHero ->
            let
                readyLevel =
                    { levelWithoutHero | tempUpdatedRooms = Nothing }
            in
            Ok { levelState | level = readyLevel, formerClickedCell = Nothing }
