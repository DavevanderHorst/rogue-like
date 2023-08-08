module Functions.DictFunctions.DoorDict exposing (..)

import Dict exposing (Dict)
import Models.LevelState exposing (Door)


addDoorToDoorDictUnSafe : Door -> Dict Int Door -> Dict Int Door
addDoorToDoorDictUnSafe door doorDict =
    -- Unsafe there is no check if door already exists, if exists door will be overwritten.
    Dict.insert door.doorNumber door doorDict


getDoorFromDoorDict : Int -> Dict Int Door -> Result String Door
getDoorFromDoorDict doorNumber doorDict =
    let
        maybeDoor =
            Dict.get doorNumber doorDict
    in
    case maybeDoor of
        Nothing ->
            Err ("No door with number " ++ String.fromInt doorNumber ++ " in this level.")

        Just door ->
            Ok door
