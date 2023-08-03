module Functions.DictFunctions.MonsterDict exposing (..)

import Dict exposing (Dict)
import Models.LevelState exposing (MonsterDetails)


getMonsterFromMonsterDict : Int -> Dict Int MonsterDetails -> Result String MonsterDetails
getMonsterFromMonsterDict monsterNumber monsterDict =
    let
        maybeMonster =
            Dict.get monsterNumber monsterDict
    in
    case maybeMonster of
        Nothing ->
            Err ("No monster with number " ++ String.fromInt monsterNumber ++ " in this monster dict yet.")

        Just monster ->
            Ok monster
