module Functions.DictFunctions.Level exposing (addMonstersAndRoomToLevelSafe, addMonstersAndRoomToLevelUnSafe)

import Dict
import Functions.DictFunctions.MonsterDict exposing (getMonsterFromMonsterDict)
import Functions.DictFunctions.RoomDict exposing (getRoomFromRoomDict)
import Models.LevelState exposing (Level, MonsterDetails, Room)


addMonstersAndRoomToLevelUnSafe : List MonsterDetails -> Room -> Level -> Result String Level
addMonstersAndRoomToLevelUnSafe monsterList room level =
    let
        monstersAdded =
            List.foldl addMonsterToLevelUnSafe level monsterList
    in
    Ok (addRoomToLevelUnSafe room monstersAdded)


addMonstersAndRoomToLevelSafe : List MonsterDetails -> Room -> Level -> Result String Level
addMonstersAndRoomToLevelSafe monsterList room level =
    let
        monstersAddedResult =
            List.foldl addMonsterToLevelSafe (Ok level) monsterList
    in
    case monstersAddedResult of
        Err err ->
            Err err

        Ok monstersAdded ->
            addRoomToLevelSafe room monstersAdded


addMonsterToLevelUnSafe : MonsterDetails -> Level -> Level
addMonsterToLevelUnSafe monster level =
    -- unsafe, because if key already exists it will be overwritten.
    { level | monsters = Dict.insert monster.monsterNumber monster level.monsters }


addMonsterToLevelSafe : MonsterDetails -> Result String Level -> Result String Level
addMonsterToLevelSafe monster levelResult =
    case levelResult of
        Err err ->
            Err err

        Ok level ->
            let
                getMonsterResult =
                    getMonsterFromMonsterDict monster.monsterNumber level.monsters
            in
            case getMonsterResult of
                Ok _ ->
                    Err
                        ("Cant add monster, there is already a monster with number: "
                            ++ String.fromInt monster.monsterNumber
                            ++ " in our monster dict from our level."
                        )

                Err _ ->
                    -- Does not yet exists so we can add it.
                    Ok (addMonsterToLevelUnSafe monster level)


addRoomToLevelUnSafe : Room -> Level -> Level
addRoomToLevelUnSafe room level =
    -- unsafe, because if key already exists it will be overwritten.
    { level | rooms = Dict.insert room.roomNumber room level.rooms }


addRoomToLevelSafe : Room -> Level -> Result String Level
addRoomToLevelSafe room level =
    let
        getRoomResult =
            getRoomFromRoomDict room.roomNumber level.rooms
    in
    case getRoomResult of
        Ok _ ->
            Err
                ("Cant add room, there is already a room with number: "
                    ++ String.fromInt room.roomNumber
                    ++ " in our room dict from our level."
                )

        Err _ ->
            -- Does not yet exists so we can add it.
            Ok (addRoomToLevelUnSafe room level)
