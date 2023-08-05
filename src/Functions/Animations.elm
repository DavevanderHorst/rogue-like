module Functions.Animations exposing (..)

import Dict exposing (Dict)
import Functions.Coordinates exposing (createMapCoordinateAlt)
import Functions.DictFunctions.RoomDict exposing (getGridCellFromRoomDict)
import Models.BaseModel exposing (AnimationType(..))
import Models.LevelState exposing (MapCoordinate, Room, RoomCoordinate)


makeMoveAnimation : MapCoordinate -> Dict Int (List RoomCoordinate) -> Dict Int Room -> Result String AnimationType
makeMoveAnimation heroSpot changedCoordinates roomDict =
    let
        startGridCellResult =
            getGridCellFromRoomDict heroSpot roomDict
    in
    case startGridCellResult of
        Err err ->
            Err err

        Ok startGridCell ->
            case Dict.toList changedCoordinates of
                [ ( roomNumber, changedCoordinatesList ) ] ->
                    let
                        maybeNextSpot =
                            List.head changedCoordinatesList
                    in
                    case maybeNextSpot of
                        Nothing ->
                            Err "No changedCoordinate for making animation"

                        Just endSpot ->
                            let
                                endMapCoordinate =
                                    createMapCoordinateAlt roomNumber endSpot

                                endGridCellResult =
                                    getGridCellFromRoomDict endMapCoordinate roomDict
                            in
                            case endGridCellResult of
                                Err err ->
                                    Err err

                                Ok endGridCell ->
                                    Ok (Walk startGridCell endGridCell)

                _ ->
                    Err "TODO walk multiple rooms"
