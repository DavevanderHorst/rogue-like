module Functions.Movement exposing (..)

import Dict exposing (Dict)
import Functions.Basic exposing (isEven)
import Functions.Coordinates exposing (getNextCoordinate, goUp, goUpLeft)
import Functions.DictFunctions.GridCellDict
    exposing
        ( getGridCellFromGridCellDict
        , setEmptyToCanBeMovedToInGridCellDict
        , setGridCellFromMovableToClickedUnsafe
        , setGridCellFromMovableToIsPathUnSafe
        )
import Functions.DictFunctions.RoomDict exposing (getRoomFromRoomDict, setGridCellsForRoomInRoomDictUnSafe)
import Models.LevelState exposing (CellState(..), GridCell, GridDirection(..), Level, MapCoordinate, Room, RoomCoordinate)



-- Set Movement


setCanBeMovedTooForMovement : Int -> RoomCoordinate -> Dict String GridCell -> Dict String GridCell
setCanBeMovedTooForMovement steps heroSpot gridCells =
    let
        startSpot =
            getGoAroundStartSpot heroSpot
    in
    goAroundAndChangeCells steps 0 1 startSpot startSpot Right gridCells []


goAroundAndChangeCells : Int -> Int -> Int -> RoomCoordinate -> RoomCoordinate -> GridDirection -> Dict String GridCell -> List RoomCoordinate -> Dict String GridCell
goAroundAndChangeCells totalRounds currentSteps roundNumber currentSpot endSpot currentDirection gridCellDict notYetSetCoordinates =
    if currentDirection == UpRight && currentSteps == roundNumber then
        if roundNumber == totalRounds then
            -- we are done
            if List.isEmpty notYetSetCoordinates then
                gridCellDict

            else
                handleNotYetSetCoordinates notYetSetCoordinates gridCellDict totalRounds

        else
            -- we need to start next round
            let
                goAroundStartSpot =
                    getGoAroundStartSpot currentSpot
            in
            goAroundAndChangeCells totalRounds 0 (roundNumber + 1) goAroundStartSpot endSpot Right gridCellDict notYetSetCoordinates

    else
        let
            gridCellResult =
                getGridCellFromGridCellDict currentSpot gridCellDict

            ( nextCell, nextSteps, nextDirection ) =
                getGridCellStatsForNextRound currentDirection currentSteps roundNumber currentSpot
        in
        case gridCellResult of
            Err _ ->
                -- not existing cell number, so we continue
                goAroundAndChangeCells totalRounds nextSteps roundNumber nextCell endSpot nextDirection gridCellDict notYetSetCoordinates

            Ok currentGridCell ->
                -- cell exists so we change cell now, then continue
                if roundNumber == 1 then
                    let
                        updatedDict =
                            setEmptyToCanBeMovedToInGridCellDict 1 currentGridCell.mapCoordinate.roomCoordinate gridCellDict
                    in
                    goAroundAndChangeCells totalRounds nextSteps roundNumber nextCell endSpot nextDirection updatedDict notYetSetCoordinates

                else
                    let
                        maybeStepsToMove =
                            checkLowestMovementCellsAround currentSpot gridCellDict
                    in
                    case maybeStepsToMove of
                        Nothing ->
                            let
                                addedCoordinateToList =
                                    currentSpot :: notYetSetCoordinates
                            in
                            goAroundAndChangeCells totalRounds nextSteps roundNumber nextCell endSpot nextDirection gridCellDict addedCoordinateToList

                        Just stepsToMove ->
                            let
                                thisCellSteps =
                                    stepsToMove + 1

                                updatedDict =
                                    if thisCellSteps > totalRounds then
                                        gridCellDict

                                    else
                                        setEmptyToCanBeMovedToInGridCellDict thisCellSteps currentGridCell.mapCoordinate.roomCoordinate gridCellDict
                            in
                            goAroundAndChangeCells totalRounds nextSteps roundNumber nextCell endSpot nextDirection updatedDict notYetSetCoordinates


getGridCellStatsForNextRound : GridDirection -> Int -> Int -> RoomCoordinate -> ( RoomCoordinate, Int, GridDirection )
getGridCellStatsForNextRound currentDirection currentSteps roundNumber currentSpot =
    if currentSteps == roundNumber then
        let
            nextDirection =
                getNextDirection currentDirection
        in
        ( getNextCoordinate nextDirection currentSpot, 1, nextDirection )

    else
        ( getNextCoordinate currentDirection currentSpot, currentSteps + 1, currentDirection )


handleNotYetSetCoordinates : List RoomCoordinate -> Dict String GridCell -> Int -> Dict String GridCell
handleNotYetSetCoordinates notYetSetCoordinates gridCellDict maxMovement =
    let
        totalCoordinates =
            List.length notYetSetCoordinates

        ( newNotYetSetCoordinates, newGridCellDict ) =
            List.foldr (trySetCoordinateToCanBeMoved maxMovement) ( [], gridCellDict ) notYetSetCoordinates
    in
    if List.length newNotYetSetCoordinates == totalCoordinates then
        newGridCellDict

    else
        handleNotYetSetCoordinates newNotYetSetCoordinates newGridCellDict maxMovement


trySetCoordinateToCanBeMoved : Int -> RoomCoordinate -> ( List RoomCoordinate, Dict String GridCell ) -> ( List RoomCoordinate, Dict String GridCell )
trySetCoordinateToCanBeMoved maxMovement spot ( notYetSetCoordinates, gridCellDict ) =
    let
        maybeStepsToMove =
            checkLowestMovementCellsAround spot gridCellDict
    in
    case maybeStepsToMove of
        Nothing ->
            ( spot :: notYetSetCoordinates, gridCellDict )

        Just stepsToMove ->
            let
                thisCellSteps =
                    stepsToMove + 1
            in
            if thisCellSteps > maxMovement then
                ( spot :: notYetSetCoordinates, gridCellDict )

            else
                let
                    updatedDict =
                        setEmptyToCanBeMovedToInGridCellDict thisCellSteps spot gridCellDict
                in
                ( notYetSetCoordinates, updatedDict )


checkLowestMovementCellsAround : RoomCoordinate -> Dict String GridCell -> Maybe Int
checkLowestMovementCellsAround currentSpot gridCellDict =
    let
        goAroundStartSpot =
            getGoAroundStartSpot currentSpot
    in
    getLowestMovement goAroundStartSpot Nothing gridCellDict Right


getLowestMovement : RoomCoordinate -> Maybe Int -> Dict String GridCell -> GridDirection -> Maybe Int
getLowestMovement currentSpot maybeLowest gridCellDict direction =
    let
        currentMovement =
            getMovementValue currentSpot gridCellDict

        newLowest =
            case currentMovement of
                Nothing ->
                    maybeLowest

                Just movement ->
                    case maybeLowest of
                        Nothing ->
                            Just movement

                        Just lowest ->
                            if movement < lowest then
                                Just movement

                            else
                                maybeLowest
    in
    if direction == UpRight then
        newLowest

    else
        let
            nextDirection =
                getNextDirection direction
        in
        getLowestMovement (getNextCoordinate direction currentSpot) newLowest gridCellDict nextDirection



-- Path


makeMovementPathInTempRoomDictForLevel : MapCoordinate -> Level -> Result String Level
makeMovementPathInTempRoomDictForLevel endCoordinate level =
    case level.tempUpdatedRooms of
        Nothing ->
            -- temp rooms should be set to make a path, cells need to be in state : CanBeMovedToo
            Err "Temp rooms are Nothing in : makeMovementPathInTempRoomDictForLevel"

        Just tempRooms ->
            let
                getRoomResult =
                    getRoomFromRoomDict endCoordinate.roomNumber tempRooms
            in
            case getRoomResult of
                Err err ->
                    Err err

                Ok activeRoom ->
                    let
                        updatedGridCells =
                            -- we assume that grid cell exists and is in state clicked
                            setGridCellFromMovableToClickedUnsafe endCoordinate.roomCoordinate activeRoom.gridCells

                        gridCellsWithMovePathResult =
                            makeMovementPath endCoordinate.roomCoordinate updatedGridCells
                    in
                    case gridCellsWithMovePathResult of
                        Err err ->
                            Err err

                        Ok ( gridCellsWithMovePath, changedCellsList ) ->
                            let
                                updatedTempRooms =
                                    setGridCellsForRoomInRoomDictUnSafe activeRoom.roomNumber gridCellsWithMovePath tempRooms

                                changedCellDict =
                                    let
                                        allChangedCells =
                                            endCoordinate.roomCoordinate :: changedCellsList
                                    in
                                    Just
                                        (Dict.fromList
                                            [ ( activeRoom.roomNumber, allChangedCells )
                                            ]
                                        )
                            in
                            Ok
                                { level
                                    | tempUpdatedRooms = Just updatedTempRooms
                                    , changedMapCoordinatesForTempRooms = changedCellDict
                                }


makeMovementPath : RoomCoordinate -> Dict String GridCell -> Result String ( Dict String GridCell, List RoomCoordinate )
makeMovementPath startSpot gridCellDict =
    let
        maybeStartMovement =
            getMovementValue startSpot gridCellDict
    in
    case maybeStartMovement of
        Nothing ->
            Err "No start movement found for : makeMovementPath"

        Just movement ->
            makeMovementPathRecursive movement startSpot gridCellDict []


makeMovementPathRecursive : Int -> RoomCoordinate -> Dict String GridCell -> List RoomCoordinate -> Result String ( Dict String GridCell, List RoomCoordinate )
makeMovementPathRecursive startMovement startSpot gridCellDict changedCoordinatesList =
    let
        newMovement =
            startMovement - 1
    in
    if newMovement == 0 then
        Ok ( gridCellDict, changedCoordinatesList )

    else
        let
            goAroundStartSpot =
                getGoAroundStartSpot startSpot

            nextCoordinateResult =
                findNextCoordinateForSettingMovePath goAroundStartSpot newMovement gridCellDict Right
        in
        case nextCoordinateResult of
            Err err ->
                Err err

            Ok nextCoordinate ->
                let
                    updatedGridCellDict =
                        setGridCellFromMovableToIsPathUnSafe nextCoordinate gridCellDict
                in
                makeMovementPathRecursive newMovement nextCoordinate updatedGridCellDict (nextCoordinate :: changedCoordinatesList)


findNextCoordinateForSettingMovePath : RoomCoordinate -> Int -> Dict String GridCell -> GridDirection -> Result String RoomCoordinate
findNextCoordinateForSettingMovePath currentSpot movement gridCellDict direction =
    let
        cellResult =
            getGridCellFromGridCellDict currentSpot gridCellDict

        nextSpot =
            getNextCoordinate direction currentSpot

        nextDirection =
            getNextDirection direction
    in
    case cellResult of
        Err _ ->
            if direction == UpRight then
                Err "No cell found with needed movement to make path"

            else
                findNextCoordinateForSettingMovePath nextSpot movement gridCellDict nextDirection

        Ok cell ->
            if getMovementFromGridCell cell == Just movement then
                Ok cell.mapCoordinate.roomCoordinate

            else
                findNextCoordinateForSettingMovePath nextSpot movement gridCellDict nextDirection


getMovementFromGridCell : GridCell -> Maybe Int
getMovementFromGridCell cell =
    case cell.cellState of
        Empty ->
            Nothing

        FigureType _ ->
            Nothing

        ClickedForMovement moves ->
            Just moves

        CanBeMovedTo moves ->
            Just moves

        IsPartOfMovePath moves ->
            Just moves


getNextDirection : GridDirection -> GridDirection
getNextDirection direction =
    -- for circling a gridCell
    case direction of
        Left ->
            UpLeft

        UpLeft ->
            UpRight

        UpRight ->
            Right

        Right ->
            DownRight

        DownRight ->
            DownLeft

        DownLeft ->
            Left


getMovementValue : RoomCoordinate -> Dict String GridCell -> Maybe Int
getMovementValue spot gridCellDict =
    let
        currentGridCellResult =
            getGridCellFromGridCellDict spot gridCellDict
    in
    case currentGridCellResult of
        Err _ ->
            Nothing

        Ok gridCell ->
            case gridCell.cellState of
                Empty ->
                    Nothing

                FigureType _ ->
                    Nothing

                ClickedForMovement movement ->
                    Just movement

                CanBeMovedTo movement ->
                    Just movement

                IsPartOfMovePath movement ->
                    Just movement


getGoAroundStartSpot : RoomCoordinate -> RoomCoordinate
getGoAroundStartSpot spot =
    if isEven spot.rowNumber then
        goUp spot

    else
        goUpLeft spot
