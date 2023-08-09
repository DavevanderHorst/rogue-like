module Functions.Movement exposing
    ( makeMovementPathInTempRoomDictForLevel
    , setCanBeMovedToForMoveAbility
    , setCanBeMovedToForOpenedRoom
    )

import Dict exposing (Dict)
import Functions.Basic exposing (isEven)
import Functions.Coordinates exposing (createMapCoordinate, createMapCoordinateAlt, getNextCoordinate, goUp, goUpLeft, isSameRoomCoordinate)
import Functions.DictFunctions.GridCellDict exposing (getGridCellFromGridCellDict, setEmptyToCanBeMovedToInGridCellDict, setGridCellFromMovableToClickedUnsafe, setGridCellFromMovableToIsPathUnSafe, trySetMovementInGridCellForGridCells)
import Functions.DictFunctions.RoomDict exposing (addRoomToRoomDictUnSafe, getRoomFromRoomDict, setGridCellsForRoomInRoomDictUnSafe)
import Models.CardState exposing (CardAbility(..))
import Models.LevelState exposing (CellState(..), FigureType(..), GameMode(..), GridCell, GridDirection(..), Level, MapCoordinate, Measurements, Room, RoomCoordinate, RoomDoorDetails)


setCanBeMovedToForOpenedRoom : MapCoordinate -> GameMode -> Dict Int Room -> Result String (Dict Int Room)
setCanBeMovedToForOpenedRoom startCoordinate gameMode roomDict =
    case gameMode of
        CardAction cardAbility ->
            case cardAbility of
                Move movement ->
                    -- movement is from heroSpot next to door in other room, so - 1
                    if movement < 1 then
                        Err ("Cant set movement in new room, movement = " ++ String.fromInt movement)

                    else
                        let
                            getRoomResult =
                                getRoomFromRoomDict startCoordinate.roomNumber roomDict
                        in
                        case getRoomResult of
                            Err err ->
                                Err err

                            Ok room ->
                                -- now we need to set start spot to movement, and check if it is empty.
                                -- if empty and there is still movement left, then we can start setting cells around
                                let
                                    ( setMovementInGridCellSucceeded, gridCells ) =
                                        trySetMovementInGridCellForGridCells 1 startCoordinate.roomCoordinate room.gridCells
                                in
                                if setMovementInGridCellSucceeded then
                                    let
                                        newGridCells =
                                            if movement > 1 then
                                                setCanBeMovedTooForOpenedRoom movement startCoordinate.roomCoordinate gridCells

                                            else
                                                gridCells

                                        newRoom =
                                            { room | gridCells = newGridCells }
                                    in
                                    Ok (addRoomToRoomDictUnSafe newRoom roomDict)

                                else
                                    Ok roomDict

                Attack _ ->
                    Err "Cant set movement in new room, cardAbility = Attack"

        ChooseCard ->
            Err "Cant set movement in new room, game mode = ChooseCard"


setCanBeMovedToForMoveAbility : Int -> MapCoordinate -> Dict Int Room -> Result String (Dict Int Room)
setCanBeMovedToForMoveAbility steps heroSpot roomDict =
    let
        getRoomResult =
            getRoomFromRoomDict heroSpot.roomNumber roomDict
    in
    case getRoomResult of
        Err err ->
            Err err

        Ok room ->
            let
                newGridCells =
                    setCanBeMovedTooForMovement steps heroSpot.roomCoordinate room.gridCells

                newRoomDict =
                    setGridCellsForRoomInRoomDictUnSafe heroSpot.roomNumber newGridCells roomDict

                otherRoomsToSetCanBeMovedToo =
                    checkDoorsForMovement steps room.roomDoors newGridCells
            in
            if List.isEmpty otherRoomsToSetCanBeMovedToo then
                Ok newRoomDict

            else
                List.foldl (setCanBeMovedToForOtherRooms steps [ heroSpot.roomNumber ]) (Ok newRoomDict) otherRoomsToSetCanBeMovedToo


setCanBeMovedToForOtherRooms : Int -> List Int -> ( Int, MapCoordinate ) -> Result String (Dict Int Room) -> Result String (Dict Int Room)
setCanBeMovedToForOtherRooms totalSteps doneRoomNumberList ( firstStep, mapCoordinate ) result =
    case result of
        Err _ ->
            result

        Ok roomDict ->
            let
                getRoomResult =
                    getRoomFromRoomDict mapCoordinate.roomNumber roomDict
            in
            case getRoomResult of
                Err err ->
                    Err err

                Ok room ->
                    -- now we need to set start spot to movement, and check if it is empty.
                    -- if empty and there is still movement left, then we can start setting cells around
                    let
                        ( setMovementInGridCellSucceeded, gridCells ) =
                            trySetMovementInGridCellForGridCells firstStep mapCoordinate.roomCoordinate room.gridCells
                    in
                    if setMovementInGridCellSucceeded then
                        let
                            newGridCells =
                                if firstStep < totalSteps then
                                    setCanBeMovedTooForOtherRoom (firstStep + 1) totalSteps mapCoordinate.roomCoordinate gridCells

                                else
                                    gridCells

                            newRoom =
                                { room | gridCells = newGridCells }
                        in
                        Ok (addRoomToRoomDictUnSafe newRoom roomDict)

                    else
                        Ok roomDict


checkDoorsForMovement : Int -> List RoomDoorDetails -> Dict String GridCell -> List ( Int, MapCoordinate )
checkDoorsForMovement totalMovement roomDoors gridCells =
    -- We check every door if its open and if it is in canBeMovedTo state and that there is movement left.
    List.foldl (checkDoorForMovement totalMovement gridCells) [] roomDoors


checkDoorForMovement : Int -> Dict String GridCell -> RoomDoorDetails -> List ( Int, MapCoordinate ) -> List ( Int, MapCoordinate )
checkDoorForMovement totalMovement gridCells roomDoor result =
    if not roomDoor.doorIsOpen then
        result

    else
        let
            getGridCellResult =
                getGridCellFromGridCellDict roomDoor.roomCoordinate gridCells
        in
        case getGridCellResult of
            Err _ ->
                -- Should not be possible
                result

            Ok gridCell ->
                case gridCell.cellState of
                    CanBeMovedTo steps ->
                        if totalMovement > steps then
                            ( steps + 1, roomDoor.connectedMapCoordinate ) :: result

                        else
                            result

                    FigureType figure ->
                        case figure of
                            Hero ->
                                ( 1, roomDoor.connectedMapCoordinate ) :: result

                            _ ->
                                result

                    _ ->
                        result


setCanBeMovedTooForOpenedRoom : Int -> RoomCoordinate -> Dict String GridCell -> Dict String GridCell
setCanBeMovedTooForOpenedRoom totalSteps heroSpot gridCells =
    setCanBeMovedTo totalSteps heroSpot gridCells 2


setCanBeMovedTooForOtherRoom : Int -> Int -> RoomCoordinate -> Dict String GridCell -> Dict String GridCell
setCanBeMovedTooForOtherRoom startSteps totalSteps heroSpot gridCells =
    setCanBeMovedTo totalSteps heroSpot gridCells startSteps


setCanBeMovedTooForMovement : Int -> RoomCoordinate -> Dict String GridCell -> Dict String GridCell
setCanBeMovedTooForMovement totalSteps heroSpot gridCells =
    setCanBeMovedTo totalSteps heroSpot gridCells 1


setCanBeMovedTo : Int -> RoomCoordinate -> Dict String GridCell -> Int -> Dict String GridCell
setCanBeMovedTo steps heroSpot gridCells firstRoundMovement =
    let
        startSpot =
            getGoAroundStartSpot heroSpot
    in
    goAroundAndChangeCells steps 0 1 startSpot startSpot Right gridCells [] firstRoundMovement


goAroundAndChangeCells : Int -> Int -> Int -> RoomCoordinate -> RoomCoordinate -> GridDirection -> Dict String GridCell -> List RoomCoordinate -> Int -> Dict String GridCell
goAroundAndChangeCells totalRounds currentSteps roundNumber currentSpot endSpot currentDirection gridCellDict notYetSetCoordinates movementForFirstRound =
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
            goAroundAndChangeCells totalRounds 0 (roundNumber + 1) goAroundStartSpot endSpot Right gridCellDict notYetSetCoordinates movementForFirstRound

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
                goAroundAndChangeCells totalRounds nextSteps roundNumber nextCell endSpot nextDirection gridCellDict notYetSetCoordinates movementForFirstRound

            Ok currentGridCell ->
                -- cell exists so we change cell now, then continue
                if roundNumber == 1 then
                    let
                        updatedDict =
                            setEmptyToCanBeMovedToInGridCellDict movementForFirstRound currentGridCell.mapCoordinate.roomCoordinate gridCellDict
                    in
                    goAroundAndChangeCells totalRounds nextSteps roundNumber nextCell endSpot nextDirection updatedDict notYetSetCoordinates movementForFirstRound

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
                            goAroundAndChangeCells totalRounds nextSteps roundNumber nextCell endSpot nextDirection gridCellDict addedCoordinateToList movementForFirstRound

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
                            goAroundAndChangeCells totalRounds nextSteps roundNumber nextCell endSpot nextDirection updatedDict notYetSetCoordinates movementForFirstRound


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
                            makeMovementPath endCoordinate updatedGridCells activeRoom.roomDoors tempRooms
                    in
                    case gridCellsWithMovePathResult of
                        Err err ->
                            Err err

                        Ok ( updatedTempRoomDict, changedCellsList ) ->
                            Ok
                                { level
                                    | tempUpdatedRooms = Just updatedTempRoomDict
                                    , changedMapCoordinatesForTempRooms = Just changedCellsList
                                }


makeMovementPath : MapCoordinate -> Dict String GridCell -> List RoomDoorDetails -> Dict Int Room -> Result String ( Dict Int Room, List MapCoordinate )
makeMovementPath startSpot gridCellDict roomDoors roomDict =
    let
        maybeStartMovement =
            getMovementValue startSpot.roomCoordinate gridCellDict
    in
    case maybeStartMovement of
        Nothing ->
            Err "No start movement found for : makeMovementPath"

        Just movement ->
            let
                openDoors =
                    keepOpenDoorsInList roomDoors
            in
            makeMovementPathRecursive movement startSpot gridCellDict [ startSpot ] openDoors roomDict


keepOpenDoorsInList : List RoomDoorDetails -> List RoomDoorDetails
keepOpenDoorsInList doors =
    List.filter (\roomDoor -> roomDoor.doorIsOpen == True) doors


makeMovementPathRecursive : Int -> MapCoordinate -> Dict String GridCell -> List MapCoordinate -> List RoomDoorDetails -> Dict Int Room -> Result String ( Dict Int Room, List MapCoordinate )
makeMovementPathRecursive startMovement startSpot gridCellDict changedMapCoordinatesList openDoors roomDict =
    let
        newMovement =
            startMovement - 1
    in
    if newMovement == 0 then
        Ok ( setGridCellsForRoomInRoomDictUnSafe startSpot.roomNumber gridCellDict roomDict, changedMapCoordinatesList )

    else
        let
            ( hasOpenDoor, otherRoomMapCoordinate ) =
                checkRoomCoordinateForOpenDoor startSpot.roomCoordinate openDoors
        in
        if hasOpenDoor then
            let
                getOtherRoomResult =
                    getRoomFromRoomDict otherRoomMapCoordinate.roomNumber roomDict
            in
            case getOtherRoomResult of
                Err err ->
                    Err err

                Ok otherRoom ->
                    let
                        connectedRoomCoordinate =
                            otherRoomMapCoordinate.roomCoordinate

                        connectedCoordinateSteps =
                            getMovementValue connectedRoomCoordinate otherRoom.gridCells
                    in
                    if connectedCoordinateSteps == Just newMovement then
                        let
                            updatedGridCellDict =
                                setGridCellFromMovableToIsPathUnSafe connectedRoomCoordinate otherRoom.gridCells

                            updatedRoomDict =
                                setGridCellsForRoomInRoomDictUnSafe startSpot.roomNumber gridCellDict roomDict

                            newOpenDoors =
                                keepOpenDoorsInList otherRoom.roomDoors
                        in
                        makeMovementPathRecursive newMovement otherRoomMapCoordinate updatedGridCellDict (otherRoomMapCoordinate :: changedMapCoordinatesList) newOpenDoors updatedRoomDict

                    else
                        let
                            goAroundStartSpot =
                                getGoAroundStartSpot startSpot.roomCoordinate

                            nextCoordinateResult =
                                findNextCoordinateForSettingMovePath goAroundStartSpot newMovement gridCellDict Right True
                        in
                        case nextCoordinateResult of
                            Err err ->
                                Err err

                            Ok nextRoomCoordinate ->
                                let
                                    updatedGridCellDict =
                                        setGridCellFromMovableToIsPathUnSafe nextRoomCoordinate gridCellDict

                                    nextMapCoordinate =
                                        createMapCoordinateAlt startSpot.roomNumber nextRoomCoordinate
                                in
                                makeMovementPathRecursive newMovement nextMapCoordinate updatedGridCellDict (nextMapCoordinate :: changedMapCoordinatesList) openDoors roomDict

        else
            let
                goAroundStartSpot =
                    getGoAroundStartSpot startSpot.roomCoordinate

                nextCoordinateResult =
                    findNextCoordinateForSettingMovePath goAroundStartSpot newMovement gridCellDict Right True
            in
            case nextCoordinateResult of
                Err err ->
                    Err err

                Ok nextRoomCoordinate ->
                    let
                        updatedGridCellDict =
                            setGridCellFromMovableToIsPathUnSafe nextRoomCoordinate gridCellDict

                        nextMapCoordinate =
                            createMapCoordinateAlt startSpot.roomNumber nextRoomCoordinate
                    in
                    makeMovementPathRecursive newMovement nextMapCoordinate updatedGridCellDict (nextMapCoordinate :: changedMapCoordinatesList) openDoors roomDict


checkRoomCoordinateForOpenDoor : RoomCoordinate -> List RoomDoorDetails -> ( Bool, MapCoordinate )
checkRoomCoordinateForOpenDoor spot doors =
    List.foldl (checkCoordinateForOpenDoor spot) ( False, createMapCoordinate 0 0 0 ) doors


checkCoordinateForOpenDoor : RoomCoordinate -> RoomDoorDetails -> ( Bool, MapCoordinate ) -> ( Bool, MapCoordinate )
checkCoordinateForOpenDoor spot door result =
    if Tuple.first result then
        result

    else if isSameRoomCoordinate door.roomCoordinate spot then
        ( True, door.connectedMapCoordinate )

    else
        result


findNextCoordinateForSettingMovePath : RoomCoordinate -> Int -> Dict String GridCell -> GridDirection -> Bool -> Result String RoomCoordinate
findNextCoordinateForSettingMovePath currentSpot movement gridCellDict direction isStart =
    if direction == Right && not isStart then
        Err ("No cell found with needed movement to make path, " ++ String.fromInt movement)

    else
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
                findNextCoordinateForSettingMovePath nextSpot movement gridCellDict nextDirection False

            Ok cell ->
                if getMovementFromGridCell cell == Just movement then
                    Ok cell.mapCoordinate.roomCoordinate

                else
                    findNextCoordinateForSettingMovePath nextSpot movement gridCellDict nextDirection False


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
    -- Start spot is always up left cell
    if isEven spot.rowNumber then
        goUp spot

    else
        goUpLeft spot
