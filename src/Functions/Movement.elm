module Functions.Movement exposing
    ( makeMovementPathInTempRoomDictForLevel
    , setCanBeJumpedToForAbility
    , setCanBeMovedToForMoveAbility
    , setDistanceForOpenedRoom
    )

import Dict exposing (Dict)
import Functions.Basic exposing (isEven)
import Functions.Coordinates exposing (createMapCoordinate, createMapCoordinateAlt, getNextCoordinate, goUp, goUpLeft, isSameRoomCoordinate)
import Functions.DictFunctions.GridCellDict exposing (getGridCellFromGridCellDict, setEmptyToCanBeJumpedToInGridCellDict, setEmptyToCanBeMovedToInGridCellDict, setGridCellFromMovableToClickedUnsafe, setGridCellFromMovableToIsPathUnSafe, trySetMovementInGridCellForGridCells)
import Functions.DictFunctions.RoomDict exposing (addRoomToRoomDictUnSafe, getRoomFromRoomDict, setGridCellsForRoomInRoomDictUnSafe)
import Models.CardState exposing (CardAbility(..))
import Models.LevelState exposing (CellState(..), FigureType(..), GameMode(..), GridCell, GridDirection(..), Level, MapCoordinate, Measurements, Room, RoomCoordinate, RoomDoorDetails)



-- Newly opened room


setDistanceForOpenedRoom : MapCoordinate -> GameMode -> Dict Int Room -> Result String (Dict Int Room)
setDistanceForOpenedRoom startCoordinate gameMode roomDict =
    case gameMode of
        CardAction cardAbility ->
            case cardAbility of
                Move steps ->
                    handleCanBeMovedToForOpenedRoom steps startCoordinate roomDict

                Jump distance ->
                    handleCanBeJumpedToForOpenedRoom distance startCoordinate roomDict

                Attack _ ->
                    Err "Cant set movement in new room, cardAbility = Attack"

        ChooseCard ->
            Err "Cant set movement in new room, game mode = ChooseCard"


handleCanBeMovedToForOpenedRoom : Int -> MapCoordinate -> Dict Int Room -> Result String (Dict Int Room)
handleCanBeMovedToForOpenedRoom movement startCoordinate roomDict =
    if movement < 1 then
        Err ("Cant set steps in new room, steps = " ++ String.fromInt movement)

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
                                setCanBeMovedToForOpenedRoom movement startCoordinate.roomCoordinate gridCells

                            else
                                gridCells

                        newRoom =
                            { room | gridCells = newGridCells }
                    in
                    Ok (addRoomToRoomDictUnSafe newRoom roomDict)

                else
                    -- room is blocked by something, so we cant enter
                    Ok roomDict


handleCanBeJumpedToForOpenedRoom : Int -> MapCoordinate -> Dict Int Room -> Result String (Dict Int Room)
handleCanBeJumpedToForOpenedRoom distance startCoordinate roomDict =
    if distance < 1 then
        Err ("Cant set distance in new room, distance = " ++ String.fromInt distance)

    else
        let
            getRoomResult =
                getRoomFromRoomDict startCoordinate.roomNumber roomDict
        in
        case getRoomResult of
            Err err ->
                Err err

            Ok room ->
                -- now we need to set first spot.
                -- if there is still movement left, then we can start setting cells around
                let
                    updatedGridCells =
                        setEmptyToCanBeJumpedToInGridCellDict 1 startCoordinate.roomCoordinate room.gridCells

                    newGridCells =
                        if distance > 1 then
                            setCanBeJumpedToForOpenedRoom distance startCoordinate.roomCoordinate updatedGridCells

                        else
                            updatedGridCells

                    newRoom =
                        { room | gridCells = newGridCells }
                in
                Ok (addRoomToRoomDictUnSafe newRoom roomDict)



--CAN BE MOVED TOO, FOR ABILITY MOVE


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
                    setCanBeMovedToForAbility steps heroSpot.roomCoordinate room.gridCells

                newRoomDict =
                    setGridCellsForRoomInRoomDictUnSafe heroSpot.roomNumber newGridCells roomDict

                otherRoomsToSetCanBeMovedToo =
                    checkDoorsForMovement steps room.roomDoors newGridCells
            in
            if List.isEmpty otherRoomsToSetCanBeMovedToo then
                Ok newRoomDict

            else
                List.foldl (setCanBeMovedToForOtherRooms steps) (Ok newRoomDict) otherRoomsToSetCanBeMovedToo


setCanBeMovedToForOtherRooms : Int -> ( Int, MapCoordinate ) -> Result String (Dict Int Room) -> Result String (Dict Int Room)
setCanBeMovedToForOtherRooms totalSteps ( firstStep, mapCoordinate ) result =
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
                                    setCanBeMovedToForOtherRoom (firstStep + 1) totalSteps mapCoordinate.roomCoordinate gridCells

                                else
                                    gridCells

                            newRoom =
                                { room | gridCells = newGridCells }
                        in
                        Ok (addRoomToRoomDictUnSafe newRoom roomDict)

                    else
                        Ok roomDict


setCanBeMovedToForOpenedRoom : Int -> RoomCoordinate -> Dict String GridCell -> Dict String GridCell
setCanBeMovedToForOpenedRoom totalSteps heroSpot gridCells =
    setCanBeMovedTo totalSteps heroSpot gridCells 2


setCanBeMovedToForOtherRoom : Int -> Int -> RoomCoordinate -> Dict String GridCell -> Dict String GridCell
setCanBeMovedToForOtherRoom startSteps totalSteps heroSpot gridCells =
    setCanBeMovedTo totalSteps heroSpot gridCells startSteps


setCanBeMovedToForAbility : Int -> RoomCoordinate -> Dict String GridCell -> Dict String GridCell
setCanBeMovedToForAbility totalSteps heroSpot gridCells =
    setCanBeMovedTo totalSteps heroSpot gridCells 1


setCanBeMovedTo : Int -> RoomCoordinate -> Dict String GridCell -> Int -> Dict String GridCell
setCanBeMovedTo steps heroSpot gridCells firstRoundMovement =
    let
        startSpot =
            getGoAroundStartSpot heroSpot
    in
    goAroundAndSetCanBeMovedTo steps 0 1 startSpot startSpot Right gridCells [] firstRoundMovement


goAroundAndSetCanBeMovedTo : Int -> Int -> Int -> RoomCoordinate -> RoomCoordinate -> GridDirection -> Dict String GridCell -> List RoomCoordinate -> Int -> Dict String GridCell
goAroundAndSetCanBeMovedTo totalRounds currentSteps roundNumber currentSpot endSpot currentDirection gridCellDict notYetSetCoordinates movementForFirstRound =
    if currentDirection == UpRight && currentSteps == roundNumber then
        if roundNumber == totalRounds then
            -- we are done
            if List.isEmpty notYetSetCoordinates then
                gridCellDict

            else
                -- there is a list of coordinates within movement reach that didnt have a cell wit ha movement value around.
                -- we go over these coordinates again, to see if anything has changed.
                handleNotYetSetCoordinates notYetSetCoordinates gridCellDict totalRounds

        else
            -- we need to start next round
            let
                goAroundStartSpot =
                    getGoAroundStartSpot currentSpot
            in
            goAroundAndSetCanBeMovedTo totalRounds 0 (roundNumber + 1) goAroundStartSpot endSpot Right gridCellDict notYetSetCoordinates movementForFirstRound

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
                goAroundAndSetCanBeMovedTo totalRounds nextSteps roundNumber nextCell endSpot nextDirection gridCellDict notYetSetCoordinates movementForFirstRound

            Ok currentGridCell ->
                -- cell exists so we change cell now, then continue
                if roundNumber == 1 then
                    let
                        updatedDict =
                            setEmptyToCanBeMovedToInGridCellDict movementForFirstRound currentGridCell.mapCoordinate.roomCoordinate gridCellDict
                    in
                    goAroundAndSetCanBeMovedTo totalRounds nextSteps roundNumber nextCell endSpot nextDirection updatedDict notYetSetCoordinates movementForFirstRound

                else
                    let
                        maybeStepsToMove =
                            checkLowestMovementCellsAround currentSpot gridCellDict
                    in
                    case maybeStepsToMove of
                        Nothing ->
                            let
                                -- no steps found around, we save this coordinate for later
                                addedCoordinateToList =
                                    currentSpot :: notYetSetCoordinates
                            in
                            goAroundAndSetCanBeMovedTo totalRounds nextSteps roundNumber nextCell endSpot nextDirection gridCellDict addedCoordinateToList movementForFirstRound

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
                            goAroundAndSetCanBeMovedTo totalRounds nextSteps roundNumber nextCell endSpot nextDirection updatedDict notYetSetCoordinates movementForFirstRound



-- CAN BE JUMPED TO FOR JUMP ABILITY


setCanBeJumpedToForAbility : Int -> MapCoordinate -> Dict Int Room -> Result String (Dict Int Room)
setCanBeJumpedToForAbility steps heroSpot roomDict =
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
                    setCanBeJumpedTo steps heroSpot.roomCoordinate room.gridCells 1

                newRoomDict =
                    setGridCellsForRoomInRoomDictUnSafe heroSpot.roomNumber newGridCells roomDict

                otherRoomsToSetCanBeJumpedTo =
                    checkDoorsForMovement steps room.roomDoors newGridCells
            in
            if List.isEmpty otherRoomsToSetCanBeJumpedTo then
                Ok newRoomDict

            else
                List.foldl (setCanBeJumpedToForOtherRooms steps) (Ok newRoomDict) otherRoomsToSetCanBeJumpedTo


setCanBeJumpedToForOtherRooms : Int -> ( Int, MapCoordinate ) -> Result String (Dict Int Room) -> Result String (Dict Int Room)
setCanBeJumpedToForOtherRooms totalSteps ( firstStep, mapCoordinate ) result =
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
                    -- now we need to set start spot to movement
                    -- if empty and there is still movement left, then we can start setting cells around
                    let
                        updatedGridCells =
                            setEmptyToCanBeJumpedToInGridCellDict firstStep mapCoordinate.roomCoordinate room.gridCells
                    in
                    let
                        newGridCells =
                            if firstStep < totalSteps then
                                setCanBeJumpedToForOtherRoom (firstStep + 1) totalSteps mapCoordinate.roomCoordinate updatedGridCells

                            else
                                updatedGridCells

                        newRoom =
                            { room | gridCells = newGridCells }
                    in
                    Ok (addRoomToRoomDictUnSafe newRoom roomDict)


setCanBeJumpedToForOpenedRoom : Int -> RoomCoordinate -> Dict String GridCell -> Dict String GridCell
setCanBeJumpedToForOpenedRoom totalDistance startSpot gridCellDict =
    setCanBeJumpedTo (totalDistance - 1) startSpot gridCellDict 2


setCanBeJumpedToForOtherRoom : Int -> Int -> RoomCoordinate -> Dict String GridCell -> Dict String GridCell
setCanBeJumpedToForOtherRoom startDistance totalDistance startSpot gridCellDict =
    setCanBeJumpedTo totalDistance startSpot gridCellDict startDistance


setCanBeJumpedTo : Int -> RoomCoordinate -> Dict String GridCell -> Int -> Dict String GridCell
setCanBeJumpedTo totalDistance heroSpot gridCells startValue =
    let
        startSpot =
            getGoAroundStartSpot heroSpot
    in
    goAroundAndSetCanBeJumpedTo totalDistance 0 1 startSpot startSpot True Right gridCells startValue


goAroundAndSetCanBeJumpedTo : Int -> Int -> Int -> RoomCoordinate -> RoomCoordinate -> Bool -> GridDirection -> Dict String GridCell -> Int -> Dict String GridCell
goAroundAndSetCanBeJumpedTo totalRounds currentSteps roundNumber currentSpot endSpot isStartOfRound currentDirection gridCellDict roundValue =
    if isSameRoomCoordinate currentSpot endSpot && not isStartOfRound then
        if roundNumber == totalRounds then
            -- we are done
            gridCellDict

        else
            -- we need to start next round
            let
                goAroundStartSpot =
                    getGoAroundStartSpot currentSpot
            in
            goAroundAndSetCanBeJumpedTo totalRounds 0 (roundNumber + 1) goAroundStartSpot goAroundStartSpot True Right gridCellDict (roundValue + 1)

    else
        let
            gridCellResult =
                getGridCellFromGridCellDict currentSpot gridCellDict

            ( nextCell, nextSteps, nextDirection ) =
                getGridCellStatsForNextRound currentDirection currentSteps roundNumber currentSpot
        in
        let
            updatedDict =
                case gridCellResult of
                    Err _ ->
                        -- not existing cell number
                        gridCellDict

                    Ok currentGridCell ->
                        -- cell exists so we change cell now
                        setEmptyToCanBeJumpedToInGridCellDict roundValue currentGridCell.mapCoordinate.roomCoordinate gridCellDict
        in
        goAroundAndSetCanBeJumpedTo totalRounds nextSteps roundNumber nextCell endSpot False nextDirection updatedDict roundValue



-- OTHERS


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


checkDoorsForMovement : Int -> List RoomDoorDetails -> Dict String GridCell -> List ( Int, MapCoordinate )
checkDoorsForMovement totalMovement roomDoors gridCells =
    -- We check every door if its open and if it is in a movable state and that there is movement left.
    -- If movement left, we set start coordinate with start movement
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

                    CanBeJumpedTo distance ->
                        if totalMovement > distance then
                            ( distance + 1, roomDoor.connectedMapCoordinate ) :: result

                        else
                            result

                    FigureType figure ->
                        case figure of
                            Hero ->
                                ( 1, roomDoor.connectedMapCoordinate ) :: result

                            _ ->
                                result

                    Empty ->
                        result

                    ClickedForMovement _ ->
                        result

                    IsPartOfMovePath _ ->
                        result


getMovementFromGridCell : GridCell -> Maybe Int
getMovementFromGridCell cell =
    case cell.cellState of
        Empty ->
            Nothing

        FigureType _ ->
            Nothing

        ClickedForMovement steps ->
            Just steps

        CanBeMovedTo steps ->
            Just steps

        IsPartOfMovePath steps ->
            Just steps

        CanBeJumpedTo distance ->
            Just distance


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
            getMovementFromGridCell gridCell


getGoAroundStartSpot : RoomCoordinate -> RoomCoordinate
getGoAroundStartSpot spot =
    -- Start spot is always up left cell
    if isEven spot.rowNumber then
        goUp spot

    else
        goUpLeft spot
