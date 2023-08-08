module Functions.DictFunctions.GridCellDict exposing (..)

import Dict exposing (Dict)
import Functions.Coordinates exposing (roomCoordinateToString)
import Models.LevelState exposing (CellState(..), FigureType(..), GridCell, MonsterType(..), Room, RoomCoordinate)


getGridCellFromGridCellDict : RoomCoordinate -> Dict String GridCell -> Result String GridCell
getGridCellFromGridCellDict roomCoordinate gridCellDict =
    let
        maybeGridCell =
            Dict.get (createGridCellDictKey roomCoordinate) gridCellDict
    in
    case maybeGridCell of
        Nothing ->
            Err ("Room coordinate is not in our grid cell dict : " ++ roomCoordinateToString roomCoordinate)

        Just gridCell ->
            Ok gridCell


addGridCellTooGridCellDictUnSafe : GridCell -> Dict String GridCell -> Dict String GridCell
addGridCellTooGridCellDictUnSafe gridCell gridCellDict =
    -- unsafe, because if key already exists it overwrites, but sometimes we want this
    Dict.insert (createGridCellDictKey gridCell.mapCoordinate.roomCoordinate) gridCell gridCellDict


openGridCellDoorInGridCellDictUnSafe : RoomCoordinate -> Dict String GridCell -> Dict String GridCell
openGridCellDoorInGridCellDictUnSafe roomCoordinate gridCellDict =
    updateGridCellDict roomCoordinate openGridCellDoor gridCellDict


setGridCellFromMovableToClickedUnsafe : RoomCoordinate -> Dict String GridCell -> Dict String GridCell
setGridCellFromMovableToClickedUnsafe roomCoordinate gridCellDict =
    -- Unsafe, only use when your 100% sure cellState is CanBeMovedToo
    updateGridCellDict roomCoordinate setMovableToClicked gridCellDict


setGridCellFromMovableToIsPathUnSafe : RoomCoordinate -> Dict String GridCell -> Dict String GridCell
setGridCellFromMovableToIsPathUnSafe roomCoordinate gridCellDict =
    -- Unsafe, only use when your 100% sure cellState is CanBeMovedToo
    updateGridCellDict roomCoordinate setMovableToIsPath gridCellDict


setGridCellFromPartOfPathToCanBeMovedTo : RoomCoordinate -> Dict String GridCell -> Dict String GridCell
setGridCellFromPartOfPathToCanBeMovedTo roomCoordinate gridCellDict =
    -- This is used to reset our created path
    -- Clicked and movePath will be set to CanBeMovedToo
    updateGridCellDict roomCoordinate setPathToIsMovable gridCellDict


setHeroToEmptyInGridCellDictUnSafe : RoomCoordinate -> Dict String GridCell -> Dict String GridCell
setHeroToEmptyInGridCellDictUnSafe roomCoordinate gridCellDict =
    -- Unsafe, only use when your 100% sure cellState is FigureType Hero
    updateGridCellDict roomCoordinate setCellStateToEmpty gridCellDict


setHeroInGridCellDictUnSafe : RoomCoordinate -> Dict String GridCell -> Dict String GridCell
setHeroInGridCellDictUnSafe roomCoordinate gridCellDict =
    -- Unsafe, only use when your 100% sure cellState is Empty
    updateGridCellDict roomCoordinate setCellStateToHero gridCellDict


setEmptyToCanBeMovedToInGridCellDict : Int -> RoomCoordinate -> Dict String GridCell -> Dict String GridCell
setEmptyToCanBeMovedToInGridCellDict moves roomCoordinate gridCellDict =
    updateGridCellDict roomCoordinate (setEmptyToCanBeMovedTo moves) gridCellDict


updateGridCellDict : RoomCoordinate -> (Maybe GridCell -> Maybe GridCell) -> Dict String GridCell -> Dict String GridCell
updateGridCellDict roomCoordinate function gridCellDict =
    Dict.update (createGridCellDictKey roomCoordinate) function gridCellDict


trySetMovementInGridCellForGridCells : Int -> RoomCoordinate -> Dict String GridCell -> ( Bool, Dict String GridCell )
trySetMovementInGridCellForGridCells movement spot gridCellDict =
    let
        gridCellResult =
            getGridCellFromGridCellDict spot gridCellDict
    in
    case gridCellResult of
        Err _ ->
            ( False, gridCellDict )

        Ok gridCell ->
            case gridCell.cellState of
                Empty ->
                    ( True, updateGridCellDict spot (setEmptyToCanBeMovedTo movement) gridCellDict )

                _ ->
                    ( False, gridCellDict )


setCellStateToHero : Maybe GridCell -> Maybe GridCell
setCellStateToHero =
    Maybe.map
        (\old -> { old | cellState = FigureType Hero })


openGridCellDoor : Maybe GridCell -> Maybe GridCell
openGridCellDoor =
    Maybe.map
        (\old ->
            case old.maybeGridDoorDetails of
                Nothing ->
                    old

                Just door ->
                    let
                        newDoor =
                            { door | doorIsOpen = True }
                    in
                    { old | maybeGridDoorDetails = Just newDoor }
        )


setCellStateToEmpty : Maybe GridCell -> Maybe GridCell
setCellStateToEmpty =
    Maybe.map
        (\old -> { old | cellState = Empty })


setMovableToClicked : Maybe GridCell -> Maybe GridCell
setMovableToClicked =
    Maybe.map
        (\old ->
            case old.cellState of
                Empty ->
                    old

                FigureType _ ->
                    old

                ClickedForMovement _ ->
                    old

                CanBeMovedTo moves ->
                    { old | cellState = ClickedForMovement moves }

                IsPartOfMovePath _ ->
                    old
        )


setPathToIsMovable : Maybe GridCell -> Maybe GridCell
setPathToIsMovable =
    -- clicked is also part of move path
    Maybe.map
        (\old ->
            case old.cellState of
                Empty ->
                    old

                FigureType _ ->
                    old

                ClickedForMovement moves ->
                    { old | cellState = CanBeMovedTo moves }

                CanBeMovedTo _ ->
                    old

                IsPartOfMovePath moves ->
                    { old | cellState = CanBeMovedTo moves }
        )


setEmptyToCanBeMovedTo : Int -> Maybe GridCell -> Maybe GridCell
setEmptyToCanBeMovedTo moves =
    Maybe.map
        (\old ->
            case old.cellState of
                Empty ->
                    { old | cellState = CanBeMovedTo moves }

                FigureType _ ->
                    old

                ClickedForMovement _ ->
                    old

                CanBeMovedTo _ ->
                    old

                IsPartOfMovePath _ ->
                    old
        )


setMovableToIsPath : Maybe GridCell -> Maybe GridCell
setMovableToIsPath =
    Maybe.map
        (\old ->
            case old.cellState of
                Empty ->
                    old

                FigureType _ ->
                    old

                ClickedForMovement _ ->
                    old

                CanBeMovedTo moves ->
                    { old | cellState = IsPartOfMovePath moves }

                IsPartOfMovePath _ ->
                    old
        )


createGridCellDictKey : RoomCoordinate -> String
createGridCellDictKey roomCoordinate =
    String.fromInt roomCoordinate.columnNumber ++ "," ++ String.fromInt roomCoordinate.rowNumber


cellStateToString : CellState -> String
cellStateToString cellState =
    case cellState of
        Empty ->
            "Empty"

        FigureType figureType ->
            "FigureType " ++ figureTypeToString figureType

        ClickedForMovement int ->
            "ClickedForMovement " ++ String.fromInt int

        CanBeMovedTo int ->
            "CanBeMovedToo" ++ String.fromInt int

        IsPartOfMovePath int ->
            "IsPartOfMovePath" ++ String.fromInt int


figureTypeToString : FigureType -> String
figureTypeToString figure =
    case figure of
        Hero ->
            "Hero"

        Monster monsterType number ->
            case monsterType of
                Dummy ->
                    "Dummy, with number " ++ String.fromInt number


getStepsFromGridCellForClickedCell : RoomCoordinate -> Dict String GridCell -> Result String Int
getStepsFromGridCellForClickedCell spot gridCells =
    let
        gridCellResult =
            getGridCellFromGridCellDict spot gridCells
    in
    case gridCellResult of
        Err err ->
            Err err

        Ok gridCell ->
            case gridCell.cellState of
                Empty ->
                    Err (wrongStateError gridCell.cellState)

                FigureType _ ->
                    Err (wrongStateError gridCell.cellState)

                ClickedForMovement steps ->
                    Ok steps

                CanBeMovedTo _ ->
                    Err (wrongStateError gridCell.cellState)

                IsPartOfMovePath _ ->
                    Err (wrongStateError gridCell.cellState)


wrongStateError : CellState -> String
wrongStateError cellState =
    "Cell is in wrong state : " ++ cellStateToString cellState
