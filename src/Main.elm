module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events exposing (onResize)
import Constants exposing (jumpAnimationDuration, mapSizeHeight, mapSizeWidth, moveAnimationDuration, startCenter, startSize, startZoom)
import Draggable
import Functions.Abilities exposing (handleNextAbility)
import Functions.Animations exposing (makeJumpAnimation, makeMoveAnimation)
import Functions.Coordinates exposing (isSameMapCoordinate)
import Functions.DictFunctions.RoomDict exposing (getStepsToMoveTowardsClickedCell)
import Functions.HeroCards exposing (activateDoubleClickedCard)
import Functions.Level exposing (resetChangedCoordinatesInTempRoomDictForLevel, setCanBeJumpedToToIsClickedForLevelUnSafe, setHeroInLevel, updateLevelForCardAbility, updateLevelForMovementAbility)
import Functions.LevelState exposing (makeLevelStateReadyForMovementAnimation, openDoorInLevelState)
import Functions.Movement exposing (makeMovementPathInTempRoomDictForLevel)
import HeroCards exposing (emptyHeroCard, startHeroCards)
import Levels.LevelOne exposing (heroStartMapCoordinate, levelOneResult)
import Math.Vector2 as Vector2
import Messages exposing (Msg(..))
import Models.BaseModel exposing (AnimationType(..), BaseModel(..), Model, Size)
import Models.CardState exposing (CardAbility(..), CardState, MovementAbility(..))
import Models.LevelState exposing (GameMode(..), LevelState, MapCoordinate, Room)
import Process
import Task
import Views.MainView exposing (view)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( BaseModel, Cmd Msg )
init _ =
    case initLevelStateResult of
        Ok initLevelState ->
            ( OkModel
                { zoom = startZoom
                , center = startCenter
                , mapSize = startSize
                , windowSize = startSize
                , drag = Draggable.init
                , levelState = initLevelState
                , cardState = initCardState
                , animation = NoAnimation
                }
            , Task.perform GotViewport Browser.Dom.getViewport
            )

        Err err ->
            ( ErrorModel err, Cmd.none )


initLevelStateResult : Result String LevelState
initLevelStateResult =
    case levelOneResult of
        Ok lvl ->
            Ok
                { level = lvl
                , gameMode = ChooseCard
                , formerClickedCell = Nothing
                , maybeClickedCard = Nothing
                , heroSpot = heroStartMapCoordinate
                }

        Err err ->
            Err err


initCardState : CardState
initCardState =
    { heroCards = startHeroCards, activeCard = emptyHeroCard }


subscriptions : BaseModel -> Sub Msg
subscriptions baseModel =
    case baseModel of
        OkModel { drag } ->
            Sub.batch
                [ Draggable.subscriptions DragMsg drag
                , onResize (\w h -> GotNewSize w h)
                ]

        ErrorModel _ ->
            Sub.none


update : Msg -> BaseModel -> ( BaseModel, Cmd Msg )
update msg baseModel =
    case baseModel of
        ErrorModel _ ->
            ( baseModel, Cmd.none )

        OkModel model ->
            let
                ( center, zoom ) =
                    ( model.center, model.zoom )
            in
            case msg of
                OnDragBy rawDelta ->
                    let
                        delta =
                            rawDelta
                                |> Vector2.scale (-1 / zoom)
                    in
                    ( OkModel { model | center = center |> Vector2.add delta }, Cmd.none )

                Zoom factor ->
                    let
                        newZoom =
                            zoom
                                |> (+) (factor * 0.0005)
                                |> clamp 0.2 5
                    in
                    ( OkModel { model | zoom = newZoom }, Cmd.none )

                GotViewport viewPort ->
                    handleScreenSize viewPort.viewport.width viewPort.viewport.height model

                GotNewSize width height ->
                    handleScreenSize (toFloat width) (toFloat height) model

                DragMsg dragMsg ->
                    let
                        ( newModel, newMsg ) =
                            Draggable.update dragConfig dragMsg model
                    in
                    ( OkModel newModel, newMsg )

                ResetMap ->
                    ( OkModel { model | zoom = startZoom, center = startCenter }, Cmd.none )

                MapIsClicked clickedCoordinate ->
                    case model.levelState.formerClickedCell of
                        Nothing ->
                            handleFirstClickedMapCoordinate model clickedCoordinate

                        Just formerClickedCell ->
                            case model.levelState.gameMode of
                                CardAction ability ->
                                    if isSameMapCoordinate formerClickedCell clickedCoordinate then
                                        case ability of
                                            Movement movementAbility ->
                                                doMovementAbility movementAbility clickedCoordinate model

                                            Attack _ ->
                                                -- TODO
                                                ( ErrorModel "not implemented", Cmd.none )

                                    else
                                        -- other cell is clicked
                                        handleNewlyClickedMapCoordinate model clickedCoordinate formerClickedCell

                                ChooseCard ->
                                    ( ErrorModel "Choose card not possible in map is clicked", Cmd.none )

                CardIsClicked number ->
                    case model.levelState.maybeClickedCard of
                        Nothing ->
                            ( OkModel <| addClickedCardNumberToModel number model, Cmd.none )

                        Just clickedCardNumber ->
                            if clickedCardNumber == number then
                                handleTwiceClickedCard model number

                            else
                                ( OkModel <| addClickedCardNumberToModel number model, Cmd.none )

                SkipMovement ->
                    handleNextAbility model

                SkipAttack ->
                    --TODO
                    ( ErrorModel "to be implemented", Cmd.none )

                OpenDoor doorNumber ->
                    let
                        openDoorResult =
                            openDoorInLevelState doorNumber model.levelState
                    in
                    case openDoorResult of
                        Err err ->
                            ( ErrorModel err, Cmd.none )

                        Ok stateWithOpenedDoor ->
                            ( OkModel { model | levelState = stateWithOpenedDoor }, Cmd.none )

                MovementAnimationIsDone stepsLeft ->
                    let
                        oldLevelState =
                            model.levelState

                        levelWithHeroResult =
                            setHeroInLevel oldLevelState.heroSpot oldLevelState.level
                    in
                    case levelWithHeroResult of
                        Err err ->
                            ( ErrorModel err, Cmd.none )

                        Ok updatedLevel ->
                            let
                                updatedLevelState =
                                    { oldLevelState | level = updatedLevel }

                                updatedModel =
                                    { model | levelState = updatedLevelState, animation = NoAnimation }
                            in
                            handleMovementAbility stepsLeft updatedModel


doMovementAbility : MovementAbility -> MapCoordinate -> Model -> ( BaseModel, Cmd Msg )
doMovementAbility ability clickedCoordinate model =
    case ability of
        Move movement ->
            let
                stepsMovedResult =
                    getStepsToMoveTowardsClickedCell clickedCoordinate model.levelState.level
            in
            case stepsMovedResult of
                Err err ->
                    ( ErrorModel err, Cmd.none )

                Ok stepsMoved ->
                    let
                        stepsLeft =
                            movement - stepsMoved
                    in
                    makeReadyForMoveAnimation model clickedCoordinate stepsLeft

        Jump _ ->
            makeReadyForJumpAnimation model clickedCoordinate


handleScreenSize : Float -> Float -> Model -> ( BaseModel, Cmd Msg )
handleScreenSize width height model =
    let
        newSize =
            Size width height

        mapSize =
            Size (newSize.width * mapSizeWidth) (newSize.height * mapSizeHeight)
    in
    ( OkModel { model | mapSize = mapSize, windowSize = newSize }, Cmd.none )


dragConfig : Draggable.Config () Msg
dragConfig =
    Draggable.basicConfig (OnDragBy << (\( dx, dy ) -> Vector2.vec2 dx dy))


handleFirstClickedMapCoordinate : Model -> MapCoordinate -> ( BaseModel, Cmd Msg )
handleFirstClickedMapCoordinate model clickedCoordinate =
    case model.levelState.gameMode of
        ChooseCard ->
            ( ErrorModel "State is ChooseCard", Cmd.none )

        CardAction cardAbility ->
            let
                updatedLevelResult =
                    case cardAbility of
                        Movement movementAbility ->
                            case movementAbility of
                                Move _ ->
                                    makeMovementPathInTempRoomDictForLevel clickedCoordinate model.levelState.level

                                Jump _ ->
                                    setCanBeJumpedToToIsClickedForLevelUnSafe clickedCoordinate model.levelState.level

                        Attack int ->
                            Err "TODO"
            in
            case updatedLevelResult of
                Err err ->
                    ( ErrorModel err, Cmd.none )

                Ok updatedLevel ->
                    ( OkModel <|
                        updateLevelState model
                            (\state ->
                                { state
                                    | formerClickedCell = Just clickedCoordinate
                                    , level = updatedLevel
                                }
                            )
                    , Cmd.none
                    )


handleNewlyClickedMapCoordinate : Model -> MapCoordinate -> MapCoordinate -> ( BaseModel, Cmd Msg )
handleNewlyClickedMapCoordinate model new old =
    if isSameMapCoordinate new old then
        ( ErrorModel "New and old are same map coordinate in : handleNewlyClickedMapCoordinate", Cmd.none )

    else
        let
            currentLevel =
                model.levelState.level

            resetLevelResult =
                resetChangedCoordinatesInTempRoomDictForLevel currentLevel
        in
        case resetLevelResult of
            Err err ->
                ( ErrorModel err, Cmd.none )

            Ok resetLevel ->
                handleFirstClickedMapCoordinate
                    (updateLevelState
                        model
                        (\state ->
                            { state
                                | level = resetLevel
                            }
                        )
                    )
                    new


handleMovementAbility : Int -> Model -> ( BaseModel, Cmd Msg )
handleMovementAbility stepsLeft model =
    if stepsLeft == 0 then
        handleNextAbility model

    else
        -- there is still some movement left
        -- so we adjust our current ability
        let
            oldLevelState =
                model.levelState

            updatedMovementAbility =
                Move stepsLeft

            updatedLevelResult =
                updateLevelForMovementAbility updatedMovementAbility oldLevelState.heroSpot oldLevelState.level
        in
        case updatedLevelResult of
            Err err ->
                ( ErrorModel err, Cmd.none )

            Ok updatedLevel ->
                let
                    newLevelState =
                        { oldLevelState
                            | level = updatedLevel
                            , gameMode = CardAction (Movement updatedMovementAbility)
                        }
                in
                ( OkModel <|
                    { model | levelState = newLevelState }
                , Cmd.none
                )


handleTwiceClickedCard : Model -> Int -> ( BaseModel, Cmd Msg )
handleTwiceClickedCard model cardNumber =
    let
        activateCardResult =
            activateDoubleClickedCard cardNumber model.cardState.heroCards
    in
    -- idea keep 3 stacks of cards unused, used, discarded.
    case activateCardResult of
        Err err ->
            ( ErrorModel err, Cmd.none )

        Ok ( newCards, activeCard, firstAbility ) ->
            let
                newCardState =
                    CardState newCards activeCard

                oldLevelState =
                    model.levelState

                newLevelResult =
                    updateLevelForCardAbility firstAbility model.levelState.heroSpot oldLevelState.level
            in
            case newLevelResult of
                Ok newLevel ->
                    let
                        finishedLevelState =
                            { oldLevelState | maybeClickedCard = Nothing, level = newLevel, gameMode = CardAction firstAbility }
                    in
                    ( OkModel { model | levelState = finishedLevelState, cardState = newCardState }, Cmd.none )

                Err err ->
                    ( ErrorModel err, Cmd.none )


addClickedCardNumberToModel : Int -> Model -> Model
addClickedCardNumberToModel number model =
    updateLevelState model (\state -> { state | maybeClickedCard = Just number })


updateLevelState : Model -> (LevelState -> LevelState) -> Model
updateLevelState model transform =
    { model | levelState = transform model.levelState }


makeReadyForMoveAnimation : Model -> MapCoordinate -> Int -> ( BaseModel, Cmd Msg )
makeReadyForMoveAnimation model newHeroSpot movement =
    let
        makeReadyResult =
            makeLevelStateReadyForMovementAnimation model.levelState
    in
    case makeReadyResult of
        Err err ->
            ( ErrorModel err, Cmd.none )

        Ok readyLevelState ->
            case model.levelState.level.changedMapCoordinatesForTempRooms of
                Nothing ->
                    ( ErrorModel "No changed cells for move animation", Cmd.none )

                Just changedCoordinates ->
                    let
                        currentHeroSpot =
                            readyLevelState.heroSpot

                        levelStateWithNewHeroSpot =
                            { readyLevelState | heroSpot = newHeroSpot }
                    in
                    handleMoveAnimation { model | levelState = levelStateWithNewHeroSpot } currentHeroSpot changedCoordinates movement


makeReadyForJumpAnimation : Model -> MapCoordinate -> ( BaseModel, Cmd Msg )
makeReadyForJumpAnimation model newHeroSpot =
    let
        makeReadyResult =
            makeLevelStateReadyForMovementAnimation model.levelState
    in
    case makeReadyResult of
        Err err ->
            ( ErrorModel err, Cmd.none )

        Ok readyLevelState ->
            let
                currentHeroSpot =
                    readyLevelState.heroSpot

                levelStateWithNewHeroSpot =
                    { readyLevelState | heroSpot = newHeroSpot }
            in
            handleJumpAnimation { model | levelState = levelStateWithNewHeroSpot } currentHeroSpot newHeroSpot


handleJumpAnimation : Model -> MapCoordinate -> MapCoordinate -> ( BaseModel, Cmd Msg )
handleJumpAnimation model start end =
    let
        makeJumpAnimationResult =
            makeJumpAnimation start end model.levelState.level.rooms
    in
    case makeJumpAnimationResult of
        Err err ->
            ( ErrorModel err, Cmd.none )

        Ok jumpAnimation ->
            let
                -- TODO
                calculateLengthOfJump =
                    3

                nextCommand =
                    Process.sleep (toFloat <| jumpAnimationDuration) |> Task.perform (always (MovementAnimationIsDone 0))
            in
            ( OkModel { model | animation = jumpAnimation }, nextCommand )


handleMoveAnimation : Model -> MapCoordinate -> List MapCoordinate -> Int -> ( BaseModel, Cmd Msg )
handleMoveAnimation model heroSpot restOfPath movement =
    let
        makeMoveAnimationResult =
            makeMoveAnimation heroSpot restOfPath model.levelState.level.rooms
    in
    case makeMoveAnimationResult of
        Err err ->
            ( ErrorModel err, Cmd.none )

        Ok moveAnimation ->
            let
                steps =
                    List.length restOfPath

                nextCommand =
                    Process.sleep (toFloat <| steps * moveAnimationDuration) |> Task.perform (always (MovementAnimationIsDone movement))
            in
            ( OkModel { model | animation = moveAnimation }, nextCommand )
