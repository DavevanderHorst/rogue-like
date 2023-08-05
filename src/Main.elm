module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events exposing (onResize)
import Constants exposing (mapSizeHeight, mapSizeWidth, startCenter, startSize, startZoom)
import Draggable
import Functions.Abilities exposing (handleNextAbility)
import Functions.Animations exposing (makeMoveAnimation)
import Functions.Coordinates exposing (isSameMapCoordinate)
import Functions.DictFunctions.RoomDict exposing (getStepsToMoveTowardsClickedCell)
import Functions.HeroCards exposing (activateDoubleClickedCard)
import Functions.Level exposing (moveHeroInLevel, resetMovementPathInTempRoomDictForLevel, setTempRoomsToNothingForLevel, updateLevelForAbility)
import Functions.LevelState exposing (makeLevelStateReadyForMoveAnimation)
import Functions.Movement exposing (makeMovementPathInTempRoomDictForLevel)
import HeroCards exposing (emptyHeroCard, startHeroCards)
import Levels.LevelOne exposing (heroStartMapCoordinate, levelOneResult)
import Math.Vector2 as Vector2
import Messages exposing (Msg(..))
import Models.BaseModel exposing (AnimationType(..), BaseModel(..), Model, Size)
import Models.CardState exposing (CardAbility(..), CardState)
import Models.LevelState exposing (GameMode(..), LevelState, MapCoordinate)
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
                , clickedCard = Nothing
                , heroSpot = heroStartMapCoordinate
                , maybeHeroSpotClosedDoorNumber = Nothing
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
                                    case ability of
                                        Move movement ->
                                            if isSameMapCoordinate formerClickedCell clickedCoordinate then
                                                -- same cell is clicked twice, so we move
                                                -- handleTwiceClickedMapCoordinateForMovement model clickedCoordinate movement
                                                handleMoveAnimation model

                                            else
                                                -- other cell is clicked
                                                handleNewlyClickedMapCoordinate model clickedCoordinate formerClickedCell

                                        Attack _ ->
                                            -- TODO
                                            ( ErrorModel "not implemented", Cmd.none )

                                ChooseCard ->
                                    ( ErrorModel "Choose card not possible in map is clicked", Cmd.none )

                CardIsClicked number ->
                    case model.levelState.clickedCard of
                        Nothing ->
                            ( OkModel <| addClickedCardNumberToModel number model, Cmd.none )

                        Just clickedCardNumber ->
                            if clickedCardNumber == number then
                                handleTwiceClickedCard model number

                            else
                                ( OkModel <| addClickedCardNumberToModel number model, Cmd.none )

                SkipMovement ->
                    --TODO
                    ( ErrorModel "to be implemented", Cmd.none )

                SkipAttack ->
                    --TODO
                    ( ErrorModel "to be implemented", Cmd.none )

                OpenDoor _ ->
                    --TODO
                    ( ErrorModel "to be implemented", Cmd.none )


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
    let
        updatedLevelResult =
            makeMovementPathInTempRoomDictForLevel clickedCoordinate model.levelState.level
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
                resetMovementPathInTempRoomDictForLevel currentLevel
        in
        case resetLevelResult of
            Err err ->
                ( ErrorModel err, Cmd.none )

            Ok resetLevel ->
                let
                    newLevelResult =
                        makeMovementPathInTempRoomDictForLevel new resetLevel
                in
                case newLevelResult of
                    Err err ->
                        ( ErrorModel err, Cmd.none )

                    Ok newLevel ->
                        ( OkModel <|
                            updateLevelState model
                                (\state ->
                                    { state
                                        | formerClickedCell = Just new
                                        , level = newLevel
                                    }
                                )
                        , Cmd.none
                        )


handleTwiceClickedMapCoordinateForMovement : Model -> MapCoordinate -> Int -> ( BaseModel, Cmd Msg )
handleTwiceClickedMapCoordinateForMovement model newHeroSpot movement =
    case model.levelState.level.tempUpdatedRooms of
        Nothing ->
            ( ErrorModel "There is no temporary saved room dict in : handleTwiceClickedMapCoordinateForMovement", Cmd.none )

        Just tempRoomDict ->
            let
                stepsMovedResult =
                    getStepsToMoveTowardsClickedCell newHeroSpot tempRoomDict
            in
            case stepsMovedResult of
                Err err ->
                    ( ErrorModel err, Cmd.none )

                Ok stepsMoved ->
                    let
                        oldLevelState =
                            model.levelState

                        moveHeroInLevelResult =
                            moveHeroInLevel oldLevelState.heroSpot newHeroSpot oldLevelState.level
                    in
                    case moveHeroInLevelResult of
                        Err err ->
                            ( ErrorModel err, Cmd.none )

                        Ok levelWithMovedHero ->
                            let
                                newLevelState =
                                    { oldLevelState
                                        | heroSpot = newHeroSpot
                                        , formerClickedCell = Nothing
                                        , level = setTempRoomsToNothingForLevel levelWithMovedHero
                                    }

                                movementLeft =
                                    movement - stepsMoved
                            in
                            if movementLeft == 0 then
                                handleNextAbility { model | levelState = newLevelState }

                            else
                                -- there is still some movement left
                                -- so we adjust our current ability
                                let
                                    updatedAbility =
                                        Move movementLeft

                                    updatedLevelResult =
                                        updateLevelForAbility updatedAbility newLevelState.heroSpot newLevelState.level
                                in
                                case updatedLevelResult of
                                    Err err ->
                                        ( ErrorModel err, Cmd.none )

                                    Ok updatedLevel ->
                                        let
                                            finishedLevelState =
                                                { newLevelState
                                                    | level = updatedLevel
                                                    , gameMode = CardAction updatedAbility
                                                }
                                        in
                                        ( OkModel <|
                                            { model | levelState = finishedLevelState }
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
                    updateLevelForAbility firstAbility model.levelState.heroSpot oldLevelState.level
            in
            case newLevelResult of
                Ok newLevel ->
                    let
                        finishedLevelState =
                            { oldLevelState | clickedCard = Nothing, level = newLevel, gameMode = CardAction firstAbility }
                    in
                    ( OkModel { model | levelState = finishedLevelState, cardState = newCardState }, Cmd.none )

                Err err ->
                    ( ErrorModel err, Cmd.none )


addClickedCardNumberToModel : Int -> Model -> Model
addClickedCardNumberToModel number model =
    updateLevelState model (\state -> { state | clickedCard = Just number })


updateLevelState : Model -> (LevelState -> LevelState) -> Model
updateLevelState model transform =
    { model | levelState = transform model.levelState }


handleMoveAnimation : Model -> ( BaseModel, Cmd Msg )
handleMoveAnimation model =
    let
        makeReadyResult =
            makeLevelStateReadyForMoveAnimation model.levelState
    in
    case makeReadyResult of
        Err err ->
            ( ErrorModel err, Cmd.none )

        Ok readyLevelState ->
            case model.levelState.level.changedMapCoordinatesForTempRooms of
                Nothing ->
                    ( ErrorModel "No changed cells for animation", Cmd.none )

                Just changedCoordinates ->
                    let
                        moveAnimationResult =
                            makeMoveAnimation model.levelState.heroSpot changedCoordinates model.levelState.level.rooms
                    in
                    case moveAnimationResult of
                        Err err ->
                            ( ErrorModel err, Cmd.none )

                        Ok moveAnimation ->
                            ( OkModel { model | levelState = readyLevelState, animation = moveAnimation }, Cmd.none )
