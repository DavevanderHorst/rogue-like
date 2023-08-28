module Functions.Abilities exposing (..)

import Functions.Level exposing (updateLevelForCardAbility)
import Messages exposing (Msg)
import Models.BaseModel exposing (BaseModel(..), Model)
import Models.LevelState exposing (GameMode(..))


handleNextAbility : Model -> ( BaseModel, Cmd Msg )
handleNextAbility model =
    let
        nextMaybeAbility =
            List.head model.cardState.activeCard.abilities

        oldLevelState =
            model.levelState
    in
    case nextMaybeAbility of
        Nothing ->
            let
                finishedLevelState =
                    { oldLevelState
                        | gameMode = ChooseCard
                    }
            in
            ( OkModel <|
                { model | levelState = finishedLevelState }
            , Cmd.none
            )

        Just nextAbility ->
            -- activating next ability
            let
                updatedLevelResult =
                    updateLevelForCardAbility nextAbility oldLevelState.heroSpot oldLevelState.level
            in
            case updatedLevelResult of
                Err err ->
                    ( ErrorModel err, Cmd.none )

                Ok updatedLevel ->
                    --Todo function for card state
                    let
                        oldCard =
                            model.cardState.activeCard

                        oldCardState =
                            model.cardState

                        newCard =
                            { oldCard | abilities = List.drop 1 oldCard.abilities }

                        newCardState =
                            { oldCardState | activeCard = newCard }

                        finishedLevelState =
                            { oldLevelState
                                | level = updatedLevel
                                , gameMode = CardAction nextAbility
                            }
                    in
                    ( OkModel
                        { model
                            | cardState = newCardState
                            , levelState = finishedLevelState
                        }
                    , Cmd.none
                    )
