module Views.MapButtons exposing (..)

import FeatherIcons
import Functions.DictFunctions.RoomDict exposing (getRoomFromRoomDict)
import Functions.Room exposing (tryGetClosedDoorNumber)
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Models.BaseModel exposing (AnimationType(..))
import Models.CardState exposing (CardAbility(..), MovementAbility(..))
import Models.LevelState exposing (GameMode(..), LevelState)


buttonsView : LevelState -> AnimationType -> Float -> Html Msg
buttonsView state animation mapWidth =
    let
        buttons =
            if animation /= NoAnimation then
                [ resetMapButton ]

            else
                let
                    ( skipButton, isMovement ) =
                        case state.gameMode of
                            CardAction cardAbility ->
                                case cardAbility of
                                    Movement movementAbility ->
                                        case movementAbility of
                                            Move _ ->
                                                ( skipMovementButton, True )

                                            Jump _ ->
                                                ( skipMovementButton, True )

                                    Attack _ ->
                                        ( skipAttackButton, False )

                            ChooseCard ->
                                ( noButton, False )

                    openDoorBut =
                        if isMovement then
                            -- only when ability is move and there is a closed door
                            case getRoomFromRoomDict state.heroSpot.roomNumber state.level.rooms of
                                Err err ->
                                    div [] [ text err ]

                                Ok heroRoom ->
                                    let
                                        ( heroSpotHasClosedDoor, doorNumber ) =
                                            tryGetClosedDoorNumber state.heroSpot.roomCoordinate heroRoom
                                    in
                                    if heroSpotHasClosedDoor then
                                        openDoorButton doorNumber

                                    else
                                        noButton

                        else
                            noButton
                in
                [ resetMapButton, skipButton, openDoorBut ]
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "left" (String.fromFloat (mapWidth - 20) ++ "px")
        , Attr.style "top" "0"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        ]
        buttons


noButton : Html Msg
noButton =
    div [] []


openDoorButton : Int -> Html Msg
openDoorButton number =
    baseButton (OpenDoor number) (FeatherIcons.logIn |> FeatherIcons.toHtml [])


resetMapButton : Html Msg
resetMapButton =
    baseButton ResetMap (FeatherIcons.maximize |> FeatherIcons.toHtml [])


skipMovementButton : Html Msg
skipMovementButton =
    baseButton SkipMovement (FeatherIcons.fastForward |> FeatherIcons.toHtml [])


skipAttackButton : Html Msg
skipAttackButton =
    baseButton SkipAttack (FeatherIcons.fastForward |> FeatherIcons.toHtml [])


baseButton : Msg -> Html Msg -> Html Msg
baseButton action buttonTxt =
    button (baseButtonStyle action) [ buttonTxt ]


baseButtonStyle action =
    [ onClick action
    , Attr.style "color" "white"
    , Attr.style "background-color" "#4CAF50"
    , Attr.style "border" "1px solid black"
    , Attr.style "padding" "5px 10px"
    , Attr.style "font-size" "30px"
    ]
