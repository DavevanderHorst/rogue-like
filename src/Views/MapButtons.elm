module Views.MapButtons exposing (..)

import FeatherIcons
import Functions.DictFunctions.RoomDict exposing (getGridCellFromRoomDict)
import Html exposing (Html, button, div)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Models.BaseModel exposing (AnimationType(..))
import Models.CardState exposing (CardAbility(..))
import Models.LevelState exposing (GameMode(..), LevelState)


buttonsView : LevelState -> AnimationType -> Float -> Html Msg
buttonsView state animation mapWidth =
    if animation /= NoAnimation then
        div [] []

    else
        let
            skipButton =
                case state.gameMode of
                    CardAction cardAbility ->
                        case cardAbility of
                            Move _ ->
                                skipMovementButton True

                            Attack _ ->
                                skipAttackButton True

                    ChooseCard ->
                        skipMovementButton False

            openDoorBut =
                case getGridCellFromRoomDict state.heroSpot state.level.rooms of
                    Err _ ->
                        openDoorButton False 0

                    Ok heroGridCell ->
                        case heroGridCell.maybeGridDoorDetails of
                            Nothing ->
                                openDoorButton False 0

                            Just door ->
                                if door.doorIsOpen then
                                    openDoorButton False 0

                                else
                                    openDoorButton True door.doorNumber
        in
        div
            [ Attr.style "position" "absolute"
            , Attr.style "left" (String.fromFloat (mapWidth - 20) ++ "px")
            , Attr.style "top" "0"
            , Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            ]
            [ resetMapButton, skipButton, openDoorBut ]


openDoorButton : Bool -> Int -> Html Msg
openDoorButton isVisible number =
    baseButton (OpenDoor number) (FeatherIcons.logIn |> FeatherIcons.toHtml []) isVisible


resetMapButton : Html Msg
resetMapButton =
    baseButton ResetMap (FeatherIcons.maximize |> FeatherIcons.toHtml []) True


skipMovementButton : Bool -> Html Msg
skipMovementButton isVisible =
    baseButton SkipMovement (FeatherIcons.fastForward |> FeatherIcons.toHtml []) isVisible


skipAttackButton : Bool -> Html Msg
skipAttackButton isVisible =
    baseButton SkipAttack (FeatherIcons.fastForward |> FeatherIcons.toHtml []) isVisible


baseButton : Msg -> Html Msg -> Bool -> Html Msg
baseButton action buttonTxt isVisible =
    let
        buttonStyle =
            if isVisible then
                baseButtonStyle action

            else
                Attr.style "display" "none" :: baseButtonStyle action
    in
    button buttonStyle [ buttonTxt ]


baseButtonStyle action =
    [ onClick action
    , Attr.style "color" "white"
    , Attr.style "background-color" "#4CAF50"
    , Attr.style "border" "1px solid black"
    , Attr.style "padding" "5px 10px"
    , Attr.style "font-size" "30px"
    ]
