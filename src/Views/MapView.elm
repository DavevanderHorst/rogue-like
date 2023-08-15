module Views.MapView exposing (..)

import Colors exposing (blackColorString, canBeAttackedColorString, canBeClickedColorString, closedDoorColor, isClickedColorString, isPartOfMovePathColorString, openDoorColor, whiteColorString)
import Constants exposing (cellMargin, cellWidth, cellWidthString, halfCellMargin, halfJumpAnimationDuration, horizontalGridPolygon, moveAnimationDuration)
import Dict
import Draggable
import Html exposing (div, text)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Math.Vector2 as Vec2
import Messages exposing (Msg(..))
import Models.BaseModel exposing (AnimationType(..), Model, Size)
import Models.CardState exposing (CardAbility(..))
import Models.LevelState exposing (CellState(..), Door, FigureType(..), GameMode(..), GridCell, LevelState, MonsterType(..), MovementType(..), Room)
import Models.Others exposing (Point)
import Simple.Animation as Animation exposing (Animation, Step)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttr
import Svg.Events
import Views.MapButtons exposing (buttonsView)


mapView : Model -> Svg Msg
mapView model =
    let
        ( center, size, zoom ) =
            ( model.center, model.mapSize, model.zoom )

        ( cx, cy ) =
            ( Vec2.getX center, Vec2.getY center )

        ( halfWidth, halfHeight ) =
            ( size.width / zoom / 2, size.height / zoom / 2 )

        ( top, left ) =
            ( cy - halfHeight, cx - halfWidth )

        panning =
            "translate(" ++ String.fromFloat -left ++ ", " ++ String.fromFloat -top ++ ")"

        zooming =
            "scale(" ++ String.fromFloat zoom ++ ")"

        mapText =
            makeMapText model.levelState.gameMode
    in
    div [ Attr.style "position" "relative" ]
        [ div
            [ Attr.style "position" "absolute"
            , Attr.style "margin" (calculateMapMarginSize model.windowSize)
            , Attr.style "text-shadow" "-1px 0 white, 0 1px white, 1px 0 white, 0 -1px white"
            , Attr.style "font-size" (calculateMapFondSize model.windowSize)
            ]
            [ text mapText ]
        , Svg.svg
            [ svgAttrFloat SvgAttr.width size.width
            , svgAttrFloat SvgAttr.height size.height

            --, SvgAttr.style "border: 4mm ridge rgba(133, 220, 50, .6);"
            , handleZoom Zoom
            , Draggable.mouseTrigger () DragMsg
            ]
            [ backgroundView
            , Svg.g
                [ SvgAttr.transform (zooming ++ " " ++ panning)
                , SvgAttr.fill "none"
                ]
                (renderLevel model.levelState model.animation)
            ]
        , buttonsView model.levelState model.animation size.width
        ]


handleAnimation : AnimationType -> Svg Msg
handleAnimation animation =
    case animation of
        NoAnimation ->
            Svg.g [] []

        AnimationMove startPoint rest ->
            animatedG (moveAnimation startPoint rest) [] [ renderHeroCell ]

        AnimationJump start middle end ->
            animatedG (jumpAnimation start middle end) [] [ renderHeroCell ]



-- Svg Animated Helpers


jumpAnimation : Point -> Point -> Point -> Animation
jumpAnimation start middle end =
    Animation.steps
        { startAt = [ P.x start.x, P.y start.y ]
        , options = []
        }
        [ Animation.step halfJumpAnimationDuration [ P.x middle.x, P.y middle.y, P.scale 2 ]
        , Animation.step halfJumpAnimationDuration [ P.x end.x, P.y end.y ]
        ]


moveAnimation : Point -> List Point -> Animation
moveAnimation start rest =
    Animation.steps
        { startAt = [ P.x start.x, P.y start.y ]
        , options = []
        }
        (List.map makeAnimationStep rest)


makeAnimationStep : Point -> Step
makeAnimationStep point =
    Animation.step moveAnimationDuration [ P.x point.x, P.y point.y ]


animatedG : Animation -> List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
animatedG =
    animatedSvg Svg.g


animatedSvg =
    Animated.svg
        { class = SvgAttr.class
        }


calculateMapFondSize : Size -> String
calculateMapFondSize windowSize =
    "40px"


calculateMapMarginSize : Size -> String
calculateMapMarginSize windowSize =
    "10px"


renderLevel : LevelState -> AnimationType -> List (Svg Msg)
renderLevel levelState animation =
    let
        level =
            levelState.level

        roomsToRender =
            case level.tempUpdatedRooms of
                Nothing ->
                    level.rooms

                Just tempRooms ->
                    tempRooms

        startSvgList =
            handleAnimation animation :: []

        svgListWithDoors =
            Dict.foldl renderDoor startSvgList level.doors
    in
    Dict.foldl renderRoom svgListWithDoors roomsToRender


renderDoor : Int -> Door -> List (Svg Msg) -> List (Svg Msg)
renderDoor _ value svgList =
    let
        measurements =
            value.measurements

        color =
            if value.isOpen then
                openDoorColor

            else
                closedDoorColor
    in
    Svg.rect
        [ SvgAttr.fill color
        , svgAttrInt SvgAttr.x measurements.startX
        , svgAttrInt SvgAttr.y measurements.startY
        , svgAttrInt SvgAttr.width measurements.width
        , svgAttrInt SvgAttr.height measurements.height
        ]
        []
        :: svgList


renderRoom : Int -> Room -> List (Svg Msg) -> List (Svg Msg)
renderRoom _ value svgList =
    let
        measurements =
            value.measurements

        roomRect : Svg Msg
        roomRect =
            Svg.rect
                [ SvgAttr.fill blackColorString
                , svgAttrInt SvgAttr.x measurements.startX
                , svgAttrInt SvgAttr.y measurements.startY
                , svgAttrInt SvgAttr.width measurements.width
                , svgAttrInt SvgAttr.height measurements.height
                ]
                []

        roomGridCellsList : List (Svg Msg)
        roomGridCellsList =
            if value.isOpen then
                renderGridCells value

            else
                []
    in
    List.append (roomRect :: roomGridCellsList) svgList


renderGridCells : Room -> List (Svg Msg)
renderGridCells room =
    Dict.foldl renderGridCell [] room.gridCells


renderGridCell : String -> GridCell -> List (Svg Msg) -> List (Svg Msg)
renderGridCell _ value svgList =
    case value.cellState of
        Empty ->
            let
                attributes =
                    SvgAttr.fill whiteColorString :: baseGridCellAttributes value
            in
            Svg.rect attributes [] :: svgList

        FigureType figureType ->
            case figureType of
                Hero ->
                    renderHeroGridCell value :: svgList

                Monster monsterType monsterNumber canBeAttacked ->
                    case monsterType of
                        Dummy ->
                            let
                                ( newSvgList, imageAttributes ) =
                                    if canBeAttacked then
                                        let
                                            extraRect =
                                                let
                                                    attributes =
                                                        SvgAttr.fill canBeAttackedColorString :: biggerBaseGridCellAttributes value
                                                in
                                                Svg.rect attributes []

                                            smallerAttributes =
                                                SvgAttr.xlinkHref "Images/dummy.png" :: smallerBaseGridCellAttributes value
                                        in
                                        ( extraRect :: svgList, smallerAttributes )

                                    else
                                        let
                                            attributes =
                                                SvgAttr.xlinkHref "Images/dummy.png" :: baseGridCellAttributes value
                                        in
                                        ( svgList, attributes )
                            in
                            Svg.image imageAttributes [] :: newSvgList

        Movement movementType ->
            case movementType of
                ClickedForMovement steps ->
                    let
                        clickableRect =
                            createClickableRect isClickedColorString value steps
                    in
                    clickableRect :: svgList

                CanBeMovedTo steps ->
                    let
                        clickableRect =
                            createClickableRect canBeClickedColorString value steps
                    in
                    clickableRect :: svgList

                CanBeJumpedTo distance ->
                    let
                        clickableRect =
                            createClickableRect canBeClickedColorString value distance
                    in
                    clickableRect :: svgList

                IsPartOfMovePath steps ->
                    let
                        clickableRect =
                            createClickableRect isPartOfMovePathColorString value steps
                    in
                    clickableRect :: svgList


renderHeroCell : Svg Msg
renderHeroCell =
    let
        attributes =
            SvgAttr.xlinkHref "Images/swordsman.png" :: heroCellAttributes
    in
    Svg.image attributes []


renderHeroGridCell : GridCell -> Svg Msg
renderHeroGridCell gridCell =
    let
        attributes =
            SvgAttr.xlinkHref "Images/swordsman.png" :: baseGridCellAttributes gridCell
    in
    Svg.image attributes []


createClickableRect : String -> GridCell -> Int -> Svg Msg
createClickableRect color cell steps =
    let
        attributes =
            Svg.Events.onClick (MapIsClicked cell.mapCoordinate)
                :: SvgAttr.fill color
                :: baseGridCellAttributes cell
    in
    Svg.rect attributes [ text (String.fromInt steps) ]


smallerBaseGridCellAttributes : GridCell -> List (Attribute msg)
smallerBaseGridCellAttributes gridCell =
    [ SvgAttr.clipPath gridCell.polygonShape
    , SvgAttr.width (smallerCellWidthString 8)
    , SvgAttr.height (smallerCellWidthString 8)
    , svgAttrInt SvgAttr.y (gridCell.startY + 4)
    , svgAttrInt SvgAttr.x (gridCell.startX + 4)
    ]


biggerBaseGridCellAttributes : GridCell -> List (Attribute msg)
biggerBaseGridCellAttributes gridCell =
    [ SvgAttr.clipPath gridCell.polygonShape
    , SvgAttr.width biggerCellWidthString
    , SvgAttr.height biggerCellWidthString
    , svgAttrInt SvgAttr.y (gridCell.startY - halfCellMargin)
    , svgAttrInt SvgAttr.x (gridCell.startX - halfCellMargin)
    ]


smallerCellWidthString : Int -> String
smallerCellWidthString smaller =
    String.fromInt (cellWidth - smaller) ++ "px"


biggerCellWidthString : String
biggerCellWidthString =
    String.fromInt (cellWidth + cellMargin) ++ "px"


baseGridCellAttributes : GridCell -> List (Attribute msg)
baseGridCellAttributes gridCell =
    [ SvgAttr.clipPath gridCell.polygonShape
    , SvgAttr.width cellWidthString
    , SvgAttr.height cellWidthString
    , svgAttrInt SvgAttr.y gridCell.startY
    , svgAttrInt SvgAttr.x gridCell.startX
    ]


heroCellAttributes : List (Attribute msg)
heroCellAttributes =
    [ SvgAttr.clipPath horizontalGridPolygon
    , SvgAttr.width cellWidthString
    , SvgAttr.height cellWidthString
    ]


svgAttrInt : (String -> Svg.Attribute msg) -> Int -> Svg.Attribute msg
svgAttrInt attr value =
    attr (String.fromInt value)


svgAttrFloat : (String -> Svg.Attribute msg) -> Float -> Svg.Attribute msg
svgAttrFloat attr value =
    attr (String.fromFloat value)


backgroundView : Svg Msg
backgroundView =
    Svg.rect [ SvgAttr.x "0", SvgAttr.y "0", SvgAttr.width "100%", SvgAttr.height "100%", SvgAttr.fill "#eee" ] []



-- Zoom


handleZoom : (Float -> msg) -> Svg.Attribute msg
handleZoom onZoom =
    let
        alwaysPreventDefaultAndStopPropagation msg =
            { message = msg, stopPropagation = True, preventDefault = True }

        zoomDecoder : Decoder msg
        zoomDecoder =
            Decode.float
                |> Decode.field "deltaY"
                |> Decode.map onZoom
    in
    Html.Events.custom
        "wheel"
    <|
        Decode.map alwaysPreventDefaultAndStopPropagation zoomDecoder


makeMapText : GameMode -> String
makeMapText mode =
    case mode of
        CardAction ability ->
            case ability of
                Move int ->
                    "Move " ++ String.fromInt int

                Jump int ->
                    "Jump " ++ String.fromInt int

                Attack int ->
                    "Attack " ++ String.fromInt int

        ChooseCard ->
            "Pick your next card"
