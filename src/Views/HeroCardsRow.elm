module Views.HeroCardsRow exposing (..)

import Colors exposing (blackColorString, canBeClickedColorString, isClickedColorString)
import Constants exposing (cardRowHeight)
import Html exposing (Attribute, Html, div, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Models.BaseModel exposing (Model)
import Models.CardState exposing (CardAbility(..), HeroCard)
import Models.LevelState exposing (GameMode(..), LevelState)


heroCardsView : Model -> Html Msg
heroCardsView model =
    div
        [ Attr.style "display" "flex"
        , Attr.style "width" "99%"
        , Attr.style "height" cardRowHeight
        , Attr.style "padding" "0 0.5%"
        ]
        (generateHeroCards model)


generateHeroCards : Model -> List (Html Msg)
generateHeroCards model =
    let
        availableCards =
            List.filter (\card -> card.isActivated == False) model.cardState.heroCards
    in
    List.map (generateHeroCard model.levelState) availableCards


generateHeroCard : LevelState -> HeroCard -> Html Msg
generateHeroCard state heroCard =
    let
        attributes =
            case state.gameMode of
                ChooseCard ->
                    case state.clickedCard of
                        Nothing ->
                            canBeClickedHeroCardAttributes heroCard.cardNumber

                        Just clickedCardNumber ->
                            if clickedCardNumber == heroCard.cardNumber then
                                isClickedHeroCardAttributes heroCard.cardNumber

                            else
                                canBeClickedHeroCardAttributes heroCard.cardNumber

                _ ->
                    baseHeroCardAttributes heroCard.cardNumber
    in
    div attributes (createAbilities heroCard)


isClickedHeroCardAttributes : Int -> List (Attribute Msg)
isClickedHeroCardAttributes cardNumber =
    upgradedHeroCardAttributes cardNumber isClickedColorString


canBeClickedHeroCardAttributes : Int -> List (Attribute Msg)
canBeClickedHeroCardAttributes cardNumber =
    upgradedHeroCardAttributes cardNumber canBeClickedColorString


upgradedHeroCardAttributes : Int -> String -> List (Attribute Msg)
upgradedHeroCardAttributes cardNumber color =
    Attr.style "background" color :: onClick (CardIsClicked cardNumber) :: baseHeroCardAttributes cardNumber


baseHeroCardAttributes : Int -> List (Attribute Msg)
baseHeroCardAttributes cardNumber =
    [ Attr.style "border-radius" "2%"
    , Attr.style "height" "100%"
    , Attr.style "width" "7%"
    , Attr.style "margin-right" "1%"
    , Attr.style "display" "flex"
    , Attr.style "flex-direction" "column"
    , Attr.style "justify-content" "space-around"
    , Attr.style "border" ("2px solid " ++ blackColorString)
    ]


createAbilities : HeroCard -> List (Html Msg)
createAbilities card =
    List.map makeAbilityDiv card.abilities


makeAbilityDiv : CardAbility -> Html Msg
makeAbilityDiv ability =
    div
        [ Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        ]
        [ text (makeAbilityText ability) ]


makeAbilityText : CardAbility -> String
makeAbilityText ability =
    case ability of
        Move int ->
            "Move: " ++ String.fromInt int

        Attack int ->
            "Attack: " ++ String.fromInt int
