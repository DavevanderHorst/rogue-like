module Functions.HeroCards exposing (..)

import HeroCards exposing (emptyAbility, emptyHeroCard)
import Models.CardState exposing (CardAbility, HeroCard)



-- TODO check this, and dont start with empty abilities


activateDoubleClickedCard : Int -> List HeroCard -> Result String ( List HeroCard, HeroCard, CardAbility )
activateDoubleClickedCard cardNumber heroCards =
    let
        emptyResultTuple =
            Ok ( [], emptyHeroCard, emptyAbility )
    in
    List.foldr (updateCardStateForActivatedCard cardNumber) emptyResultTuple heroCards


updateCardStateForActivatedCard : Int -> HeroCard -> Result String ( List HeroCard, HeroCard, CardAbility ) -> Result String ( List HeroCard, HeroCard, CardAbility )
updateCardStateForActivatedCard cardNumber card result =
    case result of
        Err err ->
            Err err

        Ok ( heroCards, heroCard, activeAbility ) ->
            if cardNumber /= card.cardNumber then
                Ok ( card :: heroCards, heroCard, activeAbility )

            else
                let
                    oldAbilities =
                        card.abilities

                    firstAbility =
                        Maybe.withDefault emptyAbility (List.head oldAbilities)

                    newAbilities =
                        List.drop 1 oldAbilities

                    newCard =
                        { card | isActivated = True, abilities = newAbilities }
                in
                Ok ( newCard :: heroCards, newCard, firstAbility )
