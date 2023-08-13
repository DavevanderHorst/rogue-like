module HeroCards exposing (..)

import Models.CardState exposing (CardAbility(..), HeroCard)


startHeroCards : List HeroCard
startHeroCards =
    [ moveExample 1 1 2, moveExample 2 3 4, simpleCard, jumpCard ]


simpleCard : HeroCard
simpleCard =
    { isActivated = False
    , cardNumber = 3
    , abilities = [ Move 5 ]
    }


moveExample : Int -> Int -> Int -> HeroCard
moveExample cardNumber ab1 ab2 =
    { isActivated = False
    , cardNumber = cardNumber
    , abilities = [ Move ab1, Move ab2, Move 5 ]
    }


jumpCard : HeroCard
jumpCard =
    { isActivated = False
    , cardNumber = 4
    , abilities = [ Move 2, Jump 6 ]
    }


emptyHeroCard : HeroCard
emptyHeroCard =
    { isActivated = False
    , cardNumber = 0
    , abilities = []
    }


emptyAbility : CardAbility
emptyAbility =
    Move 0
