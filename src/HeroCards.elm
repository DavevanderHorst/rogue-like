module HeroCards exposing (..)

import Models.CardState exposing (AttackAbility(..), CardAbility(..), HeroCard, MovementAbility(..))


startHeroCards : List HeroCard
startHeroCards =
    [ moveExample 1 1 2, moveExample 2 3 4, simpleCard, jumpCard, attackCard ]


simpleCard : HeroCard
simpleCard =
    { isActivated = False
    , cardNumber = 3
    , abilities = [ Movement (Move 5) ]
    }


attackCard : HeroCard
attackCard =
    { isActivated = False
    , cardNumber = 5
    , abilities = [ Attack (Sword 4) ]
    }


moveExample : Int -> Int -> Int -> HeroCard
moveExample cardNumber ab1 ab2 =
    { isActivated = False
    , cardNumber = cardNumber
    , abilities = [ Movement (Move ab1), Movement (Move ab2), Movement (Move 5) ]
    }


jumpCard : HeroCard
jumpCard =
    { isActivated = False
    , cardNumber = 4
    , abilities = [ Movement (Move 2), Movement (Jump 3) ]
    }


emptyHeroCard : HeroCard
emptyHeroCard =
    { isActivated = False
    , cardNumber = 0
    , abilities = []
    }


emptyAbility : CardAbility
emptyAbility =
    Movement (Move 0)
