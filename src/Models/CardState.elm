module Models.CardState exposing (..)


type alias CardState =
    { heroCards : List HeroCard
    , activeCard : HeroCard
    }


type alias HeroCard =
    { isActivated : Bool
    , cardNumber : Int
    , abilities : List CardAbility
    }


type CardAbility
    = Movement MovementAbility
    | Attack AttackAbility


type
    MovementAbility
    -- Move can be done in multiple steps
    = Move Int
      -- Jump is one jump, when you land, and you didnt use all your movement, its gone
    | Jump Int


type AttackAbility
    = Sword Int
    | Spear Int
