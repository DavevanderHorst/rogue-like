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
    = Move Int
      -- Move can be done in multiple steps
    | Jump Int
      -- Jump is one jump, when you land, and you didnt use all your movement, its gone
    | Attack Int
