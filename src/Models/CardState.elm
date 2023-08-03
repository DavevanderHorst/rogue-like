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
    | Attack Int
