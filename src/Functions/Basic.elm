module Functions.Basic exposing (..)


isEven : Int -> Bool
isEven number =
    if modBy 2 number == 0 then
        True

    else
        False
