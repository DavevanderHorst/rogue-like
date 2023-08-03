module Views.ViewHelpers exposing (..)

import Html


attrFloat : (String -> Html.Attribute msg) -> Float -> Html.Attribute msg
attrFloat attr value =
    attr (String.fromFloat value ++ "px")
