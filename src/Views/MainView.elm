module Views.MainView exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Messages exposing (Msg)
import Models.BaseModel exposing (BaseModel(..))
import Views.HeroCardsRow exposing (heroCardsView)
import Views.MapView exposing (mapView)
import Views.ViewHelpers exposing (attrFloat)


view : BaseModel -> Html Msg
view baseModel =
    case baseModel of
        OkModel model ->
            div
                [ attrFloat (Attr.style "width") model.windowSize.width
                , attrFloat (Attr.style "height") model.windowSize.height
                ]
                [ div
                    [ Attr.style "width" "99%"
                    , attrFloat (Attr.style "height") model.mapSize.height
                    , Attr.style "padding" "0.5%"
                    ]
                    [ mapView model
                    ]
                , heroCardsView model
                ]

        ErrorModel err ->
            div [] [ text err ]
