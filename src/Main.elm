module Main exposing (main)

import Browser
import Html exposing (div)
import Html.Attributes
import Mapbox.Element exposing (..)
import Styles.Dark exposing (style)



-- import Styles.Normal exposing (style)


main =
    div [ Html.Attributes.style "height" "100vh" ]
        [ map [] style ]
