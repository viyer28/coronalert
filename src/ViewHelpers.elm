-- module ViewHelpers: helpers for creating the view


module ViewHelpers exposing (..)

import Colors exposing (..)
import Element exposing (..)
import Html.Events
import Json.Decode as D
import Json.Encode as E
import Mapbox.Element exposing (..)
import Mapbox.Expression as Expr
import Types exposing (..)



-- finds the max confirmed cases in a list of records for sizing map circles


maxConfirmed : List RecordEntry -> Int
maxConfirmed records =
    Maybe.withDefault 0
        (List.maximum <| List.map (\r -> r.stats.confirmed) records)



-- finds the min confirmed cases in a list of records for sizing map circles


minConfirmed : List RecordEntry -> Int
minConfirmed records =
    Maybe.withDefault 0
        (List.minimum <| List.map (\r -> r.stats.confirmed) records)



-- if a feature is a country, size it based on zoom level and confirmed cases


countrySize : Int -> Int -> Expr.Expression Expr.CameraExpression Float
countrySize min max =
    let
        confirmedSize =
            Expr.getProperty (Expr.str "confirmed")
                |> Expr.toFloat 0
                |> Expr.interpolate (Expr.Exponential 0.99995)
                    [ ( toFloat min, Expr.int 5 )
                    , ( toFloat max, Expr.int 50 )
                    ]
    in
    Expr.zoom
        |> Expr.interpolate (Expr.Exponential 1.25)
            [ ( 0
              , Expr.multiply
                    (Expr.float 0.1)
                    confirmedSize
              )
            , ( 19
              , Expr.multiply
                    (Expr.float 20)
                    confirmedSize
              )
            ]



-- if a feature is a country, make it more opaque when it is hovered on at any zoom level


countryOpacity : Expr.Expression Expr.DataExpression Float
countryOpacity =
    Expr.ifElse (Expr.toBool (Expr.featureState (Expr.str "hover")))
        (Expr.float 0.9)
        (Expr.float 0.5)



-- if a feature is a state, size it based on zoom level and confirmed cases


stateSize : Int -> Int -> Expr.Expression Expr.CameraExpression Float
stateSize min max =
    let
        confirmedSize =
            Expr.getProperty (Expr.str "confirmed")
                |> Expr.toFloat 0
                |> Expr.interpolate (Expr.Exponential 0.99995)
                    [ ( toFloat min, Expr.int 5 )
                    , ( toFloat max, Expr.int 50 )
                    ]
    in
    Expr.zoom
        |> Expr.interpolate (Expr.Exponential 1.25)
            [ ( 0
              , Expr.multiply
                    (Expr.float 0.1)
                    confirmedSize
              )
            , ( 19
              , Expr.multiply
                    (Expr.float 20)
                    confirmedSize
              )
            ]



-- if a feature is a state, make it more opaque when it is hovered on at zoom levels less than 5.25, otherwise hide it


stateOpacity : Expr.Expression Expr.CameraExpression Float
stateOpacity =
    Expr.zoom
        |> Expr.interpolate Expr.Linear
            [ ( 0
              , Expr.ifElse
                    (Expr.toBool (Expr.featureState (Expr.str "hover")))
                    (Expr.float 0.9)
                    (Expr.float 0.5)
              )
            , ( 5
              , Expr.ifElse
                    (Expr.toBool (Expr.featureState (Expr.str "hover")))
                    (Expr.float 0.9)
                    (Expr.float 0.5)
              )
            , ( 5.25, Expr.float 0 )
            ]



-- if a feature is a county, size it based on zoom level and confirmed cases


countySize : Int -> Int -> Expr.Expression Expr.CameraExpression Float
countySize min max =
    let
        confirmedSize =
            Expr.getProperty (Expr.str "confirmed")
                |> Expr.toFloat 0
                |> Expr.interpolate (Expr.Exponential 0.99995)
                    [ ( toFloat min, Expr.float 5 )
                    , ( toFloat max, Expr.int 50 )
                    ]
    in
    Expr.zoom
        |> Expr.interpolate (Expr.Exponential 0.999999)
            [ ( 5
              , Expr.multiply
                    (Expr.float 0.5)
                    confirmedSize
              )
            , ( 23
              , Expr.multiply
                    (Expr.float 10)
                    confirmedSize
              )
            ]



-- if a feature is a county, make it more opaque when it is hovered on at zoom levels greater than 5.25, otherwise hide it


countyOpacity : Expr.Expression Expr.CameraExpression Float
countyOpacity =
    Expr.zoom
        |> Expr.interpolate Expr.Linear
            [ ( 0, Expr.float 0 )
            , ( 5, Expr.float 0 )
            , ( 5.25
              , Expr.ifElse
                    (Expr.toBool (Expr.featureState (Expr.str "hover")))
                    (Expr.float 0.9)
                    (Expr.float 0.5)
              )
            ]



-- determine a feature's color based on number of confirmed cases


featureColor : Expr.Expression Expr.DataExpression Expr.Color
featureColor =
    Expr.conditionally
        [ ( Expr.lessThanOrEqual
                (Expr.getProperty (Expr.str "confirmed"))
                (Expr.float 1000)
          , greenExpr
          )
        , ( Expr.lessThanOrEqual
                (Expr.getProperty (Expr.str "confirmed"))
                (Expr.float 10000)
          , yellowExpr
          )
        ]
        redExpr



-- set the hover state for a feature if it is hovered on


hoverFeature : Maybe E.Value -> MapboxAttr msg
hoverFeature feature =
    case feature of
        Nothing ->
            featureState []

        Just feat ->
            featureState [ ( feat, [ ( "hover", E.bool True ) ] ) ]



-- a custom onEnter Attribute for Element inputs


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (D.field "key" D.string
                |> D.andThen
                    (\key ->
                        if key == "Enter" then
                            D.succeed msg

                        else
                            D.fail "Not the enter key"
                    )
            )
        )
