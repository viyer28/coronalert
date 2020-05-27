port module Main exposing (main, redExpr)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Http exposing (..)
import Json.Decode as D
import Json.Encode as E
import List exposing (map)
import LngLat exposing (LngLat)
import MapCommands
import Mapbox.Cmd.Option as Opt
import Mapbox.Element exposing (..)
import Mapbox.Expression as Expr
import Mapbox.Layer as Layer
import Mapbox.Source as Source
import Maybe.Extra as Extra
import PhoneNumber
import PhoneNumber.Countries
import Styles.Dark as Dark


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }



-- MODEL


type alias Model =
    { countries : List RecordEntry
    , states : List RecordEntry
    , counties : List RecordEntry
    , errorMessage : Maybe String
    , hoveredFeature : Maybe E.Value
    , hoveredEntry : Maybe FeatureEntry
    , hoverPoint : Maybe ( Int, Int )
    , clickedFeature : Maybe E.Value
    , clickedEntry : Maybe FeatureEntry
    , phoneNumber : String
    , validPhone : Maybe Bool
    }


type Msg
    = DataReceived (Result Http.Error (List RecordEntry))
    | Hover EventData
    | Click EventData
    | Touch TouchEvent
    | PhoneUpdate String
    | AlertMe


type alias CoordinatesEntry =
    { latitude : String
    , longitude : String
    }


type alias StatsEntry =
    { confirmed : Int
    , deaths : Int
    , recovered : Int
    }


type alias RecordEntry =
    { country : String
    , province : Maybe String
    , county : Maybe String
    , updatedAt : String
    , stats : StatsEntry
    , coordinates : CoordinatesEntry
    }


type alias GeometryEntry =
    { coordinates : List Float }


type alias PropertiesEntry =
    { country : String
    , province : String
    , county : Maybe String
    , updatedAt : String
    , confirmed : Int
    , deaths : Int
    , recovered : Int
    }


type alias LayerEntry =
    { id : String }


type alias FeatureEntry =
    { geometry : GeometryEntry
    , properties : PropertiesEntry
    , layer : LayerEntry
    }


type alias WriteEntry =
    { phoneNumber : String
    , country : String
    , province : String
    , county : Maybe String
    , id : String
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { countries = []
      , states = []
      , counties = []
      , errorMessage = Nothing
      , hoveredFeature = Nothing
      , hoveredEntry = Nothing
      , hoverPoint = Nothing
      , clickedFeature = Nothing
      , clickedEntry = Nothing
      , phoneNumber = ""
      , validPhone = Nothing
      }
    , Cmd.batch [ getLatestCountyData, getLatestWorldData ]
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataReceived (Ok records) ->
            if List.any isCountry records then
                ( { model
                    | countries = List.filter isCountry records
                    , states = List.filter isState records
                  }
                , Cmd.none
                )

            else
                ( { model
                    | counties = records
                  }
                , Cmd.none
                )

        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )

        Hover { point, lngLat, renderedFeatures } ->
            case renderedFeatures of
                [] ->
                    ( { model
                        | hoveredFeature = Nothing
                        , hoveredEntry = Nothing
                        , hoverPoint = Nothing
                      }
                    , Cmd.none
                    )

                feat :: _ ->
                    case D.decodeValue featureDecoder feat of
                        Ok entry ->
                            ( { model
                                | hoveredFeature = Just feat
                                , hoveredEntry = Just entry
                                , hoverPoint = Just point
                              }
                            , Cmd.none
                            )

                        Err _ ->
                            ( { model
                                | hoveredFeature = Just feat
                                , hoveredEntry = Nothing
                                , hoverPoint = Just point
                              }
                            , Cmd.none
                            )

        Click { point, lngLat, renderedFeatures } ->
            case renderedFeatures of
                [] ->
                    ( { model
                        | clickedFeature = Nothing
                        , clickedEntry = Nothing
                        , validPhone = Nothing
                      }
                    , Cmd.none
                    )

                feat :: _ ->
                    case D.decodeValue featureDecoder feat of
                        Ok entry ->
                            case entry.geometry.coordinates of
                                lng :: lat :: _ ->
                                    flyIn model
                                        feat
                                        entry
                                        (LngLat.LngLat lng lat)

                                _ ->
                                    flyIn model feat entry lngLat

                        Err _ ->
                            ( { model
                                | clickedFeature = Just feat
                                , clickedEntry = Nothing
                                , validPhone = Nothing
                              }
                            , Cmd.none
                            )

        Touch { touches, center } ->
            let
                lngLat =
                    center.lngLat

                renderedFeatures =
                    center.renderedFeatures
            in
            case renderedFeatures of
                [] ->
                    ( { model
                        | clickedFeature = Nothing
                        , clickedEntry = Nothing
                        , validPhone = Nothing
                      }
                    , Cmd.none
                    )

                feat :: _ ->
                    case D.decodeValue featureDecoder feat of
                        Ok entry ->
                            case entry.geometry.coordinates of
                                lng :: lat :: _ ->
                                    flyIn model
                                        feat
                                        entry
                                        (LngLat.LngLat lng lat)

                                _ ->
                                    flyIn model feat entry lngLat

                        Err _ ->
                            ( { model
                                | clickedFeature = Just feat
                                , clickedEntry = Nothing
                                , validPhone = Nothing
                              }
                            , Cmd.none
                            )

        PhoneUpdate phone ->
            ( { model | phoneNumber = phone }, Cmd.none )

        AlertMe ->
            ( { model | validPhone = Just (validNumber model.phoneNumber) }
            , if validNumber model.phoneNumber then
                case model.clickedEntry of
                    Nothing ->
                        Cmd.none

                    Just entry ->
                        firebaseWrite
                            { phoneNumber = model.phoneNumber
                            , country = entry.properties.country
                            , province = entry.properties.province
                            , county = entry.properties.county
                            , id = entry.layer.id
                            }

              else
                Cmd.none
            )


flyIn : Model -> E.Value -> FeatureEntry -> LngLat -> ( Model, Cmd msg )
flyIn model feat entry lngLat =
    if entry.layer.id == "countries" then
        ( { model
            | clickedFeature = Just feat
            , clickedEntry = Just entry
            , validPhone = Nothing
          }
        , MapCommands.flyTo
            [ Opt.center lngLat
            , Opt.duration 2000
            , Opt.zoom 4
            , Opt.animate True
            ]
        )

    else if entry.layer.id == "states" then
        ( { model
            | clickedFeature = Just feat
            , clickedEntry = Just entry
            , validPhone = Nothing
          }
        , MapCommands.flyTo
            [ Opt.center lngLat
            , Opt.duration 2000
            , Opt.zoom 6
            , Opt.animate True
            ]
        )

    else
        ( { model
            | clickedFeature = Just feat
            , clickedEntry = Just entry
            , validPhone = Nothing
          }
        , MapCommands.flyTo
            [ Opt.center lngLat
            , Opt.duration 2000
            , Opt.zoom 10
            , Opt.animate True
            ]
        )


validNumber : String -> Bool
validNumber number =
    PhoneNumber.valid
        { defaultCountry = PhoneNumber.Countries.countryUS
        , otherCountries = []
        , types = [ PhoneNumber.SmsServices ]
        }
        number



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout []
        (column
            [ height fill
            , width fill
            , centerX
            ]
            [ Element.el
                [ Element.inFront
                    (hoverView model.hoverPoint model.hoveredEntry)
                , Element.inFront
                    (clickView model.phoneNumber model.validPhone model.clickedEntry)
                , height fill
                , width fill
                ]
                (Element.html (map model))
            ]
        )


map : Model -> Html Msg
map model =
    div
        [ Html.Attributes.style "height" "100vh" ]
        [ Mapbox.Element.map
            [ id "my-map"
            , onMouseMove Hover
            , onClick Click
            , onTouchEnd Touch
            , eventFeaturesLayers [ "counties", "states", "countries" ]
            , hoverFeature model.hoveredFeature
            ]
            (Dark.styleWithAttr
                [ Source.geoJSONFromValue "counties"
                    []
                    (encodeRecords model.counties)
                , Source.geoJSONFromValue "states"
                    []
                    (encodeRecords model.states)
                , Source.geoJSONFromValue "countries"
                    []
                    (encodeRecords model.countries)
                ]
                [ Layer.circle "counties"
                    "counties"
                    [ Layer.circleRadius
                        (countySize
                            (minConfirmed model.counties)
                            (maxConfirmed model.counties)
                        )
                    , Layer.circleColor featureColor
                    , Layer.circleOpacity countyOpacity
                    , Layer.minzoom 5
                    ]
                , Layer.circle "states"
                    "states"
                    [ Layer.circleRadius
                        (stateSize
                            (minConfirmed model.counties)
                            (maxConfirmed model.countries)
                        )
                    , Layer.circleColor featureColor
                    , Layer.circleOpacity stateOpacity
                    , Layer.maxzoom 5.25
                    ]
                , Layer.circle "countries"
                    "countries"
                    [ Layer.circleRadius
                        (countrySize
                            (minConfirmed model.countries)
                            (maxConfirmed model.countries)
                        )
                    , Layer.circleColor featureColor
                    , Layer.circleOpacity countryOpacity
                    ]
                ]
            )
        ]


hoverView : Maybe ( Int, Int ) -> Maybe FeatureEntry -> Element msg
hoverView hoverPoint hoveredEntry =
    case ( hoverPoint, hoveredEntry ) of
        ( Just ( x, y ), Just { geometry, properties, layer } ) ->
            let
                ( title, subtitle ) =
                    titleLabel layer properties
            in
            Element.el
                [ moveRight (toFloat x)
                , moveDown (toFloat y)
                , width (px 0)
                , height (px 0)
                ]
                (Element.el
                    [ centerX
                    , alignTop
                    , moveDown 15.0
                    , padding 10
                    , Background.color (Element.rgb 0 0 0)
                    , Border.rounded 10
                    , width shrink
                    , height shrink
                    , Border.shadow
                        { offset = ( 0, 1 )
                        , size = 4
                        , blur = 20
                        , color = Element.rgb 0.1 0.1 0.1
                        }
                    ]
                    (Element.column [ spacing 2 ]
                        [ Element.el
                            [ Font.color (Element.rgb 1 1 1)
                            , Font.size 16
                            , Font.bold
                            , Font.center
                            , centerX
                            , width shrink
                            , height shrink
                            ]
                            (Element.text title)
                        , Element.el
                            [ Font.color (Element.rgb 1 1 1)
                            , Font.size 13
                            , Font.light
                            , Font.center
                            , centerX
                            , width shrink
                            , height shrink
                            ]
                            (Element.text subtitle)
                        , Element.el
                            [ Font.color
                                (if properties.confirmed <= 1000 then
                                    green

                                 else if properties.confirmed <= 10000 then
                                    yellow

                                 else
                                    red
                                )
                            , Font.size 11
                            , Font.regular
                            , Font.center
                            , centerX
                            , width shrink
                            , height shrink
                            ]
                            (if properties.confirmed == 1 then
                                Element.text (String.fromInt properties.confirmed ++ " case")

                             else
                                Element.text (String.fromInt properties.confirmed ++ " cases")
                            )
                        ]
                    )
                )

        ( _, _ ) ->
            Element.none


titleLabel : LayerEntry -> PropertiesEntry -> ( String, String )
titleLabel layer properties =
    case
        ( layer.id
        , ( properties.county, properties.province, properties.country )
        )
    of
        ( "counties", ( Just county, province, _ ) ) ->
            ( county ++ " County", province )

        ( "states", ( _, province, country ) ) ->
            ( province, country )

        ( "countries", ( _, "null", country ) ) ->
            ( country, "" )

        ( "countries", ( _, province, country ) ) ->
            ( province, country )

        _ ->
            ( "", "" )


clickView : String -> Maybe Bool -> Maybe FeatureEntry -> Element Msg
clickView phoneNum validPhone entry =
    case entry of
        Nothing ->
            Element.none

        Just { geometry, properties, layer } ->
            let
                ( title, subtitle ) =
                    titleLabel layer properties
            in
            Element.el
                [ centerX
                , alignBottom
                , moveUp 50
                , width shrink
                , height shrink
                , Background.color (Element.rgb 0 0 0)
                , Border.shadow
                    { offset = ( 0, 1 )
                    , size = 4
                    , blur = 20
                    , color = Element.rgb 0.1 0.1 0.1
                    }
                , Border.rounded 35
                , padding 25
                ]
                (Element.column
                    [ spacing 5
                    , centerX
                    ]
                    ([ Element.el
                        [ Font.color (Element.rgb 1 1 1)
                        , Font.size 36
                        , Font.bold
                        , Font.center
                        , centerX
                        , width shrink
                        , height shrink
                        ]
                        (Element.text title)
                     , Element.el
                        [ Font.color (Element.rgb 1 1 1)
                        , Font.size 24
                        , Font.light
                        , Font.center
                        , centerX
                        , width shrink
                        , height shrink
                        ]
                        (Element.text subtitle)
                     , Element.column
                        [ centerX
                        , padding 5
                        , spacing 3
                        , width shrink
                        , height shrink
                        ]
                        [ Element.el
                            [ Font.color
                                (if properties.confirmed <= 1000 then
                                    green

                                 else if properties.confirmed <= 10000 then
                                    yellow

                                 else
                                    red
                                )
                            , Font.size 32
                            , Font.regular
                            , Font.center
                            , centerX
                            , width shrink
                            , height shrink
                            ]
                            (if properties.confirmed == 1 then
                                Element.text (String.fromInt properties.confirmed ++ " case")

                             else
                                Element.text (String.fromInt properties.confirmed ++ " cases")
                            )
                        , Element.el
                            [ Font.color red
                            , Font.size 28
                            , Font.regular
                            , Font.center
                            , centerX
                            , width shrink
                            , height shrink
                            ]
                            (if properties.deaths == 1 then
                                Element.text (String.fromInt properties.deaths ++ " death")

                             else
                                Element.text (String.fromInt properties.deaths ++ " deaths")
                            )
                        ]
                     ]
                        ++ textView phoneNum validPhone properties title
                    )
                )


textView : String -> Maybe Bool -> PropertiesEntry -> String -> List (Element Msg)
textView phoneNum validPhone properties title =
    case validPhone of
        Just True ->
            let
                shareUrl =
                    "https://twitter.com/intent/tweet?url=http%3A%2F%2Fwww.coronalert.live&text="
                        ++ String.fromInt properties.confirmed
                        ++ "%20cases%2C%20"
                        ++ String.fromInt properties.deaths
                        ++ "%20deaths%20in%20"
                        ++ String.replace " " "%20" title
                        ++ ".%20Stay%20safe%2C%20stay%20alert%21&hashtags=COVID%2C%20coronavirus"
            in
            [ Element.el
                [ paddingEach { top = 15, bottom = 5, left = 5, right = 5 }
                , centerX
                , centerY
                ]
                (Element.el
                    [ Font.center
                    , Font.color (Element.rgb 1 1 1)
                    , Font.size 14
                    , centerX
                    , centerY
                    ]
                    (Element.text "Check your phone for a confirmation text")
                )
            , Element.el
                [ padding 5
                , centerX
                , centerY
                , Background.color (Element.rgb255 28 28 30)
                , width (px 325)
                , height (px 50)
                , Border.rounded 15
                ]
                (Element.el
                    [ Font.center
                    , Font.bold
                    , Font.color green
                    , centerX
                    , centerY
                    ]
                    (Element.text "Subscribed ✓")
                )
            , Element.el
                [ Font.size 14
                , centerX
                ]
                Element.none
            , Element.el
                [ padding 5
                , centerX
                ]
                (Element.newTabLink
                    [ Background.color blue
                    , width (px 325)
                    , height (px 50)
                    , Border.rounded 15
                    , padding 10
                    , Font.center
                    , Border.shadow
                        { offset = ( 0, 1 )
                        , size = 4
                        , blur = 20
                        , color = Element.rgb 0.1 0.1 0.1
                        }
                    ]
                    { url = shareUrl
                    , label =
                        Element.el
                            [ Font.size 18
                            , Font.color
                                (Element.rgb 1 1 1)
                            , centerX
                            , centerY
                            , width shrink
                            , height shrink
                            , Font.extraLight
                            ]
                            (Element.text "Share to Twitter")
                    }
                )
            ]

        _ ->
            [ Element.el
                [ padding 5
                , centerX
                ]
                (let
                    formatted =
                        if phoneNum == "" then
                            ""

                        else if String.length phoneNum < 3 then
                            "(" ++ phoneNum

                        else if String.length phoneNum < 6 then
                            "(" ++ String.slice 0 3 phoneNum ++ ") " ++ String.slice 3 6 phoneNum

                        else if String.length phoneNum <= 10 then
                            "(" ++ String.slice 0 3 phoneNum ++ ") " ++ String.slice 3 6 phoneNum ++ " - " ++ String.slice 6 10 phoneNum

                        else
                            phoneNum
                 in
                 Input.text
                    [ centerX
                    , Background.color (Element.rgb 1 1 1)
                    , width (px 325)
                    , height (px 50)
                    , Font.alignLeft
                    , Border.rounded 15
                    , onEnter AlertMe
                    ]
                    { label =
                        Input.labelAbove
                            [ Font.size 14
                            , Font.color (Element.rgb 1 1 1)
                            , centerX
                            , padding 5
                            , Font.center
                            ]
                            (Element.text "Enter your phone number for text updates")
                    , onChange =
                        \new ->
                            if
                                (String.right 1 new == ")")
                                    || (String.right 1 new == "-")
                                    || (String.length (String.filter Char.isDigit new) > 10)
                            then
                                PhoneUpdate (String.dropRight 1 (String.filter Char.isDigit new))

                            else
                                PhoneUpdate (String.filter Char.isDigit new)
                    , placeholder =
                        Just (Input.placeholder [] (Element.text "(___) ___ - ____"))
                    , text = formatted
                    }
                )
            , Element.el
                [ Font.size 14
                , Font.color red
                , centerX
                ]
                (case validPhone of
                    Just False ->
                        Element.text "Oops, that's not a phone number. Try again!"

                    _ ->
                        Element.none
                )
            , Element.el
                [ padding 5
                , centerX
                ]
                (Input.button
                    [ Background.color blue
                    , width (px 325)
                    , height (px 50)
                    , Border.rounded 15
                    , padding 10
                    , Font.center
                    , Border.shadow
                        { offset = ( 0, 1 )
                        , size = 4
                        , blur = 20
                        , color = Element.rgb 0.1 0.1 0.1
                        }
                    ]
                    { onPress = Just AlertMe
                    , label =
                        Element.el
                            [ Font.size 18
                            , Font.color
                                (Element.rgb 1 1 1)
                            , centerX
                            , centerY
                            , width shrink
                            , height shrink
                            , Font.extraLight
                            ]
                            (Element.text ("Alert me on " ++ title))
                    }
                )
            ]



-- VIEW HELPERS


isCountry : RecordEntry -> Bool
isCountry record =
    not (record.country == "US")


isState : RecordEntry -> Bool
isState record =
    record.country == "US"


maxConfirmed : List RecordEntry -> Int
maxConfirmed records =
    Maybe.withDefault 0
        (List.maximum <| List.map (\r -> r.stats.confirmed) records)


minConfirmed : List RecordEntry -> Int
minConfirmed records =
    Maybe.withDefault 0
        (List.minimum <| List.map (\r -> r.stats.confirmed) records)


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


countryOpacity : Expr.Expression Expr.DataExpression Float
countryOpacity =
    Expr.ifElse (Expr.toBool (Expr.featureState (Expr.str "hover")))
        (Expr.float 0.9)
        (Expr.float 0.5)


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


hoverFeature : Maybe E.Value -> MapboxAttr msg
hoverFeature feature =
    case feature of
        Nothing ->
            featureState []

        Just feat ->
            featureState [ ( feat, [ ( "hover", E.bool True ) ] ) ]


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



-- HTTP


worldUrl : String
worldUrl =
    "https://disease.sh/v2/jhucsse"


countyUrl : String
countyUrl =
    "https://disease.sh/v2/jhucsse/counties"


getLatestCountyData : Cmd Msg
getLatestCountyData =
    Http.get
        { url = countyUrl
        , expect = Http.expectJson DataReceived listOfRecordsDecoder
        }


getLatestWorldData : Cmd Msg
getLatestWorldData =
    Http.get
        { url = worldUrl
        , expect = Http.expectJson DataReceived listOfRecordsDecoder
        }


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message



-- ENCODE/DECODE


encodeRecords : List RecordEntry -> E.Value
encodeRecords records =
    E.object
        [ ( "type", E.string "FeatureCollection" )
        , ( "features"
          , E.list E.object
                (List.map2 encodeRecord
                    (List.range 1 (List.length records + 1))
                    records
                )
          )
        ]


encodeRecord : Int -> RecordEntry -> List ( String, E.Value )
encodeRecord id record =
    [ ( "type", E.string "Feature" )
    , ( "id", E.int id )
    , ( "properties"
      , E.object
            [ ( "country", E.string record.country )
            , ( "province"
              , Extra.unwrap E.null E.string record.province
              )
            , ( "county"
              , Extra.unwrap E.null E.string record.county
              )
            , ( "updatedAt", E.string record.updatedAt )
            , ( "confirmed", E.int record.stats.confirmed )
            , ( "deaths", E.int record.stats.deaths )
            , ( "recovered", E.int record.stats.recovered )
            ]
      )
    , ( "geometry"
      , E.object
            [ ( "type", E.string "Point" )
            , ( "coordinates"
              , E.list E.float
                    [ Maybe.withDefault 0
                        (String.toFloat record.coordinates.longitude)
                    , Maybe.withDefault 0
                        (String.toFloat record.coordinates.latitude)
                    ]
              )
            ]
      )
    ]


listOfRecordsDecoder : D.Decoder (List RecordEntry)
listOfRecordsDecoder =
    D.list recordDecoder


recordDecoder : D.Decoder RecordEntry
recordDecoder =
    D.map6
        RecordEntry
        (D.field "country" D.string)
        (D.field "province" (D.nullable D.string))
        (D.field "county" (D.nullable D.string))
        (D.field "updatedAt" D.string)
        (D.field "stats" statsDecoder)
        (D.field "coordinates" coordinatesDecoder)


coordinatesDecoder : D.Decoder CoordinatesEntry
coordinatesDecoder =
    D.map2
        CoordinatesEntry
        (D.field "latitude" D.string)
        (D.field "longitude" D.string)


statsDecoder : D.Decoder StatsEntry
statsDecoder =
    D.map3
        StatsEntry
        (D.field "confirmed" D.int)
        (D.field "deaths" D.int)
        (D.field "recovered" D.int)


featureDecoder : D.Decoder FeatureEntry
featureDecoder =
    D.map3
        FeatureEntry
        (D.field "geometry" geometryDecoder)
        (D.field "properties" propertiesDecoder)
        (D.field "layer" layerDecoder)


geometryDecoder : D.Decoder GeometryEntry
geometryDecoder =
    D.map
        GeometryEntry
        (D.field "coordinates" (D.list D.float))


propertiesDecoder : D.Decoder PropertiesEntry
propertiesDecoder =
    D.map7
        PropertiesEntry
        (D.field "country" D.string)
        (D.field "province" D.string)
        (D.field "county" (D.nullable D.string))
        (D.field "updatedAt" D.string)
        (D.field "confirmed" D.int)
        (D.field "deaths" D.int)
        (D.field "recovered" D.int)


layerDecoder : D.Decoder LayerEntry
layerDecoder =
    D.map
        LayerEntry
        (D.field "id" D.string)



-- COLORS


red : Element.Color
red =
    Element.rgb255 254 127 156


yellow : Element.Color
yellow =
    Element.rgb255 250 218 94


blue : Element.Color
blue =
    Element.rgb255 10 132 255


green : Element.Color
green =
    Element.rgb255 126 249 255


redExpr : Expr.Expression Expr.DataExpression Expr.Color
redExpr =
    Expr.rgba 254 127 156 1


yellowExpr : Expr.Expression Expr.DataExpression Expr.Color
yellowExpr =
    Expr.rgba 250 218 94 1


greenExpr : Expr.Expression Expr.DataExpression Expr.Color
greenExpr =
    Expr.rgba 126 249 255 1



-- Ports


port firebaseWrite : WriteEntry -> Cmd msg
