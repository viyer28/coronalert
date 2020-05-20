module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes
import Http exposing (..)
import Json.Decode as D
import Json.Encode as E
import List exposing (map)
import LngLat exposing (LngLat)
import Mapbox.Element exposing (..)
import Mapbox.Expression as Expr
import Mapbox.Layer as Layer
import Mapbox.Source as Source
import Maybe.Extra as Extra
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
    { records : List RecordEntry
    , errorMessage : Maybe String
    }


type Msg
    = DataReceived (Result Http.Error (List RecordEntry))


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



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { records = []
      , errorMessage = Nothing
      }
    , Cmd.batch [ getLatestCountyData, getLatestWorldData ]
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataReceived (Ok records) ->
            ( { model
                | records =
                    if List.any notUS records then
                        model.records ++ List.map removeCounty records

                    else
                        model.records ++ records
              }
            , Cmd.none
            )

        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ Html.Attributes.style "height" "100vh" ]
        [ Mapbox.Element.map
            []
            (Dark.styleWithAttr
                (Source.geoJSONFromValue "points"
                    []
                    (encodeRecords (List.filter notUSState model.records))
                )
                (Layer.circle "points"
                    "points"
                    [ Layer.circleRadius
                        (sizeByConfirmed
                            (minConfirmed model.records)
                            (maxConfirmed model.records)
                        )
                    , Layer.circleColor (Expr.rgba 126 249 255 1)
                    , Layer.circleOpacity
                        (opacityByConfirmed
                            (minConfirmed model.records)
                            (maxConfirmed model.records)
                        )
                    ]
                )
            )
        ]



-- VIEW ELPERS


notUS : RecordEntry -> Bool
notUS record =
    not (record.country == "US")


removeCounty : RecordEntry -> RecordEntry
removeCounty record =
    { record | county = Nothing }


notUSState : RecordEntry -> Bool
notUSState record =
    not (record.country == "US" && record.county == Nothing)


maxConfirmed : List RecordEntry -> Int
maxConfirmed records =
    Maybe.withDefault 0
        (List.maximum <| List.map (\r -> r.stats.confirmed) records)


minConfirmed : List RecordEntry -> Int
minConfirmed records =
    Maybe.withDefault 0
        (List.minimum <| List.map (\r -> r.stats.confirmed) records)


sizeByConfirmed : Int -> Int -> Expr.Expression Expr.CameraExpression Float
sizeByConfirmed min max =
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


opacityByConfirmed : Int -> Int -> Expr.Expression Expr.CameraExpression Float
opacityByConfirmed min max =
    let
        confirmedOpacity =
            Expr.getProperty (Expr.str "confirmed")
                |> Expr.toFloat 0
                |> Expr.interpolate (Expr.Exponential 0.9999)
                    [ ( toFloat min, Expr.float 0.4 )
                    , ( toFloat max, Expr.float 0.75 )
                    ]
    in
    Expr.zoom
        |> Expr.interpolate (Expr.Exponential 1.3)
            [ ( 0
              , Expr.multiply
                    (Expr.float 0.9)
                    confirmedOpacity
              )
            , ( 15
              , Expr.multiply
                    (Expr.float 2.5)
                    confirmedOpacity
              )
            ]



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
          , E.list E.object (List.map encodeRecord records)
          )
        ]


encodeRecord : RecordEntry -> List ( String, E.Value )
encodeRecord record =
    [ ( "type", E.string "Feature" )
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
