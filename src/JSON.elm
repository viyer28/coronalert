-- module JSON: operations for parsing JSON into Elm objects and vice versa


module JSON exposing (..)

import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Extra
import Types exposing (..)



-- turns a list of searchable records to normal records to encode in a GeoJSON


idsToRecords : List IdRecordEntry -> List RecordEntry
idsToRecords records =
    List.map (\r -> r.data) records



-- encode a list of records into a GeoJSON FeatureCollection for displaying on the map


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



-- encode a single record into a GeoJSON Feature for displaying on the map


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



-- decodes Json into a list of records


listOfRecordsDecoder : D.Decoder (List RecordEntry)
listOfRecordsDecoder =
    D.list recordDecoder



-- decodes Json into a record


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



-- decodes Json into coordinates


coordinatesDecoder : D.Decoder CoordinatesEntry
coordinatesDecoder =
    D.map2
        CoordinatesEntry
        (D.field "latitude" D.string)
        (D.field "longitude" D.string)



-- decodes Json into COVID stats


statsDecoder : D.Decoder StatsEntry
statsDecoder =
    D.map3
        StatsEntry
        (D.field "confirmed" D.int)
        (D.field "deaths" D.int)
        (D.field "recovered" D.int)



-- decodes GeoJSON feature into a feature object


featureDecoder : D.Decoder FeatureEntry
featureDecoder =
    D.map3
        FeatureEntry
        (D.field "geometry" geometryDecoder)
        (D.field "properties" propertiesDecoder)
        (D.field "layer" layerDecoder)



-- decodes GeoJSON feature geometry into coordinates


geometryDecoder : D.Decoder GeometryEntry
geometryDecoder =
    D.map
        GeometryEntry
        (D.field "coordinates" (D.list D.float))



-- decodes GeoJSON feature properties into a properties object


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



-- decodes GeoJSON feature layer into an id determining if it is a county, state, or country


layerDecoder : D.Decoder LayerEntry
layerDecoder =
    D.map
        LayerEntry
        (D.field "id" D.string)
