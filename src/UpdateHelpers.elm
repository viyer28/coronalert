-- module UpdateHelpers: helper functions for updating the view


module UpdateHelpers exposing (..)

import Element exposing (DeviceClass(..))
import LngLat exposing (LngLat)
import MapCommands
import Mapbox.Cmd.Option as Opt
import PhoneNumber
import PhoneNumber.Countries
import Types exposing (..)



-- controls map movement on searches and taps/clicks


flyIn : Model -> FeatureEntry -> LngLat -> ( Model, Cmd msg )
flyIn model entry coords =
    if entry.layer.id == "countries" then
        let
            zoom =
                if model.device.class == Phone then
                    3.5

                else
                    4

            lngLat =
                if model.device.class == Phone then
                    { lng = coords.lng
                    , lat = coords.lat - 3
                    }

                else
                    coords
        in
        ( { model
            | clickedEntry = Just entry
            , validPhone = Nothing
          }
        , MapCommands.flyTo
            [ Opt.center lngLat
            , Opt.duration 2000
            , Opt.zoom zoom
            , Opt.animate True
            ]
        )

    else if entry.layer.id == "states" then
        let
            zoom =
                if model.device.class == Phone then
                    5.25

                else
                    6

            lngLat =
                if model.device.class == Phone then
                    { lng = coords.lng
                    , lat = coords.lat - 1
                    }

                else
                    coords
        in
        ( { model
            | clickedEntry = Just entry
            , validPhone = Nothing
          }
        , MapCommands.flyTo
            [ Opt.center lngLat
            , Opt.duration 2000
            , Opt.zoom zoom
            , Opt.animate True
            ]
        )

    else
        let
            zoom =
                if model.device.class == Phone then
                    8

                else
                    10

            lngLat =
                if model.device.class == Phone then
                    { lng = coords.lng
                    , lat = coords.lat - 0.15
                    }

                else
                    coords
        in
        ( { model
            | clickedEntry = Just entry
            , validPhone = Nothing
          }
        , MapCommands.flyTo
            [ Opt.center lngLat
            , Opt.duration 2000
            , Opt.zoom zoom
            , Opt.animate True
            ]
        )



-- determines if a phone number is a textable number in the US


validNumber : String -> Bool
validNumber number =
    PhoneNumber.valid
        { defaultCountry = PhoneNumber.Countries.countryUS
        , otherCountries = []
        , types = [ PhoneNumber.SmsServices ]
        }
        number



-- converts a record into a searchable record for search


recordToId : String -> RecordEntry -> IdRecordEntry
recordToId region record =
    case region of
        "counties" ->
            case record.province of
                Nothing ->
                    { id = ""
                    , regionType = region
                    , data = record
                    }

                Just state ->
                    case record.county of
                        Nothing ->
                            { id = ""
                            , regionType = region
                            , data = record
                            }

                        Just county ->
                            { id = county ++ " County, " ++ state
                            , regionType = region
                            , data = record
                            }

        "states" ->
            case record.province of
                Nothing ->
                    { id = ""
                    , regionType = region
                    , data = record
                    }

                Just state ->
                    { id = state ++ ", " ++ record.country
                    , regionType = region
                    , data = record
                    }

        "countries" ->
            case record.province of
                Nothing ->
                    { id = record.country
                    , regionType = region
                    , data = record
                    }

                Just province ->
                    { id = province ++ ", " ++ record.country
                    , regionType = region
                    , data = record
                    }

        _ ->
            { id = ""
            , regionType = region
            , data = record
            }



-- searches a list of searchable records


searchRecords : String -> List IdRecordEntry -> List IdRecordEntry
searchRecords query records =
    let
        lowerQuery =
            String.toLower query
    in
    if query == "" then
        []

    else
        List.sortBy (\x -> String.length x.id)
            (List.filter
                (\x -> String.contains lowerQuery (String.toLower x.id))
                records
            )



-- converts a searchable record into an Elm feature object for controlling map movement following a search


idToFeature : IdRecordEntry -> FeatureEntry
idToFeature id =
    FeatureEntry
        (GeometryEntry
            [ Maybe.withDefault
                0
                (String.toFloat id.data.coordinates.longitude)
            , Maybe.withDefault
                0
                (String.toFloat id.data.coordinates.latitude)
            ]
        )
        { country = id.data.country
        , province = Maybe.withDefault "null" id.data.province
        , county = id.data.county
        , updatedAt = id.data.updatedAt
        , confirmed = id.data.stats.confirmed
        , deaths = id.data.stats.deaths
        , recovered = id.data.stats.recovered
        }
        (LayerEntry
            id.regionType
        )



-- determines if a record is a country (for the world HTTP call)


isCountry : RecordEntry -> Bool
isCountry record =
    not (record.country == "US")



-- determines if a record is a state (for the world HTTP call)


isState : RecordEntry -> Bool
isState record =
    record.country == "US"
