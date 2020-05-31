-- module Networking: defines HTTP calls to get COVID data


module Networking exposing (..)

import Http exposing (..)
import JSON exposing (listOfRecordsDecoder)
import Types exposing (Msg(..))



-- URL for getting world COVID data from JHU


worldUrl : String
worldUrl =
    "https://disease.sh/v2/jhucsse"



-- URL for getting county COVID data from JHU


countyUrl : String
countyUrl =
    "https://disease.sh/v2/jhucsse/counties"



-- HTTP call for getting county COVID data from JHU


getLatestCountyData : Cmd Msg
getLatestCountyData =
    Http.get
        { url = countyUrl
        , expect = Http.expectJson DataReceived listOfRecordsDecoder
        }



-- HTTP call for getting world COVID data from JHU


getLatestWorldData : Cmd Msg
getLatestWorldData =
    Http.get
        { url = worldUrl
        , expect = Http.expectJson DataReceived listOfRecordsDecoder
        }



-- HTTP error message handling


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
