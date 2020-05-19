module UpdateData exposing (..)

import Browser
import Html.Events exposing (onClick)

import Html exposing (..)
import Http exposing (..)
import Json.Decode as D

type alias Model =
    { records : List RecordEntry
    , errorMessage : Maybe String
    }

type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List RecordEntry))

url : String
url =
    "https://disease.sh/v2/jhucsse/counties"

getLatestData : Cmd Msg
getLatestData = 
    Http.get
        { url = url
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

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ]
            [ text "Get data from server" ]
        , viewRecordsOrError model
        ]


viewRecordsOrError : Model -> Html Msg
viewRecordsOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            viewRecords model.records


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch nicknames at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewRecords : List RecordEntry -> Html Msg
viewRecords records =
    div []
        [ h3 [] [ text "Data:" ]
        , ul [] (List.map viewRecord records)
        ]


viewRecord : RecordEntry -> Html Msg
viewRecord record =
    li [] [ text record.county ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, getLatestData )

        DataReceived (Ok records) ->
            ( { model | records = records }, Cmd.none )

        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )

init : () -> ( Model, Cmd Msg )
init _ =
    ( { records = []
      , errorMessage = Nothing
      }
    , Cmd.none
    )   

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

-- Decoder --

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
    , province : String
    , county : String
    , updatedAt : String
    , stats : StatsEntry
    , coordinates : CoordinatesEntry
    }

listOfRecordsDecoder : D.Decoder (List RecordEntry)
listOfRecordsDecoder =
    D.list recordDecoder

recordDecoder : D.Decoder RecordEntry
recordDecoder =
    D.map6
        RecordEntry
        (D.field "country" D.string)
        (D.field "province" D.string)
        (D.field "county" D.string)
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


