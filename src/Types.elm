-- module Types: custom type aliases and types


module Types exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Element exposing (Device)
import Http exposing (Error)
import Json.Encode as E
import Mapbox.Element exposing (EventData, TouchEvent)
import Url exposing (Url)



-- the main Model


type alias Model =
    { countries : List IdRecordEntry
    , states : List IdRecordEntry
    , counties : List IdRecordEntry
    , errorMessage : Maybe String
    , hoveredFeature : Maybe E.Value
    , hoveredEntry : Maybe FeatureEntry
    , hoverPoint : Maybe ( Int, Int )
    , clickedEntry : Maybe FeatureEntry
    , phoneNumber : String
    , validPhone : Maybe Bool
    , invalidSub : Maybe Bool
    , search : String
    , searchResults : List IdRecordEntry
    , displayPremium : Bool
    , premiumSuccess : Bool
    , key : Nav.Key
    , url : Url.Url
    , device : Device
    , width : Int
    , height : Int
    }



-- the Msg type used for updating the view


type Msg
    = DataReceived (Result Http.Error (List RecordEntry))
    | Hover EventData
    | Click EventData
    | Touch TouchEvent
    | PhoneUpdate String
    | AlertMe
    | Search String
    | ClickSearch IdRecordEntry
    | ClickPremium
    | Upgrade
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ExitPremium
    | InvalidSubscribe Bool
    | SetScreenSize Int Int
    | Noop



-- stores coordinates within a RecordEntry


type alias CoordinatesEntry =
    { latitude : String
    , longitude : String
    }



-- stores stats within a RecordEntry


type alias StatsEntry =
    { confirmed : Int
    , deaths : Int
    , recovered : Int
    }



-- a main object for storing COVID data


type alias RecordEntry =
    { country : String
    , province : Maybe String
    , county : Maybe String
    , updatedAt : String
    , stats : StatsEntry
    , coordinates : CoordinatesEntry
    }



-- extracts coordinate data from a GeoJSON feature


type alias GeometryEntry =
    { coordinates : List Float }



-- extracts property data from a GeoJSON feature


type alias PropertiesEntry =
    { country : String
    , province : String
    , county : Maybe String
    , updatedAt : String
    , confirmed : Int
    , deaths : Int
    , recovered : Int
    }



-- determines whether a GeoJSON feature is a county, country, or state


type alias LayerEntry =
    { id : String }



-- extracted GeoJSON feature data


type alias FeatureEntry =
    { geometry : GeometryEntry
    , properties : PropertiesEntry
    , layer : LayerEntry
    }



-- stores subscription data to be written to Firebase


type alias WriteEntry =
    { phoneNumber : String
    , country : String
    , province : String
    , county : Maybe String
    , id : String
    }



-- wrapper for RecordEntries for performing searches


type alias IdRecordEntry =
    { id : String
    , regionType : String
    , data : RecordEntry
    }



-- stores premium data to handle Stripe events


type alias PremiumEntry =
    { phoneNumber : String
    , url : String
    }



-- Flags for setting the initial viewport


type alias Flags =
    { height : Int
    , width : Int
    }
