port module Main exposing (main)

import Browser
import Browser.Events as Ev
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
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
import Mapbox.Style as Style
import Maybe.Extra as Extra
import PhoneNumber
import PhoneNumber.Countries
import Styles.Dark as Dark
import Url


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


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


type alias IdRecordEntry =
    { id : String
    , regionType : String
    , data : RecordEntry
    }


type alias PremiumEntry =
    { phoneNumber : String
    , url : String
    }


type alias Flags =
    { height : Int
    , width : Int
    }



-- INIT


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        displayPremium =
            String.contains "premium" url.path

        ( success, phoneNumber ) =
            if String.startsWith "/premium/success/" url.path then
                ( True, String.dropLeft 17 url.path )

            else if String.startsWith "/premium/failure/" url.path then
                ( False, String.dropLeft 17 url.path )

            else
                ( False, "" )
    in
    ( { countries = []
      , states = []
      , counties = []
      , errorMessage = Nothing
      , hoveredFeature = Nothing
      , hoveredEntry = Nothing
      , hoverPoint = Nothing
      , clickedEntry = Nothing
      , phoneNumber = phoneNumber
      , validPhone = Nothing
      , invalidSub = Nothing
      , search = ""
      , searchResults = []
      , displayPremium = displayPremium
      , premiumSuccess = success
      , key = key
      , url = url
      , device =
            Element.classifyDevice
                { height = flags.height
                , width = flags.width
                }
      , height = flags.height
      , width = flags.width
      }
    , Cmd.batch
        ([ getLatestCountyData
         , getLatestWorldData
         ]
            ++ (if success then
                    [ firebaseUpgrade phoneNumber ]

                else
                    []
               )
        )
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ invalidSubscription InvalidSubscribe
        , Ev.onResize (\values -> SetScreenSize values)
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataReceived (Ok records) ->
            if List.any isCountry records then
                ( { model
                    | countries =
                        List.map
                            (recordToId "countries")
                            (List.filter isCountry records)
                    , states =
                        List.map
                            (recordToId "states")
                            (List.filter isState records)
                  }
                , Cmd.none
                )

            else
                ( { model
                    | counties =
                        List.map
                            (recordToId "counties")
                            records
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
                        | clickedEntry = Nothing
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
                                        entry
                                        (LngLat.LngLat lng lat)

                                _ ->
                                    flyIn model entry lngLat

                        Err _ ->
                            ( { model
                                | clickedEntry = Nothing
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
                        | clickedEntry = Nothing
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
                                        entry
                                        (LngLat.LngLat lng lat)

                                _ ->
                                    flyIn model entry lngLat

                        Err _ ->
                            ( { model
                                | clickedEntry = Nothing
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

        Search term ->
            ( { model
                | search = term
                , searchResults =
                    searchRecords term
                        (model.counties ++ model.states ++ model.countries)
              }
            , Cmd.none
            )

        ClickSearch record ->
            let
                entry =
                    idToFeature record

                lat =
                    Maybe.withDefault 0
                        (String.toFloat record.data.coordinates.latitude)

                lng =
                    Maybe.withDefault 0
                        (String.toFloat record.data.coordinates.longitude)

                ( newModel, newCmd ) =
                    flyIn model entry (LngLat.LngLat lng lat)
            in
            ( { newModel | search = "", searchResults = [] }, newCmd )

        ClickPremium ->
            ( { model | displayPremium = True }, Cmd.none )

        Upgrade ->
            ( { model | validPhone = Just (validNumber model.phoneNumber) }
            , if validNumber model.phoneNumber then
                let
                    url =
                        Url.toString model.url
                in
                if String.contains "premium" url then
                    let
                        index =
                            String.length url
                                - Maybe.withDefault 0
                                    (List.head (String.indexes "premium" url))
                    in
                    processPremium
                        { phoneNumber = model.phoneNumber
                        , url =
                            String.dropRight index url
                        }

                else
                    processPremium
                        { phoneNumber = model.phoneNumber
                        , url = url
                        }

              else
                Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        ExitPremium ->
            ( { model | displayPremium = False }, Cmd.none )

        InvalidSubscribe isInvalid ->
            ( { model
                | invalidSub = Just isInvalid
                , displayPremium = isInvalid
              }
            , Cmd.none
            )

        SetScreenSize x y ->
            let
                device =
                    Element.classifyDevice
                        { width = x
                        , height = y
                        }
            in
            ( { model | device = device, width = x, height = y }, Cmd.none )

        Noop ->
            ( model, Cmd.none )


flyIn : Model -> FeatureEntry -> LngLat -> ( Model, Cmd msg )
flyIn model entry coords =
    if entry.layer.id == "countries" then
        let
            lngLat =
                if model.device.class == Phone then
                    { lng = coords.lng
                    , lat = coords.lat - 5
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
            , Opt.zoom 4
            , Opt.animate True
            ]
        )

    else if entry.layer.id == "states" then
        let
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
            , Opt.zoom 6
            , Opt.animate True
            ]
        )

    else
        let
            lngLat =
                if model.device.class == Phone then
                    { lng = coords.lng
                    , lat = coords.lat - 0.075
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


idsToRecords : List IdRecordEntry -> List RecordEntry
idsToRecords records =
    List.map (\r -> r.data) records


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



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        noHoverOption =
            if model.device.class == Phone || model.device.class == Tablet then
                [ noHover ]

            else
                []

        isMobile =
            model.device.class == Phone
    in
    { title = "Coronalert | The COVID Map with Text Alerts"
    , body =
        [ Element.layoutWith
            { options = noHoverOption }
            []
            (column
                [ height (px model.height)
                , width (px model.width)
                , centerX
                , Background.color (Element.rgb 0 0 0)
                ]
                [ Element.el
                    ([ height fill
                     , width fill
                     , Element.inFront
                        (hoverView model.hoverPoint model.hoveredEntry)
                     , Element.inFront
                        (clickView isMobile model.phoneNumber model.validPhone model.invalidSub model.clickedEntry)
                     , Element.inFront
                        (header isMobile model.search model.searchResults)
                     ]
                        ++ (if isMobile then
                                [ Element.inFront shareButton
                                , Element.inFront premiumButton
                                ]

                            else
                                [ Element.inFront actions ]
                           )
                        ++ [ Element.inFront
                                (premium
                                    model.displayPremium
                                    model.phoneNumber
                                    model.validPhone
                                    model.premiumSuccess
                                )
                           ]
                    )
                    (Element.html (map model))
                ]
            )
        ]
    }


header : Bool -> String -> List IdRecordEntry -> Element Msg
header mobile searchTerm searchResults =
    let
        searchDisplay =
            List.take 5 searchResults

        constraints =
            if mobile then
                [ centerX
                , alignTop
                , moveDown 10
                ]

            else
                [ alignLeft
                , moveRight 25
                , alignTop
                , moveDown 25
                ]

        search =
            if mobile then
                Element.el
                    [ width fill
                    , height (px 40)
                    , Border.rounded 20
                    , Background.color (Element.rgb 0 0 0)
                    , Border.color (Element.rgb255 58 58 60)
                    , Border.width 1
                    , paddingEach { top = 0, bottom = 0, left = 29, right = 29 }
                    , clip
                    , Element.inFront
                        (Element.image
                            [ centerY
                            , alignLeft
                            , moveRight 15
                            , height (px 15)
                            ]
                            { src = "https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcTS3q-vOrSdYJMUqjBY-f4SLffkAQVVXo0jCKUuSaIzIiJi4gro&usqp=CAU"
                            , description = "search icon"
                            }
                        )
                    ]
                    (Input.text
                        [ centerY
                        , centerX
                        , Font.alignLeft
                        , height (px 30)
                        , Background.color (Element.rgba 0 0 0 0)
                        , width fill
                        , Border.color (Element.rgba 0 0 0 0)
                        , Font.size 16
                        , Font.color (Element.rgb 1 1 1)
                        , Element.focused
                            [ Border.color (Element.rgba 0 0 0 0) ]
                        , moveUp 6
                        , Input.focusedOnLoad
                        , onEnter
                            (case List.head searchDisplay of
                                Nothing ->
                                    Noop

                                Just entry ->
                                    ClickSearch entry
                            )
                        ]
                        { label = Input.labelHidden ""
                        , onChange =
                            \new -> Search new
                        , placeholder =
                            Just (Input.placeholder [ moveUp 6, Font.size 15 ] (Element.text "search a county, state, or country"))
                        , text = searchTerm
                        }
                    )

            else
                Element.row
                    [ width fill
                    , height (px 40)
                    , Border.rounded 20
                    , Background.color (Element.rgb 0 0 0)
                    , Border.color (Element.rgb255 58 58 60)
                    , Border.width 1
                    , spacing 15
                    , clip
                    , mouseOver
                        [ Background.color
                            (Element.rgb255 15 15 15)
                        ]
                    ]
                    [ Element.image
                        [ centerY
                        , alignLeft
                        , moveRight 15
                        , height (px 15)
                        ]
                        { src = "https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcTS3q-vOrSdYJMUqjBY-f4SLffkAQVVXo0jCKUuSaIzIiJi4gro&usqp=CAU"
                        , description = "search icon"
                        }
                    , Input.text
                        [ centerY
                        , Font.alignLeft
                        , height (px 30)
                        , Background.color (Element.rgba 0 0 0 0)
                        , width fill
                        , Border.color (Element.rgba 0 0 0 0)
                        , Font.size 16
                        , Font.color (Element.rgb 1 1 1)
                        , Element.focused [ Border.color (Element.rgba 0 0 0 0) ]
                        , moveUp 6
                        , Input.focusedOnLoad
                        , onEnter
                            (case List.head searchDisplay of
                                Nothing ->
                                    Noop

                                Just entry ->
                                    ClickSearch entry
                            )
                        ]
                        { label = Input.labelHidden ""
                        , onChange =
                            \new -> Search new
                        , placeholder =
                            Just (Input.placeholder [ moveUp 6, Font.size 16 ] (Element.text "search a county, state, or country"))
                        , text = searchTerm
                        }
                    ]
    in
    Element.el
        constraints
        (Element.column
            ([ width shrink
             , height shrink
             , spacing 13
             ]
                ++ (if mobile then
                        [ scale 0.8 ]

                    else
                        []
                   )
            )
            [ Element.column
                [ width (px 385)
                , height shrink
                , centerX
                , spacing 10
                , Background.color (Element.rgb 0 0 0)
                , Border.shadow
                    { offset = ( 0, 1 )
                    , size = 4
                    , blur = 15
                    , color = Element.rgb 0.1 0.1 0.1
                    }
                , Border.rounded 25
                , padding 25
                ]
                [ Element.el
                    [ width shrink
                    , height shrink
                    , centerX
                    , Font.color (Element.rgb 1 1 1)
                    , Font.size 36
                    , Font.bold
                    ]
                    (Element.text "Coronalert 🚨")
                , Element.el
                    [ width shrink
                    , height shrink
                    , centerX
                    , Font.color (Element.rgb 1 1 1)
                    , Font.size 18
                    , Font.light
                    ]
                    (Element.text "The COVID Map with Text Alerts")
                ]
            , Element.el
                ([ width fill
                 , height fill
                 , Element.behindContent
                    (Element.el
                        [ width fill
                        , height
                            (px
                                (List.length searchDisplay
                                    * 40
                                    + 20
                                )
                            )
                        , moveDown 20
                        , Border.roundEach
                            { topLeft = 0
                            , topRight = 0
                            , bottomLeft = 20
                            , bottomRight = 20
                            }
                        , Border.shadow
                            { offset = ( 0, 1 )
                            , size = 4
                            , blur = 15
                            , color = Element.rgb 0.1 0.1 0.1
                            }
                        , Background.color (Element.rgb255 0 0 0)
                        , clip
                        ]
                        (Element.column
                            [ width fill
                            , height (px (List.length searchDisplay * 40))
                            , moveDown 20
                            ]
                            (List.map searchEntry searchDisplay)
                        )
                    )
                 ]
                    ++ (if mobile then
                            [ width (px 300)
                            , centerX
                            ]

                        else
                            []
                       )
                )
                search
            ]
        )


searchEntry : IdRecordEntry -> Element Msg
searchEntry entry =
    Input.button
        [ Background.color (Element.rgba 0 0 0 0)
        , width fill
        , height (px 40)
        , Font.center
        , Element.focused
            [ Border.color (Element.rgba 0 0 0 0) ]
        , mouseOver [ Background.color (Element.rgb255 15 15 15) ]
        ]
        { onPress = Just (ClickSearch entry)
        , label =
            Element.row
                [ width fill
                , height fill
                , spacing 30
                ]
                [ Element.image
                    [ centerY
                    , alignLeft
                    , moveRight 17
                    , height (px 15)
                    , moveUp 1
                    ]
                    { src = "https://raw.githubusercontent.com/viyer28/coronalert/master/pin_icon.png"
                    , description = "pin icon"
                    }
                , Element.el
                    [ Font.size 14
                    , Font.color
                        (Element.rgb 1 1 1)
                    , centerY
                    , width shrink
                    , height shrink
                    , Font.extraLight
                    , centerY
                    , Font.alignLeft
                    ]
                    (Element.text entry.id)
                ]
        }


actions : Element Msg
actions =
    Element.column
        [ width shrink
        , height shrink
        , alignRight
        , moveLeft 25
        , alignTop
        , moveDown 25
        , spacing 10
        ]
        [ Element.newTabLink
            [ width (px 50)
            , height (px 50)
            , Background.color blue
            , Border.rounded 25
            , clip
            , Border.shadow
                { offset = ( 0, 1 )
                , size = 2
                , blur = 15
                , color = Element.rgb 0.1 0.1 0.1
                }
            , mouseOver
                [ Background.color
                    (Element.rgb255 51 153 255)
                ]
            , Element.focused [ Border.color (Element.rgba 0 0 0 0) ]
            ]
            { url = "https://twitter.com/intent/tweet?url=https%3A%2F%2Fwww.coronalert.live&text=Check%20out%20Coronalert%20-%20the%20COVID%20map%20with%20text%20alerts.%20Stay%20safe%2C%20stay%20alert%21&hashtags=COVID%2Ccoronavirus"
            , label =
                Element.image
                    [ centerY
                    , centerX
                    , height (px 20)
                    ]
                    { src = "https://raw.githubusercontent.com/viyer28/coronalert/master/share_icon.png"
                    , description = "share button"
                    }
            }
        , Input.button
            [ width (px 50)
            , height (px 50)
            , Background.color (Element.rgb255 94 92 230)
            , Border.rounded 25
            , clip
            , Border.shadow
                { offset = ( 0, 1 )
                , size = 2
                , blur = 15
                , color = Element.rgb 0.1 0.1 0.1
                }
            , mouseOver
                [ Background.color
                    (Element.rgb255 111 109 232)
                ]
            , Element.focused
                [ Border.color (Element.rgba 0 0 0 0) ]
            ]
            { onPress = Just ClickPremium
            , label =
                Element.image
                    [ centerY
                    , centerX
                    , height (px 20)
                    ]
                    { src = " https://raw.githubusercontent.com/viyer28/coronalert/master/premium_icon.png"
                    , description = "premium button"
                    }
            }
        ]


shareButton : Element Msg
shareButton =
    Element.newTabLink
        [ width (px 30)
        , height (px 30)
        , centerX
        , alignTop
        , moveDown 129
        , moveLeft 145
        , Background.color blue
        , Border.rounded 25
        , clip
        , Border.shadow
            { offset = ( 0, 1 )
            , size = 2
            , blur = 15
            , color = Element.rgb 0.1 0.1 0.1
            }
        , mouseOver
            [ Background.color
                (Element.rgb255 51 153 255)
            ]
        , Element.focused [ Border.color (Element.rgba 0 0 0 0) ]
        ]
        { url = "https://twitter.com/intent/tweet?url=https%3A%2F%2Fwww.coronalert.live&text=Check%20out%20Coronalert%20-%20the%20COVID%20map%20with%20text%20alerts.%20Stay%20safe%2C%20stay%20alert%21&hashtags=COVID%2Ccoronavirus"
        , label =
            Element.image
                [ centerY
                , centerX
                , height (px 12)
                ]
                { src = "https://raw.githubusercontent.com/viyer28/coronalert/master/share_icon.png"
                , description = "share button"
                }
        }


premiumButton : Element Msg
premiumButton =
    Input.button
        [ width (px 30)
        , height (px 30)
        , centerX
        , alignTop
        , moveDown 129
        , moveRight 145
        , Background.color (Element.rgb255 94 92 230)
        , Border.rounded 25
        , clip
        , Border.shadow
            { offset = ( 0, 1 )
            , size = 2
            , blur = 15
            , color = Element.rgb 0.1 0.1 0.1
            }
        , mouseOver
            [ Background.color
                (Element.rgb255 111 109 232)
            ]
        , Element.focused
            [ Border.color (Element.rgba 0 0 0 0) ]
        ]
        { onPress = Just ClickPremium
        , label =
            Element.image
                [ centerY
                , centerX
                , height (px 12)
                ]
                { src = " https://raw.githubusercontent.com/viyer28/coronalert/master/premium_icon.png"
                , description = "premium button"
                }
        }


premium : Bool -> String -> Maybe Bool -> Bool -> Element Msg
premium displayPremium phoneNum validPhone success =
    if displayPremium then
        Element.el
            [ width fill
            , height fill
            , Element.inFront
                (Element.el
                    [ width fill
                    , height fill
                    , Background.color (Element.rgba 0 0 0 0.5)
                    , Events.onClick ExitPremium
                    ]
                    Element.none
                )
            , Element.inFront
                (Element.el
                    [ centerX
                    , centerY
                    , width shrink
                    , height shrink
                    , Background.color (Element.rgb255 0 0 0)
                    , Border.rounded 40
                    , Border.shadow
                        { offset = ( 0, 1 )
                        , size = 4
                        , blur = 20
                        , color = Element.rgb 0.1 0.1 0.1
                        }
                    , paddingEach
                        { top = 14
                        , left = 0
                        , right = 0
                        , bottom = 40
                        }
                    ]
                    (Element.column
                        [ spacing 5 ]
                        [ Input.button
                            [ alignLeft
                            , moveRight 20
                            , moveDown 2
                            , Background.color (Element.rgb255 255 69 58)
                            , width (px 16)
                            , height (px 16)
                            , Border.rounded 8
                            , Element.focused []
                            , mouseOver
                                [ Background.color (Element.rgb255 255 90 82) ]
                            ]
                            { onPress = Just ExitPremium
                            , label =
                                Element.image
                                    [ centerX
                                    , centerY
                                    , height (px 7)
                                    , width (px 7)
                                    ]
                                    { src = " https://raw.githubusercontent.com/viyer28/coronalert/master/close_icon.png"
                                    , description = "back button"
                                    }
                            }
                        , Element.column
                            [ spacing 10
                            , centerX
                            , paddingEach
                                { top = 0
                                , left = 40
                                , right = 40
                                , bottom = 0
                                }
                            ]
                            ([ Element.el
                                [ Font.color (Element.rgb 1 1 1)
                                , Font.size 28
                                , Font.bold
                                , Font.center
                                , centerX
                                , width shrink
                                , height shrink
                                ]
                                (Element.text "Love Coronalert? ❤️")
                             , Element.el
                                [ Font.color (Element.rgb 1 1 1)
                                , Font.size 24
                                , Font.light
                                , Font.center
                                , centerX
                                , width shrink
                                , height shrink
                                ]
                                (Element.text "Upgrade to Coronalert Premium")
                             , Element.el
                                [ Font.color (Element.rgb 1 1 1)
                                , Font.size 72
                                , Font.light
                                , Font.center
                                , centerX
                                , width shrink
                                , height shrink
                                , padding 10
                                ]
                                (Element.text "$5")
                             , Element.column
                                [ centerX
                                , padding 15
                                , spacing 10
                                , width shrink
                                , height shrink
                                ]
                                [ Element.el
                                    [ Font.color (Element.rgb 1 1 1)
                                    , Font.size 18
                                    , Font.regular
                                    , Font.center
                                    , alignLeft
                                    , width shrink
                                    , height shrink
                                    ]
                                    (Element.text "✅ Unlimited Subscriptions")
                                , Element.el
                                    [ Font.color (Element.rgb 1 1 1)
                                    , Font.size 18
                                    , Font.regular
                                    , Font.center
                                    , alignLeft
                                    , width shrink
                                    , height shrink
                                    ]
                                    (Element.text "✅ Unlimited Alerts")
                                , Element.el
                                    [ Font.color (Element.rgb 1 1 1)
                                    , Font.size 18
                                    , Font.regular
                                    , Font.center
                                    , alignLeft
                                    , width shrink
                                    , height shrink
                                    ]
                                    (Element.text "✅ 20% Donated to:")
                                , Element.newTabLink
                                    [ Font.color (Element.rgba 1 1 1 0.75)
                                    , Font.size 14
                                    , Font.regular
                                    , Font.center
                                    , Font.underline
                                    , alignLeft
                                    , width shrink
                                    , height shrink
                                    , mouseOver [ Font.color (Element.rgba 1 1 1 1) ]
                                    ]
                                    { url = "https://www.feedingamerica.org/about-us/press-room/feeding-america-establishes-covid-19-response-fund-help-food-banks-during"
                                    , label = Element.text "Feeding America COVID-19 Response Fund"
                                    }
                                ]
                             ]
                                ++ premiumFooter success phoneNum validPhone
                            )
                        ]
                    )
                )
            ]
            Element.none

    else
        Element.none


premiumFooter : Bool -> String -> Maybe Bool -> List (Element Msg)
premiumFooter success phoneNum validPhone =
    if success then
        let
            shareUrl =
                "https://twitter.com/intent/tweet?url=http%3A%2F%2Fwww.coronalert.live&text=Check%20out%20Coronalert%20-%20the%20best%20COVID%20map%20on%20the%20web.%20Stay%20safe%2C%20stay%20alert%21&hashtags=COVID%2Ccoronavirus"
        in
        [ Element.column
            [ spacing 5 ]
            [ Element.el
                [ paddingEach { top = 5, bottom = 0, left = 5, right = 5 }
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
                    (Element.el
                        [ Font.size 14
                        , Font.color (Element.rgb 1 1 1)
                        , centerX
                        , paddingEach { top = 0, bottom = 5, left = 5, right = 5 }
                        , Font.center
                        , Font.bold
                        ]
                        (Element.text "Thank you for upgrading to Premium 🙏")
                    )
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
                    (Element.text "Upgraded ✓")
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
                    , mouseOver
                        [ Background.color
                            (Element.rgb255 51 153 255)
                        ]
                    ]
                    { url = shareUrl
                    , label =
                        Element.el
                            [ Font.size 16
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
        ]

    else
        [ Element.column
            [ centerX ]
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
                    , onEnter Upgrade
                    , mouseOver
                        [ Background.color
                            (Element.rgb255 242 242 247)
                        ]
                    ]
                    { label =
                        Input.labelAbove
                            [ Font.size 14
                            , Font.color (Element.rgb 1 1 1)
                            , centerX
                            , paddingEach { top = 0, bottom = 5, left = 5, right = 5 }
                            , Font.center
                            , Font.bold
                            ]
                            (Element.text "Enter your phone number to upgrade")
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
                    [ Background.color (Element.rgb255 94 92 230)
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
                    , mouseOver
                        [ Background.color
                            (Element.rgb255 111 109 232)
                        ]
                    ]
                    { onPress = Just Upgrade
                    , label =
                        Element.el
                            [ Font.size 16
                            , Font.color
                                (Element.rgb 1 1 1)
                            , centerX
                            , centerY
                            , width shrink
                            , height shrink
                            , Font.extraLight
                            ]
                            (Element.text "Get Premium")
                    }
                )
            ]
        ]


map : Model -> Html Msg
map model =
    let
        noHoverOption =
            if model.device.class == Phone || model.device.class == Tablet then
                []

            else
                [ onMouseMove Hover ]

        initialZoom =
            if model.device.class == Phone then
                [ Style.defaultZoomLevel 2.5 ]

            else
                [ Style.defaultZoomLevel 3.25 ]
    in
    div
        [ Html.Attributes.style "height" "100vh" ]
        [ Mapbox.Element.map
            ([ id "my-map"
             , onClick Click
             , onTouchEnd Touch
             , eventFeaturesLayers [ "counties", "states", "countries" ]
             , hoverFeature model.hoveredFeature
             ]
                ++ noHoverOption
            )
            (Dark.styleWithAttr
                [ Source.geoJSONFromValue "counties"
                    []
                    (encodeRecords (idsToRecords model.counties))
                , Source.geoJSONFromValue "states"
                    []
                    (encodeRecords (idsToRecords model.states))
                , Source.geoJSONFromValue "countries"
                    []
                    (encodeRecords (idsToRecords model.countries))
                ]
                [ Layer.circle "counties"
                    "counties"
                    [ Layer.circleRadius
                        (countySize
                            (minConfirmed (idsToRecords model.counties))
                            (maxConfirmed (idsToRecords model.counties))
                        )
                    , Layer.circleColor featureColor
                    , Layer.circleOpacity countyOpacity
                    , Layer.minzoom 5
                    ]
                , Layer.circle "states"
                    "states"
                    [ Layer.circleRadius
                        (stateSize
                            (minConfirmed (idsToRecords model.counties))
                            (maxConfirmed (idsToRecords model.countries))
                        )
                    , Layer.circleColor featureColor
                    , Layer.circleOpacity stateOpacity
                    , Layer.maxzoom 5.25
                    ]
                , Layer.circle "countries"
                    "countries"
                    [ Layer.circleRadius
                        (countrySize
                            (minConfirmed (idsToRecords model.countries))
                            (maxConfirmed (idsToRecords model.countries))
                        )
                    , Layer.circleColor featureColor
                    , Layer.circleOpacity countryOpacity
                    ]
                ]
                initialZoom
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


clickView : Bool -> String -> Maybe Bool -> Maybe Bool -> Maybe FeatureEntry -> Element Msg
clickView mobile phoneNum validPhone invalidSub entry =
    case entry of
        Nothing ->
            Element.none

        Just { geometry, properties, layer } ->
            let
                ( title, subtitle ) =
                    titleLabel layer properties
            in
            Element.el
                ([ width shrink
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
                    ++ (if mobile then
                            [ centerX
                            , alignBottom
                            , moveUp 50
                            , scale 0.8
                            ]

                        else
                            [ alignLeft
                            , alignBottom
                            , moveUp 25
                            , moveRight 25
                            ]
                       )
                )
                (Element.column
                    [ spacing 5
                    , centerX
                    ]
                    ([ Element.el
                        [ Font.color (Element.rgb 1 1 1)
                        , Font.size 28
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
                        ++ textView phoneNum validPhone invalidSub properties title
                    )
                )


textView : String -> Maybe Bool -> Maybe Bool -> PropertiesEntry -> String -> List (Element Msg)
textView phoneNum validPhone invalidSub properties title =
    case ( validPhone, invalidSub ) of
        ( Just True, Just False ) ->
            let
                shareUrl =
                    "https://twitter.com/intent/tweet?url=http%3A%2F%2Fwww.coronalert.live&text="
                        ++ String.fromInt properties.confirmed
                        ++ "%20cases%2C%20"
                        ++ String.fromInt properties.deaths
                        ++ "%20deaths%20in%20"
                        ++ String.replace " " "%20" title
                        ++ ".%20Stay%20safe%2C%20stay%20alert%21&hashtags=COVID%2Ccoronavirus"
            in
            [ Element.el
                [ paddingEach { top = 10, bottom = 10, left = 5, right = 5 }
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
                    (Element.column
                        [ spacing 5
                        , Font.bold
                        ]
                        [ Element.text "Check your phone for a confirmation text"
                        , Element.el
                            [ Font.center
                            , centerX
                            , Font.light
                            ]
                            (Element.text "It will arrive within 30 seconds")
                        ]
                    )
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
                    , mouseOver
                        [ Background.color
                            (Element.rgb255 51 153 255)
                        ]
                    ]
                    { url = shareUrl
                    , label =
                        Element.el
                            [ Font.size 16
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
                    , mouseOver
                        [ Background.color
                            (Element.rgb255 242 242 247)
                        ]
                    ]
                    { label =
                        Input.labelAbove
                            [ Font.size 14
                            , Font.color (Element.rgb 1 1 1)
                            , centerX
                            , paddingEach { top = 0, bottom = 5, left = 5, right = 5 }
                            , Font.center
                            ]
                            (Element.column
                                [ paddingEach { top = 0, bottom = 5, left = 5, right = 5 }
                                , spacing 5
                                ]
                                [ Element.el
                                    [ Font.bold ]
                                    (Element.text "Enter your phone number for text updates")
                                , Element.el
                                    [ Font.center
                                    , centerX
                                    , Font.light
                                    ]
                                    (Element.text "Text 'STOP' anytime to unsubscribe")
                                ]
                            )
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
                    , mouseOver
                        [ Background.color
                            (Element.rgb255 51 153 255)
                        ]
                    ]
                    { onPress = Just AlertMe
                    , label =
                        Element.el
                            [ Font.size 16
                            , Font.color
                                (Element.rgb 1 1 1)
                            , centerX
                            , centerY
                            , width shrink
                            , height shrink
                            , Font.extraLight
                            ]
                            (Element.text ("Subscribe to " ++ title))
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


port processPremium : PremiumEntry -> Cmd msg


port firebaseUpgrade : String -> Cmd msg


port invalidSubscription : (Bool -> msg) -> Sub msg
