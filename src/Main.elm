-- module Main: main application


module Main exposing (main)

import Browser
import Browser.Events as Ev
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background exposing (color)
import JSON exposing (featureDecoder)
import Json.Decode as D
import List exposing (map)
import LngLat exposing (LngLat)
import Networking exposing (..)
import Ports exposing (..)
import Types exposing (..)
import UpdateHelpers exposing (..)
import Url
import View exposing (..)



-- defines the web application


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
                        (View.header isMobile model.search model.searchResults)
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
                                    isMobile
                                    model.displayPremium
                                    model.phoneNumber
                                    model.validPhone
                                    model.premiumSuccess
                                )
                           ]
                    )
                    (Element.html (View.map model))
                ]
            )
        ]
    }
