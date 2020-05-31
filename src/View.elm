module View exposing (..)

import Colors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, div, text)
import Html.Attributes
import JSON exposing (encodeRecords, idsToRecords)
import Mapbox.Element exposing (..)
import Mapbox.Layer as Layer
import Mapbox.Source as Source
import Mapbox.Style as Style
import Styles.Dark as Dark
import Types exposing (..)
import ViewHelpers exposing (..)


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


premium : Bool -> Bool -> String -> Maybe Bool -> Bool -> Element Msg
premium mobile displayPremium phoneNum validPhone success =
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
                    ([ centerX
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
                        ++ (if mobile then
                                [ scale 0.8 ]

                            else
                                []
                           )
                    )
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
        [ Html.Attributes.style "height" (String.fromInt model.height ++ "px")
        , Html.Attributes.style "width" (String.fromInt model.width ++ "px")
        ]
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
