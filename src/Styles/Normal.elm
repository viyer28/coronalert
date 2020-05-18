module Styles.Normal exposing (style)

import LngLat exposing (LngLat)
import Mapbox.Expression as E exposing (false, float, str, true)
import Mapbox.Layer as Layer
import Mapbox.Source as Source
import Mapbox.Style as Style exposing (Style(..))


style : Style
style =
    Style
        { transition = Style.defaultTransition
        , light = Style.defaultLight
        , layers =
            [ Layer.background "background" [ Layer.backgroundColor (E.rgba 230 233 232 1) ]
            , Layer.fill "national_park"
                "composite"
                [ Layer.minzoom 5
                , Layer.filter (E.getProperty (str "class") |> E.isEqual (str "national_park"))
                , Layer.sourceLayer "landuse_overlay"
                , Layer.fillColor (E.rgba 218 243 201 1)
                , Layer.fillOpacity (E.zoom |> E.interpolate E.Linear [ ( 5, float 0 ), ( 6, float 0.5 ) ])
                ]
            , Layer.fill "landuse"
                "composite"
                [ Layer.minzoom 5
                , Layer.filter
                    (E.getProperty (str "class")
                        |> E.matchesStr [ ( "airport", true ), ( "hospital", true ), ( "park", true ), ( "pitch", true ), ( "school", true ) ] false
                    )
                , Layer.sourceLayer "landuse"
                , Layer.fillColor
                    (E.getProperty (str "class")
                        |> E.matchesStr [ ( "park", E.rgba 218 243 201 1 ), ( "pitch", E.rgba 218 243 201 1 ) ] (E.rgba 235 238 237 1)
                    )
                , Layer.fillOpacity (E.zoom |> E.interpolate E.Linear [ ( 5, float 0 ), ( 6, float 1 ) ])
                ]
            , Layer.line "waterway"
                "composite"
                [ Layer.minzoom 8
                , Layer.filter
                    (E.all
                        [ E.geometryType |> E.isEqual (str "LineString")
                        , E.getProperty (str "class") |> E.matchesStr [ ( "canal", true ), ( "river", true ) ] false
                        ]
                    )
                , Layer.sourceLayer "waterway"
                , Layer.lineColor (E.rgba 120 188 237 1)
                , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.3) [ ( 8.5, float 0.1 ), ( 20, float 8 ) ])
                , Layer.lineOpacity (E.zoom |> E.interpolate E.Linear [ ( 8, float 0 ), ( 8.5, float 1 ) ])
                , Layer.lineJoin E.rounded
                , Layer.lineCap E.rounded
                ]
            , Layer.fill "water" "composite" [ Layer.sourceLayer "water", Layer.fillColor (E.rgba 185 227 248 1) ]
            , Layer.fill "aeroway-polygon"
                "composite"
                [ Layer.minzoom 10
                , Layer.filter
                    (E.all
                        [ E.geometryType |> E.isEqual (str "Polygon")
                        , E.getProperty (str "type")
                            |> E.matchesStr [ ( "helipad", true ), ( "runway", true ), ( "taxiway", true ) ] false
                        ]
                    )
                , Layer.sourceLayer "aeroway"
                , Layer.fillColor (E.rgba 196 196 196 1)
                ]
            , Layer.line "aeroway-line"
                "composite"
                [ Layer.minzoom 9
                , Layer.filter
                    (E.all
                        [ E.geometryType |> E.isEqual (str "LineString")
                        , E.getProperty (str "type") |> E.matchesStr [ ( "runway", true ), ( "taxiway", true ) ] false
                        ]
                    )
                , Layer.sourceLayer "aeroway"
                , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.5) [ ( 10, float 0.5 ), ( 18, float 20 ) ])
                , Layer.lineColor (E.rgba 196 196 196 1)
                ]
            , Layer.fill "building"
                "composite"
                [ Layer.minzoom 15
                , Layer.filter
                    (E.all
                        [ E.getProperty (str "type") |> E.notEqual (str "building:part")
                        , E.getProperty (str "underground") |> E.isEqual (str "false")
                        ]
                    )
                , Layer.sourceLayer "building"
                , Layer.fillColor (E.rgba 189 187 225 0.2)
                ]
            , Layer.line "pedestrian-path"
                "composite"
                [ Layer.minzoom 14
                , Layer.filter
                    (E.all
                        [ E.geometryType |> E.isEqual (str "LineString")
                        , E.getProperty (str "type") |> E.notEqual (str "platform")
                        , E.getProperty (str "class") |> E.matchesStr [ ( "path", true ), ( "pedestrian", true ) ] false
                        ]
                    )
                , Layer.sourceLayer "road"
                , Layer.lineWidth
                    (E.zoom
                        |> E.interpolate (E.Exponential 1.5)
                            [ ( 14, E.getProperty (str "class") |> E.matchesStr [ ( "pedestrian", float 1 ) ] (float 0.75) )
                            , ( 20, E.getProperty (str "class") |> E.matchesStr [ ( "pedestrian", float 8 ) ] (float 5) )
                            ]
                    )
                , Layer.lineColor
                    (E.getProperty (str "type")
                        |> E.matchesStr [ ( "crossing", E.rgba 221 208 186 1 ), ( "sidewalk", E.rgba 221 208 186 1 ) ] (E.rgba 199 184 157 1)
                    )
                , Layer.lineJoin E.rounded
                , Layer.lineCap E.rounded
                ]
            , Layer.line "tunnel"
                "composite"
                [ Layer.minzoom 13
                , Layer.filter
                    (E.all
                        [ E.geometryType |> E.isEqual (str "LineString")
                        , E.getProperty (str "type") |> E.notEqual (str "service:parking_aisle")
                        , E.getProperty (str "structure") |> E.isEqual (str "tunnel")
                        , E.getProperty (str "class")
                            |> E.matchesStr
                                [ ( "motorway", true )
                                , ( "motorway_link", true )
                                , ( "trunk", true )
                                , ( "trunk_link", true )
                                , ( "primary", true )
                                , ( "primary_link", true )
                                , ( "secondary", true )
                                , ( "secondary_link", true )
                                , ( "tertiary", true )
                                , ( "tertiary_link", true )
                                , ( "street", true )
                                , ( "street_limited", true )
                                , ( "service", true )
                                , ( "track", true )
                                ]
                                false
                        ]
                    )
                , Layer.sourceLayer "road"
                , Layer.lineWidth
                    (E.zoom
                        |> E.interpolate (E.Exponential 1.5)
                            [ ( 5
                              , E.getProperty (str "class")
                                    |> E.matchesStr [ ( "motorway", float 0.5 ), ( "trunk", float 0.5 ), ( "primary", float 0.5 ), ( "tertiary", float 0.01 ) ] (float 0)
                              )
                            , ( 12
                              , E.getProperty (str "class")
                                    |> E.matchesStr
                                        [ ( "motorway", float 3 )
                                        , ( "trunk", float 3 )
                                        , ( "primary", float 3 )
                                        , ( "secondary", float 2 )
                                        , ( "tertiary", float 2 )
                                        , ( "motorway_link", float 0.5 )
                                        , ( "trunk_link", float 0.5 )
                                        , ( "street", float 0.5 )
                                        , ( "street_limited", float 0.5 )
                                        ]
                                        (float 0)
                              )
                            , ( 18
                              , E.getProperty (str "class")
                                    |> E.matchesStr
                                        [ ( "motorway", float 30 )
                                        , ( "trunk", float 30 )
                                        , ( "primary", float 30 )
                                        , ( "secondary", float 24 )
                                        , ( "tertiary", float 24 )
                                        , ( "motorway_link", float 12 )
                                        , ( "trunk_link", float 12 )
                                        , ( "street", float 12 )
                                        , ( "street_limited", float 12 )
                                        ]
                                        (float 10)
                              )
                            ]
                    )
                , Layer.lineColor
                    (E.getProperty (str "class")
                        |> E.matchesStr
                            [ ( "primary_link", E.rgba 255 251 244 1 )
                            , ( "secondary_link", E.rgba 255 251 244 1 )
                            , ( "tertiary_link", E.rgba 255 251 244 1 )
                            , ( "street", E.rgba 255 251 244 1 )
                            , ( "street_limited", E.rgba 255 251 244 1 )
                            , ( "service", E.rgba 255 251 244 1 )
                            , ( "track", E.rgba 255 251 244 1 )
                            ]
                            (E.rgba 255 255 255 1)
                    )
                , Layer.lineDasharray (E.floats [ 0.2, 0.2 ])
                , Layer.lineJoin E.rounded
                ]
            , Layer.line "road"
                "composite"
                [ Layer.minzoom 5
                , Layer.filter
                    (E.all
                        [ E.geometryType |> E.isEqual (str "LineString")
                        , E.getProperty (str "type") |> E.notEqual (str "service:parking_aisle")
                        , E.getProperty (str "structure") |> E.matchesStr [ ( "bridge", false ), ( "tunnel", false ) ] true
                        , E.getProperty (str "class")
                            |> E.matchesStr
                                [ ( "motorway", true )
                                , ( "motorway_link", true )
                                , ( "trunk", true )
                                , ( "trunk_link", true )
                                , ( "primary", true )
                                , ( "primary_link", true )
                                , ( "secondary", true )
                                , ( "secondary_link", true )
                                , ( "tertiary", true )
                                , ( "tertiary_link", true )
                                , ( "street", true )
                                , ( "street_limited", true )
                                , ( "service", true )
                                , ( "track", true )
                                ]
                                false
                        ]
                    )
                , Layer.sourceLayer "road"
                , Layer.lineWidth
                    (E.zoom
                        |> E.interpolate (E.Exponential 1.5)
                            [ ( 5
                              , E.getProperty (str "class")
                                    |> E.matchesStr [ ( "motorway", float 0.5 ), ( "trunk", float 0.5 ), ( "primary", float 0.5 ), ( "tertiary", float 0.01 ) ] (float 0)
                              )
                            , ( 12
                              , E.getProperty (str "class")
                                    |> E.matchesStr
                                        [ ( "motorway", float 3 )
                                        , ( "trunk", float 3 )
                                        , ( "primary", float 3 )
                                        , ( "secondary", float 2 )
                                        , ( "tertiary", float 2 )
                                        , ( "motorway_link", float 0.5 )
                                        , ( "trunk_link", float 0.5 )
                                        , ( "street", float 0.5 )
                                        , ( "street_limited", float 0.5 )
                                        ]
                                        (float 0)
                              )
                            , ( 18
                              , E.getProperty (str "class")
                                    |> E.matchesStr
                                        [ ( "motorway", float 30 )
                                        , ( "trunk", float 30 )
                                        , ( "primary", float 30 )
                                        , ( "secondary", float 24 )
                                        , ( "tertiary", float 24 )
                                        , ( "motorway_link", float 12 )
                                        , ( "trunk_link", float 12 )
                                        , ( "street", float 12 )
                                        , ( "street_limited", float 12 )
                                        ]
                                        (float 10)
                              )
                            ]
                    )
                , Layer.lineColor (E.rgba 249 249 249 1)
                , Layer.lineJoin E.rounded
                , Layer.lineCap E.rounded
                ]
            , Layer.line "bridge-case"
                "composite"
                [ Layer.minzoom 13
                , Layer.filter
                    (E.all
                        [ E.geometryType |> E.isEqual (str "LineString")
                        , E.getProperty (str "type") |> E.notEqual (str "service:parking_aisle")
                        , E.getProperty (str "structure") |> E.isEqual (str "bridge")
                        , E.getProperty (str "class")
                            |> E.matchesStr
                                [ ( "motorway", true )
                                , ( "motorway_link", true )
                                , ( "trunk", true )
                                , ( "trunk_link", true )
                                , ( "primary", true )
                                , ( "primary_link", true )
                                , ( "secondary", true )
                                , ( "secondary_link", true )
                                , ( "tertiary", true )
                                , ( "tertiary_link", true )
                                , ( "street", true )
                                , ( "street_limited", true )
                                , ( "service", true )
                                , ( "track", true )
                                ]
                                false
                        ]
                    )
                , Layer.sourceLayer "road"
                , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.5) [ ( 10, float 1 ), ( 16, float 2 ) ])
                , Layer.lineColor (E.rgba 236 223 202 1)
                , Layer.lineGapWidth
                    (E.zoom
                        |> E.interpolate (E.Exponential 1.5)
                            [ ( 5
                              , E.getProperty (str "class")
                                    |> E.matchesStr [ ( "motorway", float 0.5 ), ( "trunk", float 0.5 ), ( "primary", float 0.5 ), ( "tertiary", float 0.01 ) ] (float 0)
                              )
                            , ( 12
                              , E.getProperty (str "class")
                                    |> E.matchesStr
                                        [ ( "motorway", float 3 )
                                        , ( "trunk", float 3 )
                                        , ( "primary", float 3 )
                                        , ( "secondary", float 2 )
                                        , ( "tertiary", float 2 )
                                        , ( "motorway_link", float 0.5 )
                                        , ( "trunk_link", float 0.5 )
                                        , ( "street", float 0.5 )
                                        , ( "street_limited", float 0.5 )
                                        ]
                                        (float 0)
                              )
                            , ( 18
                              , E.getProperty (str "class")
                                    |> E.matchesStr
                                        [ ( "motorway", float 30 )
                                        , ( "trunk", float 30 )
                                        , ( "primary", float 30 )
                                        , ( "secondary", float 24 )
                                        , ( "tertiary", float 24 )
                                        , ( "motorway_link", float 12 )
                                        , ( "trunk_link", float 12 )
                                        , ( "street", float 12 )
                                        , ( "street_limited", float 12 )
                                        ]
                                        (float 10)
                              )
                            ]
                    )
                , Layer.lineJoin E.rounded
                ]
            , Layer.line "bridge"
                "composite"
                [ Layer.minzoom 13
                , Layer.filter
                    (E.all
                        [ E.geometryType |> E.isEqual (str "LineString")
                        , E.getProperty (str "type") |> E.notEqual (str "service:parking_aisle")
                        , E.getProperty (str "structure") |> E.isEqual (str "bridge")
                        , E.getProperty (str "class")
                            |> E.matchesStr
                                [ ( "motorway", true )
                                , ( "motorway_link", true )
                                , ( "trunk", true )
                                , ( "trunk_link", true )
                                , ( "primary", true )
                                , ( "primary_link", true )
                                , ( "secondary", true )
                                , ( "secondary_link", true )
                                , ( "tertiary", true )
                                , ( "tertiary_link", true )
                                , ( "service", true )
                                , ( "street", true )
                                , ( "street_limited", true )
                                , ( "track", true )
                                ]
                                false
                        ]
                    )
                , Layer.sourceLayer "road"
                , Layer.lineWidth
                    (E.zoom
                        |> E.interpolate (E.Exponential 1.5)
                            [ ( 5
                              , E.getProperty (str "class")
                                    |> E.matchesStr [ ( "motorway", float 0.5 ), ( "trunk", float 0.5 ), ( "primary", float 0.5 ), ( "tertiary", float 0.01 ) ] (float 0)
                              )
                            , ( 12
                              , E.getProperty (str "class")
                                    |> E.matchesStr
                                        [ ( "motorway", float 3 )
                                        , ( "trunk", float 3 )
                                        , ( "primary", float 3 )
                                        , ( "secondary", float 2 )
                                        , ( "tertiary", float 2 )
                                        , ( "motorway_link", float 0.5 )
                                        , ( "trunk_link", float 0.5 )
                                        , ( "street", float 0.5 )
                                        , ( "street_limited", float 0.5 )
                                        ]
                                        (float 0)
                              )
                            , ( 18
                              , E.getProperty (str "class")
                                    |> E.matchesStr
                                        [ ( "motorway", float 30 )
                                        , ( "trunk", float 30 )
                                        , ( "primary", float 30 )
                                        , ( "secondary", float 24 )
                                        , ( "tertiary", float 24 )
                                        , ( "motorway_link", float 12 )
                                        , ( "trunk_link", float 12 )
                                        , ( "street", float 12 )
                                        , ( "street_limited", float 12 )
                                        ]
                                        (float 10)
                              )
                            ]
                    )
                , Layer.lineColor
                    (E.getProperty (str "class")
                        |> E.matchesStr
                            [ ( "primary_link", E.rgba 255 251 244 1 )
                            , ( "secondary_link", E.rgba 255 251 244 1 )
                            , ( "tertiary_link", E.rgba 255 251 244 1 )
                            , ( "street", E.rgba 255 251 244 1 )
                            , ( "street_limited", E.rgba 255 251 244 1 )
                            , ( "service", E.rgba 255 251 244 1 )
                            , ( "track", E.rgba 255 251 244 1 )
                            ]
                            (E.rgba 255 255 255 1)
                    )
                , Layer.lineJoin E.rounded
                , Layer.lineCap E.rounded
                ]
            , Layer.line "admin-state-province"
                "composite"
                [ Layer.minzoom 2
                , Layer.filter
                    (E.all
                        [ E.getProperty (str "admin_level") |> E.isEqual (float 1)
                        , E.getProperty (str "disputed") |> E.isEqual (str "false")
                        , E.getProperty (str "maritime") |> E.isEqual (str "false")
                        , E.getProperty (str "worldview") |> E.matchesStr [ ( "US", true ), ( "all", true ) ] false
                        ]
                    )
                , Layer.sourceLayer "admin"
                , Layer.lineDasharray (E.zoom |> E.step (E.floats [ 2, 0 ]) [ ( 7, E.floats [ 2, 2, 6, 2 ] ) ])
                , Layer.lineWidth (E.zoom |> E.interpolate E.Linear [ ( 7, float 0.75 ), ( 12, float 1.5 ) ])
                , Layer.lineOpacity (E.zoom |> E.interpolate E.Linear [ ( 2, float 0 ), ( 3, float 1 ) ])
                , Layer.lineColor (E.zoom |> E.step (E.rgba 204 204 204 1) [ ( 4, E.rgba 165 165 165 1 ) ])
                , Layer.lineJoin E.rounded
                , Layer.lineCap E.rounded
                ]
            , Layer.line "admin-country"
                "composite"
                [ Layer.minzoom 1
                , Layer.filter
                    (E.all
                        [ E.getProperty (str "admin_level") |> E.isEqual (float 0)
                        , E.getProperty (str "disputed") |> E.isEqual (str "false")
                        , E.getProperty (str "maritime") |> E.isEqual (str "false")
                        , E.getProperty (str "worldview") |> E.matchesStr [ ( "US", true ), ( "all", true ) ] false
                        ]
                    )
                , Layer.sourceLayer "admin"
                , Layer.lineColor (E.rgba 127 127 127 1)
                , Layer.lineWidth (E.zoom |> E.interpolate E.Linear [ ( 3, float 0.5 ), ( 10, float 2 ) ])
                , Layer.lineJoin E.rounded
                , Layer.lineCap E.rounded
                ]
            , Layer.line "admin-country-disputed"
                "composite"
                [ Layer.minzoom 1
                , Layer.filter
                    (E.all
                        [ E.getProperty (str "admin_level") |> E.isEqual (float 0)
                        , E.getProperty (str "disputed") |> E.isEqual (str "true")
                        , E.getProperty (str "maritime") |> E.isEqual (str "false")
                        , E.getProperty (str "worldview") |> E.matchesStr [ ( "US", true ), ( "all", true ) ] false
                        ]
                    )
                , Layer.sourceLayer "admin"
                , Layer.lineColor (E.rgba 127 127 127 1)
                , Layer.lineWidth (E.zoom |> E.interpolate E.Linear [ ( 3, float 0.5 ), ( 10, float 2 ) ])
                , Layer.lineDasharray (E.floats [ 1.5, 1.5 ])
                , Layer.lineJoin E.rounded
                ]
            , Layer.symbol "road-label"
                "composite"
                [ Layer.minzoom 12
                , Layer.filter
                    (E.getProperty (str "class")
                        |> E.matchesStr
                            [ ( "motorway", true )
                            , ( "trunk", true )
                            , ( "primary", true )
                            , ( "secondary", true )
                            , ( "tertiary", true )
                            , ( "street", true )
                            , ( "street_limited", true )
                            , ( "pedestrian", true )
                            ]
                            false
                    )
                , Layer.sourceLayer "road"
                , Layer.textColor (E.rgba 0 0 0 1)
                , Layer.textHaloColor (E.rgba 255 255 255 0.95)
                , Layer.textHaloWidth (float 1)
                , Layer.textField (E.coalesce [ E.getProperty (str "name_en"), E.getProperty (str "name") ])
                , Layer.textSize
                    (E.zoom
                        |> E.interpolate E.Linear
                            [ ( 9
                              , E.getProperty (str "class")
                                    |> E.matchesStr
                                        [ ( "motorway", float 10 )
                                        , ( "trunk", float 10 )
                                        , ( "primary", float 10 )
                                        , ( "secondary", float 10 )
                                        , ( "tertiary", float 10 )
                                        ]
                                        (float 9)
                              )
                            , ( 20
                              , E.getProperty (str "class")
                                    |> E.matchesStr
                                        [ ( "motorway", float 15 )
                                        , ( "trunk", float 15 )
                                        , ( "primary", float 15 )
                                        , ( "secondary", float 15 )
                                        , ( "tertiary", float 15 )
                                        ]
                                        (float 14)
                              )
                            ]
                    )
                , Layer.textMaxAngle (float 30)
                , Layer.textFont (E.strings [ "Roboto Regular", "Arial Unicode MS Regular" ])
                , Layer.symbolPlacement E.line
                , Layer.textPadding (float 1)
                , Layer.textRotationAlignment E.map
                , Layer.textPitchAlignment E.viewport
                ]
            , Layer.symbol "poi-label"
                "composite"
                [ Layer.minzoom 6
                , Layer.filter (E.getProperty (str "filterrank") |> E.lessThanOrEqual (float 3))
                , Layer.sourceLayer "poi_label"
                , Layer.textColor (E.rgba 88 77 59 1)
                , Layer.textHaloColor (E.rgba 255 255 255 0.75)
                , Layer.textHaloWidth (float 1)
                , Layer.textHaloBlur (float 0.5)
                , Layer.textLineHeight (float 1.1)
                , Layer.textSize (E.zoom |> E.interpolate E.Linear [ ( 10, float 11 ), ( 18, float 13 ) ])
                , Layer.iconImage (E.getProperty (str "maki") |> E.append (str "-11"))
                , Layer.textMaxAngle (float 38)
                , Layer.textFont (E.strings [ "Roboto Regular", "Arial Unicode MS Regular" ])
                , Layer.textOffset (E.floats [ 0, 0.75 ])
                , Layer.textAnchor E.top
                , Layer.textField (E.coalesce [ E.getProperty (str "name_en"), E.getProperty (str "name") ])
                , Layer.textMaxWidth (float 8)
                ]
            , Layer.symbol "airport-label"
                "composite"
                [ Layer.minzoom 8
                , Layer.sourceLayer "airport_label"
                , Layer.textColor (E.rgba 88 77 59 1)
                , Layer.textHaloColor (E.rgba 255 255 255 0.95)
                , Layer.textHaloWidth (float 1)
                , Layer.textLineHeight (float 1.1)
                , Layer.textSize (E.zoom |> E.interpolate E.Linear [ ( 10, float 12 ), ( 18, float 18 ) ])
                , Layer.iconImage
                    (E.zoom
                        |> E.step (E.getProperty (str "maki") |> E.append (str "-11")) [ ( 13, E.getProperty (str "maki") |> E.append (str "-15") ) ]
                    )
                , Layer.textFont (E.strings [ "Roboto Regular", "Arial Unicode MS Regular" ])
                , Layer.textOffset (E.floats [ 0, 0.75 ])
                , Layer.textAnchor E.top
                , Layer.textField
                    (E.zoom
                        |> E.step (E.getProperty (str "ref")) [ ( 14, E.coalesce [ E.getProperty (str "name_en"), E.getProperty (str "name") ] ) ]
                    )
                , Layer.textMaxWidth (float 9)
                ]
            , Layer.symbol "place-neighborhood-suburb-label"
                "composite"
                [ Layer.minzoom 11
                , Layer.maxzoom 15
                , Layer.filter
                    (E.all
                        [ E.getProperty (str "filterrank") |> E.lessThanOrEqual (float 3)
                        , E.getProperty (str "type")
                            |> E.matchesStr [ ( "suburb", true ), ( "quarter", true ), ( "neighbourhood", true ) ] false
                        ]
                    )
                , Layer.sourceLayer "place_label"
                , Layer.textHaloColor (E.rgba 255 255 255 0.95)
                , Layer.textHaloWidth (float 1)
                , Layer.textColor (E.rgba 86 62 20 1)
                , Layer.textField (E.coalesce [ E.getProperty (str "name_en"), E.getProperty (str "name") ])
                , Layer.textTransform E.uppercase
                , Layer.textLetterSpacing (float 0.15)
                , Layer.textMaxWidth (float 8)
                , Layer.textFont (E.strings [ "Roboto Regular", "Arial Unicode MS Regular" ])
                , Layer.textPadding (float 3)
                , Layer.textSize (E.zoom |> E.interpolate E.Linear [ ( 12, float 11 ), ( 16, float 16 ) ])
                ]
            , Layer.symbol "place-town-village-hamlet-label"
                "composite"
                [ Layer.minzoom 6
                , Layer.maxzoom 14
                , Layer.filter
                    (E.all
                        [ E.getProperty (str "type")
                            |> E.matchesStr [ ( "town", true ), ( "village", true ), ( "hamlet", true ) ] false
                        , E.getProperty (str "filterrank") |> E.lessThanOrEqual (float 3)
                        ]
                    )
                , Layer.sourceLayer "place_label"
                , Layer.textColor (E.rgba 0 0 0 1)
                , Layer.textHaloBlur (float 0.5)
                , Layer.textHaloColor (E.rgba 255 255 255 0.95)
                , Layer.textHaloWidth (float 1)
                , Layer.textSize
                    (E.zoom
                        |> E.interpolate E.Linear
                            [ ( 5, E.getProperty (str "type") |> E.matchesStr [ ( "town", float 9.5 ) ] (float 8) )
                            , ( 16, E.getProperty (str "type") |> E.matchesStr [ ( "town", float 20 ) ] (float 16) )
                            ]
                    )
                , Layer.textFont
                    (E.zoom
                        |> E.step (E.strings [ "Roboto Regular", "Arial Unicode MS Regular" ])
                            [ ( 12
                              , E.getProperty (str "type")
                                    |> E.matchesStr [ ( "town", E.strings [ "Roboto Medium", "Arial Unicode MS Regular" ] ) ] (E.strings [ "Roboto Regular", "Arial Unicode MS Regular" ])
                              )
                            ]
                    )
                , Layer.textMaxWidth (float 7)
                , Layer.textField (E.coalesce [ E.getProperty (str "name_en"), E.getProperty (str "name") ])
                ]
            , Layer.symbol "place-city-label"
                "composite"
                [ Layer.minzoom 3
                , Layer.maxzoom 14
                , Layer.filter
                    (E.all
                        [ E.getProperty (str "filterrank") |> E.lessThanOrEqual (float 3)
                        , E.getProperty (str "type") |> E.isEqual (str "city")
                        ]
                    )
                , Layer.sourceLayer "place_label"
                , Layer.textColor (E.zoom |> E.interpolate E.Linear [ ( 5, E.rgba 84 84 84 1 ), ( 6, E.rgba 0 0 0 1 ) ])
                , Layer.textHaloBlur (float 0.5)
                , Layer.textHaloColor (E.rgba 255 255 255 0.95)
                , Layer.textHaloWidth (float 1.25)
                , Layer.textSize
                    (E.zoom
                        |> E.interpolate E.Linear
                            [ ( 3
                              , E.getProperty (str "symbolrank")
                                    |> E.step (float 12) [ ( 9, float 11 ), ( 12, float 10 ), ( 14, float 6.5 ) ]
                              )
                            , ( 14
                              , E.getProperty (str "symbolrank")
                                    |> E.step (float 27) [ ( 9, float 23 ), ( 10, float 21 ), ( 12, float 20 ) ]
                              )
                            ]
                    )
                , Layer.textFont
                    (E.zoom
                        |> E.step (E.strings [ "Roboto Medium", "Arial Unicode MS Regular" ])
                            [ ( 10
                              , E.getProperty (str "symbolrank")
                                    |> E.step (E.strings [ "Roboto Bold", "Arial Unicode MS Bold" ]) [ ( 5, E.strings [ "Roboto Medium", "Arial Unicode MS Regular" ] ) ]
                              )
                            ]
                    )
                , Layer.textField (E.coalesce [ E.getProperty (str "name_en"), E.getProperty (str "name") ])
                ]
            , Layer.symbol "state-label"
                "composite"
                [ Layer.minzoom 4
                , Layer.maxzoom 8
                , Layer.filter (E.getProperty (str "type") |> E.isEqual (str "state"))
                , Layer.sourceLayer "place_label"
                , Layer.textColor (E.rgba 169 164 156 1)
                , Layer.textHaloWidth (float 1)
                , Layer.textHaloColor (E.rgba 255 255 255 0.95)
                , Layer.textSize (E.zoom |> E.interpolate E.Linear [ ( 4, float 9.5 ), ( 9, float 18 ) ])
                , Layer.textTransform E.uppercase
                , Layer.textFont (E.strings [ "Roboto Black", "Arial Unicode MS Bold" ])
                , Layer.textPadding (float 1)
                , Layer.textField
                    (E.zoom
                        |> E.step (E.getProperty (str "abbr")) [ ( 5, E.coalesce [ E.getProperty (str "name_en"), E.getProperty (str "name") ] ) ]
                    )
                , Layer.textLetterSpacing (float 0.2)
                , Layer.textMaxWidth (float 6)
                ]
            , Layer.symbol "country-label"
                "composite"
                [ Layer.minzoom 3
                , Layer.maxzoom 8
                , Layer.filter (E.getProperty (str "type") |> E.isEqual (str "country"))
                , Layer.sourceLayer "place_label"
                , Layer.textHaloWidth (float 1.5)
                , Layer.textHaloColor (E.rgba 255 255 255 0.95)
                , Layer.textColor (E.rgba 0 0 0 1)
                , Layer.textField (E.coalesce [ E.getProperty (str "name_en"), E.getProperty (str "name") ])
                , Layer.textMaxWidth (E.zoom |> E.interpolate E.Linear [ ( 0, float 5 ), ( 3, float 6 ) ])
                , Layer.textFont
                    (E.zoom
                        |> E.step (E.strings [ "Roboto Medium", "Arial Unicode MS Regular" ]) [ ( 4, E.strings [ "Roboto Bold", "Arial Unicode MS Bold" ] ) ]
                    )
                , Layer.textSize
                    (E.zoom
                        |> E.interpolate E.Linear
                            [ ( 1, E.getProperty (str "symbolrank") |> E.step (float 12) [ ( 3, float 10 ), ( 5, float 9 ) ] )
                            , ( 9, E.getProperty (str "symbolrank") |> E.step (float 35) [ ( 3, float 27 ), ( 5, float 22 ) ] )
                            ]
                    )
                ]
            ]
        , sources = [ Source.vectorFromUrl "composite" "mapbox://mapbox.mapbox-streets-v8" ]
        , misc =
            [ Style.sprite "mapbox://sprites/spottiyer/ck05sz4xx1qmn1co21xc9c1e9/629bpdwpqxu5j66f3aoku8h84"
            , Style.glyphs "mapbox://fonts/spottiyer/{fontstack}/{range}.pbf"
            , Style.name "Basic Template"
            , Style.defaultZoomLevel 3.25
            , Style.defaultBearing 0
            , Style.defaultPitch 0
            , Style.defaultCenter (LngLat -95 41)
            ]
        }
