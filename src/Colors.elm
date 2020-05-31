-- module Colors: defines all custom colors


module Colors exposing (..)

import Element exposing (..)
import Mapbox.Expression as Expr


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
