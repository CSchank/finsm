module Helpers exposing (LatexAlign(..), add, dot, editIcon, latex, latexurl, mult, p, setMax, sub, trashIcon, vertex)

import GraphicSVG exposing (..)
import Html as H exposing (Html, input, node)
import Html.Attributes exposing (attribute, placeholder, style, value)
import Html.Events exposing (onInput)
import Set exposing (Set)
import Url exposing (Url, percentEncode)


vertex ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) =
    let
        p0 =
            ( x0, y0 )

        p1 =
            ( x1, y1 )

        p2 =
            ( x2, y2 )

        p3 =
            add p0 p2

        t =
            dot (sub p0 p1) (sub p3 (mult p1 2)) / (dot p3 p3 - 4 * dot p1 (sub p3 p1))
    in
    p p0 p1 p2 t


p p0 p1 p2 t =
    add (mult p0 ((1 - t) ^ 2)) (add (mult (mult (mult p1 t) (1 - t)) 2) (mult p2 (t ^ 2)))


add ( x0, y0 ) ( x1, y1 ) =
    ( x0 + x1, y0 + y1 )


mult ( x, y ) s =
    ( x * s, y * s )


sub ( x0, y0 ) ( x1, y1 ) =
    ( x0 - x1, y0 - y1 )


dot ( x0, y0 ) ( x1, y1 ) =
    x0 * x1 + y0 * y1


editIcon =
    group
        [ --square 5 |> outlined (solid 1) black
          rect 5 2
            |> filled (rgb 21 137 255)
            |> rotate (degrees 45)
            |> move ( 3, 3 )
        , triangle 1
            |> filled blue
            |> rotate (degrees -15)
        ]


trashIcon =
    group
        [ roundedRect 30 40 3
            |> outlined (solid 4) black
        , rect 42 5 |> filled black |> move ( 0, 19.5 )
        , roundedRect 36 5 1 |> filled black |> move ( 0, 21.5 )
        , roundedRect 10 10 1 |> outlined (solid 3) black |> move ( 0, 23.5 )
        , rect 4 30 |> filled black
        , rect 4 30 |> filled black |> move ( -8, 0 )
        , rect 4 30 |> filled black |> move ( 8, 0 )
        ]


type LatexAlign
    = AlignLeft
    | AlignRight
    | AlignCentre


latex w h txt align =
    (html w h <|
        H.div
            [ style "width" "100%"
            , style "height" "100%"
            , style "-moz-user-select" "none"
            , style "-webkit-user-select" "none"
            , style "-user-select" "none"
            ]
            [ H.img
                ([ Html.Attributes.attribute "onerror" ("this.src='" ++ latexurl "\\LaTeX?" ++ "'")
                 , Html.Attributes.src (latexurl txt)

                 --, style "width" "100%"
                 , style "height" "100%"
                 ]
                    ++ (case align of
                            AlignCentre ->
                                [ style "margin-left" "auto"
                                , style "margin-right" "auto"
                                ]

                            AlignLeft ->
                                [ style "margin-right" "auto"
                                ]

                            AlignRight ->
                                [ style "margin-left" "auto"
                                ]
                       )
                    ++ [ style "display" "block"
                       , style "max-width" "100%"
                       ]
                )
                []
            ]
    )
        |> move ( -w / 2, 0 )


latexurl : String -> String
latexurl lx =
    "https://finsm.io/latex/render/" ++ percentEncode lx


setMax : Set Int -> Int
setMax s =
    Set.foldl max 0 s
