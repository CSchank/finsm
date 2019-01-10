module Helpers exposing (add, dot, editIcon, mult, p, sub, vertex)

import GraphicSVG exposing (..)


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
