module Utils exposing (..)

import Dict exposing (Dict)
import GraphicSVG exposing (..)
import Html exposing (input)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Task


encodePair : (a -> E.Value) -> (b -> E.Value) -> ( a, b ) -> E.Value
encodePair encA encB ( a, b ) =
    E.object [ ( "f", encA a ), ( "s", encB b ) ]


encodeTriple : (a -> E.Value) -> (b -> E.Value) -> (c -> E.Value) -> ( a, b, c ) -> E.Value
encodeTriple encA encB encC ( a, b, c ) =
    E.object [ ( "f", encA a ), ( "s", encB b ), ( "t", encC c ) ]


decodeDict : D.Decoder comparable -> D.Decoder value -> D.Decoder (Dict comparable value)
decodeDict decComp decValu =
    D.map Dict.fromList <| D.list <| D.map2 Tuple.pair (D.field "k" decComp) (D.field "v" decValu)


decodeSet : D.Decoder comparable -> D.Decoder (Set comparable)
decodeSet decComp =
    D.map Set.fromList <| D.list decComp


decodePair : D.Decoder x -> D.Decoder y -> D.Decoder ( x, y )
decodePair decX decY =
    D.map2 Tuple.pair (D.field "f" decX) (D.field "s" decY)


decodeTriple : D.Decoder x -> D.Decoder y -> D.Decoder z -> D.Decoder ( x, y, z )
decodeTriple decX decY decZ =
    D.map3 (\x y z -> ( x, y, z )) (D.field "f" decX) (D.field "s" decY) (D.field "t" decZ)


encodeSet : (comparable -> E.Value) -> Set comparable -> E.Value
encodeSet valFn =
    E.list valFn << Set.toList


encodeDict : (comparable -> E.Value) -> (value -> E.Value) -> Dict comparable value -> E.Value
encodeDict compFn valFn dict =
    E.list
        (\( k, v ) ->
            E.object
                [ ( "k", compFn k )
                , ( "v", valFn v )
                ]
        )
    <|
        Dict.toList dict


textBox : String -> Float -> Float -> String -> (String -> msg) -> Shape msg
textBox txt w h place msg =
    move ( -w / 2, h / 2 ) <|
        html (w * 1.5) (h * 1.5) <|
            input
                [ id <| "input" ++ place
                , placeholder place
                , onInput msg
                , value txt
                , style "width" (String.fromFloat w ++ "px")
                , style "height" (String.fromFloat h ++ "px")
                , style "margin-top" "1px"
                , style "font-family" "monospace"
                ]
                []


textBox3 : ( String, String, String ) -> Float -> Float -> ( String, String, String ) -> (String -> msg) -> (String -> msg) -> (String -> msg) -> Shape msg
textBox3 ( t1, t2, t3 ) w h ( p1, p2, p3 ) msg1 msg2 msg3 =
    let
        box1 =
            textBox t1 w h p1 msg1 |> move ( 0, h )

        box2 =
            textBox t2 w h p2 msg2

        box3 =
            textBox t3 w h p3 msg3 |> move ( 0, -h )
    in
    group [ box1, box2, box3 ]


newMsg : msg -> Cmd msg
newMsg msg =
    Task.perform identity <| Task.succeed msg
