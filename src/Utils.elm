module Utils exposing (..)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)


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
