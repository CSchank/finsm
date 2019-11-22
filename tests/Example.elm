module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Machine exposing (test)

import Json.Encode as E
import Json.Decode as D


suite : Test
suite =
    describe "Machine encoder-decoder"
        [ Test.test "Self-cancellation of encoding and decoding for V1" <|
                     \_ -> Expect.equal (Ok Machine.test) (D.decodeString Machine.machineDecoderV1 <| E.encode 0 (Machine.machineEncoderV1 Machine.test))
        ]
