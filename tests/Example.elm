module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as D
import Json.Encode as E
import Machine exposing (test)
import Test exposing (..)


suite : Test
suite =
    describe "Machine encoder-decoder"
        [ Test.test "Self-cancellation of encoding and decoding for V1" <|
            \_ ->
                Expect.equal (Ok Machine.test)
                    (D.decodeString Machine.machineDecoder <|
                        E.encode 0 (Machine.machineEncoder Machine.test)
                    )
        ]
