module Examples exposing (suite)

import Convert exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "An easy test"
        [ test "Example test" <|
            \_ -> Expect.equal 3 3
        ]
