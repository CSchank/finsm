module SharedModel exposing (..)

import Machine exposing (Machine)


type alias SharedModel =
    { machine : Machine
    }


init : SharedModel
init =
    { machine = Machine.test
    }
