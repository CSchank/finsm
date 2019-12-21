module Environment exposing (Environment, init)

import Time


init : Environment
init =
    { windowSize = ( 0, 0 )
    , holdingShift = False
    , holdingControl = False
    , holdingMeta = False
    , currentTime = Time.millisToPosix 1576798602274
    }


type alias Environment =
    { windowSize : ( Int, Int )
    , holdingShift : Bool
    , holdingControl : Bool
    , holdingMeta : Bool
    , currentTime : Time.Posix
    }
