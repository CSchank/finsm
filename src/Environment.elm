module Environment exposing (Environment, init)

import Time


init : Environment
init =
    { windowSize = ( 0, 0 )
    , holdingShift = False
    , holdingControl = False
    , holdingMeta = False
    , currentTime = Time.millisToPosix 1576798602274
    , timeZone = Time.utc
    , mousePos = ( 0, 0 )
    }


type alias Environment =
    { windowSize : ( Int, Int )
    , holdingShift : Bool
    , holdingControl : Bool
    , holdingMeta : Bool
    , currentTime : Time.Posix
    , timeZone : Time.Zone
    , mousePos : ( Float, Float )
    }
