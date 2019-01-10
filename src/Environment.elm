module Environment exposing (Environment, init)


init : Environment
init =
    { windowSize = ( 0, 0 )
    , holdingShift = False
    }


type alias Environment =
    { windowSize : ( Int, Int )
    , holdingShift : Bool
    }
