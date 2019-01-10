module Environment exposing(..)

init : Environment
init = 
        { windowSize = ( 0, 0 )
        , holdingShift = False
        }

type alias Environment =
    { windowSize : ( Int, Int )
    , holdingShift : Bool
    }