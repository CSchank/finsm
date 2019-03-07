module Environment exposing (DevMode(..), Environment, init, initDev)


type DevMode
    = Deploy
    | Development


init : Environment
init =
    { windowSize = ( 0, 0 )
    , holdingShift = False
    , holdingControl = False
    , holdingMeta = False
    , devMode = Deploy
    }


initDev : Environment
initDev =
    { windowSize = ( 0, 0 )
    , holdingShift = False
    , holdingControl = False
    , holdingMeta = False
    , devMode = Development
    }


type alias Environment =
    { windowSize : ( Int, Int )
    , holdingShift : Bool
    , holdingControl : Bool
    , holdingMeta : Bool
    , devMode : DevMode
    }
