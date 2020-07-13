module ApplicationModel exposing (..)

import Building
import Exporting
import SharedModel exposing (SharedModel)
import Simulating


type ApplicationState
    = Building Building.Model
    | Simulating Simulating.Model
    | Exporting Exporting.Model


type alias ApplicationModel a =
    { appState : ApplicationState
    , simulatingData : Simulating.PersistentModel
    , buildingData : Building.PersistentModel
    , exportingData : Exporting.PersistentModel
    , sharedModel : SharedModel a
    }
