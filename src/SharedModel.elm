module SharedModel exposing (MachineType(..), SharedModel, init)

import Machine exposing (Machine)


type MachineType
    = DFA
    | NFA


type alias SharedModel =
    { machine : Machine
    , machineType : MachineType
    }


init : SharedModel
init =
    { machine = Machine.test
    , machineType = DFA
    }
