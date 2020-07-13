module SharedModel exposing (MachineFAType(..), SharedModel, init)

import MachineFA exposing (..)

type MachineFAType = DFA | NFA

type alias SharedModel a  =
    { machine : a
    , machineFAType : MachineFAType
    }


init : SharedModel MachineFA
init =
    { machine = MachineFA.test
    , machineFAType = DFA
    }
