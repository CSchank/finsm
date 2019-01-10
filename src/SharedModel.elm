module SharedModel exposing (..)
import Machine exposing(Machine)

init : SharedModel
init = 
    {
        machine = Machine.test
    }

type alias SharedModel =
    {
        machine : Machine
    }