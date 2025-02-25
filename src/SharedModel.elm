module SharedModel exposing (SharedModel, init, machineModeButtons)

import GraphicSVG exposing (..)
import Helpers exposing (..)
import Machine exposing (Machine, MachineType(..))


type alias SharedModel =
    { machine : Machine
    , machineType : MachineType
    }


init : SharedModel
init =
    { machine = Machine.test
    , machineType = DFA
    }


machineModeButtons : MachineType -> Float -> Float -> (MachineType -> msg) -> Shape msg
machineModeButtons mtype winX winY changeMsg =
    group
        [ group
            [ roundedRect 30 15 1
                |> filled
                    (if mtype == DFA then
                        finsmLightBlue

                     else
                        blank
                    )
                |> addOutline (solid 1) darkGray
            , text "DFA"
                |> centered
                |> fixedwidth
                |> filled
                    (if mtype == DFA then
                        white

                     else
                        darkGray
                    )
                |> move ( 0, -4 )
            ]
            |> move ( -winX / 2 + 20, winY / 2 - 32 )
            |> notifyTap (changeMsg DFA)
        , group
            [ roundedRect 30 15 1
                |> filled
                    (if mtype == NFA then
                        finsmLightBlue

                     else
                        blank
                    )
                |> addOutline (solid 1) darkGray
            , text "NFA"
                |> centered
                |> fixedwidth
                |> filled
                    (if mtype == NFA then
                        white

                     else
                        darkGray
                    )
                |> move ( 0, -4 )
            ]
            |> move ( -winX / 2 + 52, winY / 2 - 32 )
            |> notifyTap (changeMsg NFA)
        ]
