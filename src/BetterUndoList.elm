module BetterUndoList exposing (BetterUndoList, fresh, new, redo, replace, undo, UndoAction(..))

import UndoList as U


type alias BetterUndoList state =
    { ul : U.UndoList state
    , present : state
    }


type UndoAction = NoUndo | UndoRequired

fresh : state -> BetterUndoList state
fresh state =
    { present = state
    , ul = U.fresh state
    }


new : state -> BetterUndoList state -> BetterUndoList state
new state nUL =
    { nUL
        | present = state
        , ul = U.new state nUL.ul
    }


replace : state -> BetterUndoList state -> BetterUndoList state
replace state nUL =
    { nUL
        | present = state
    }


undo : BetterUndoList state -> BetterUndoList state
undo nUL =
    let
        newUL =
            U.undo nUL.ul
    in
    { present = newUL.present
    , ul = U.undo nUL.ul
    }


redo : BetterUndoList state -> BetterUndoList state
redo nUL =
    let
        newUL =
            U.redo nUL.ul
    in
    { present = newUL.present
    , ul = newUL
    }
