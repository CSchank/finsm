port module Ports exposing (..)


port launchLogin : () -> Cmd msg


port launchLogout : () -> Cmd msg


port loginComplete : (() -> msg) -> Sub msg


port logoutComplete : (() -> msg) -> Sub msg
