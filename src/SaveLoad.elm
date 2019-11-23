module SaveLoad exposing (..)

import Http
import Json.Decode as D
import Json.Encode as E
import Machine exposing (Machine)
import Time exposing (Posix)


type alias SaveMetadata =
    { id : Int
    , name : String
    , date : Posix
    , description : String
    }


decodeMetadataV1 : D.Decoder SaveMetadata
decodeMetadataV1 =
    D.map4 SaveMetadata
        (D.field "id" D.int)
        (D.field "name" D.string)
        (D.field "date" <| D.map Time.millisToPosix D.int)
        (D.field "desc" D.string)


decodeMetadata : D.Decoder SaveMetadata
decodeMetadata =
    D.field "v" D.int
        |> D.andThen
            (\v ->
                case v of
                    1 ->
                        decodeMetadataV1

                    _ ->
                        D.fail <| "Invalid save metadata version " ++ String.fromInt v
            )


decodeMachineList : D.Decoder (List SaveMetadata)
decodeMachineList =
    D.list decodeMetadata


encodeMachinePayload =
    encodeMachinePayloadV1


encodeMachinePayloadV1 : String -> String -> Posix -> Machine -> E.Value
encodeMachinePayloadV1 name desc time machine =
    E.object
        [ ( "name", E.string name )
        , ( "desc", E.string desc )
        , ( "time", E.int <| Time.posixToMillis time )
        , ( "machine", Machine.machineEncoder machine )
        , ( "v", E.int 1 )
        ]


saveMachine : String -> String -> Posix -> Machine -> (Result Http.Error Bool -> msg) -> Cmd msg
saveMachine name desc time machine toMsg =
    Http.send toMsg <|
        Http.post
            "https://finsm.io/api/machine/save"
            (Http.jsonBody <| encodeMachinePayload name desc time machine)
            D.bool


archiveMachine : Int -> (Result Http.Error Bool -> msg) -> Cmd msg
archiveMachine id toMsg =
    Http.send toMsg <|
        Http.post
            "https://finsm.io/api/machine/archive"
            (Http.jsonBody <| E.int id)
            D.bool


loadMachine : String -> String -> Posix -> Int -> (Result Http.Error Machine -> msg) -> Cmd msg
loadMachine name desc time id toMsg =
    Http.send toMsg <|
        Http.post
            "https://finsm.io/api/machine/load"
            (Http.jsonBody <| E.int id)
            Machine.machineDecoder


loadList : (Result Http.Error (List SaveMetadata) -> msg) -> Cmd msg
loadList toMsg =
    Http.send toMsg <|
        Http.post
            "https://finsm.io/api/machine/list"
            Http.emptyBody
            decodeMachineList
