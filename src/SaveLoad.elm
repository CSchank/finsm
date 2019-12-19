module SaveLoad exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as D
import Json.Encode as E
import Machine exposing (Machine)
import Simulating exposing (InputTape)
import Time exposing (Posix)


type alias LoadMetadata =
    { id : String
    , name : String
    , date : Posix
    , description : String
    }


decodeMetadataV1 : D.Decoder LoadMetadata
decodeMetadataV1 =
    D.map4 LoadMetadata
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "date" <| D.map Time.millisToPosix D.int)
        (D.field "desc" D.string)


decodeMetadata : D.Decoder LoadMetadata
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


decodeMachineList : D.Decoder (List LoadMetadata)
decodeMachineList =
    D.list decodeMetadata


encodeMachinePayload =
    encodeMachinePayloadV1



-- encode the payload when saving a machine to the server
-- note: id is empty if the machine is a new one instead of one already saved to the server
-- sending an existing id will overwrite the machine saved with that id


encodeMachinePayloadV1 : String -> String -> Machine -> String -> Dict Int ( InputTape, a ) -> E.Value
encodeMachinePayloadV1 name desc machine uuid inputTape =
    E.object
        [ ( "name", E.string name )
        , ( "desc", E.string desc )
        , ( "machine", Machine.machineEncoder machine )
        , ( "v", E.int 1 )
        , ( "uuid", E.string uuid )
        , ( "tape", Simulating.inputTapeEncoder inputTape )
        ]


type alias SaveResponse =
    { success : Bool
    , uuid : String
    }


decodeSaveResponse : D.Decoder SaveResponse
decodeSaveResponse =
    D.map2 SaveResponse
        (D.field "success" D.bool)
        (D.field "uuid" D.string)


saveMachine : String -> String -> Machine -> String -> Dict Int ( InputTape, a ) -> (Result Http.Error SaveResponse -> msg) -> Cmd msg
saveMachine name desc machine uuid inputTape toMsg =
    Http.send toMsg <|
        Http.post
            "/api/machine/save"
            (Http.jsonBody <| encodeMachinePayload name desc machine uuid inputTape)
            decodeSaveResponse


archiveMachine : Int -> (Result Http.Error Bool -> msg) -> Cmd msg
archiveMachine id toMsg =
    Http.send toMsg <|
        Http.post
            "/api/machine/archive"
            (Http.jsonBody <| E.int id)
            D.bool


type alias LoadPayload =
    { machine : Machine
    , tape : InputTape
    }


decodeLoadPayload : D.Decoder LoadPayload
decodeLoadPayload =
    D.map2 LoadPayload
        (D.field "machine" Machine.machineDecoder)
        (D.field "tape" Simulating.inputTapeDecoder)


loadMachine : String -> String -> Posix -> String -> (Result Http.Error Machine -> msg) -> Cmd msg
loadMachine name desc time uuid toMsg =
    Http.send toMsg <|
        Http.post
            "/api/machine/load"
            (Http.jsonBody <| E.string uuid)
            Machine.machineDecoder


loadList : (Result Http.Error (List LoadMetadata) -> msg) -> Cmd msg
loadList toMsg =
    Http.send toMsg <|
        Http.post
            "/api/machine/list"
            Http.emptyBody
            decodeMachineList
