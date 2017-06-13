port module ConfigStorage exposing (..)

import Json.Encode as JSONE
import Json.Decode as JSOND

type alias Key =
  String

port localStorageSet : (Key, JSONE.Value) -> Cmd message
port localStorageGet : Key -> Cmd message
port localStorageClear : String -> Cmd message
port localStorageResponse : (JSOND.Value -> message) -> Sub message

