port module ConfigStorage exposing (..)

import Json.Encode as JSONE
import Json.Decode as JSOND exposing (field)
import Json.Decode.Extra exposing ((|:))

type alias Key =
  String

type alias Config =
  { url : String
  , username : String
  , password : String }

port localStorageSet : (Key, JSONE.Value) -> Cmd message
port localStorageGet : Key -> Cmd message
port localStorageClear : String -> Cmd message
port localStorageResponse : (JSOND.Value -> message) -> Sub message

encodeConfig : Config -> JSONE.Value
encodeConfig config =
  JSONE.object
    [ ("url", JSONE.string config.url),
      ("username", JSONE.string config.username),
      ("password", JSONE.string config.password)
    ]

decodeConfig : JSOND.Decoder Config
decodeConfig =
  JSOND.succeed Config
    |: (field "url" JSOND.string)
    |: (field "username" JSOND.string)
    |: (field "password" JSOND.string)
