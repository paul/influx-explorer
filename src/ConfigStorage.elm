module ConfigStorage exposing (Message, Config, initialConfig, subscriptions, saveConfig, update)

import Json.Encode as JSONE
import Json.Decode as JSOND exposing (field)
import Json.Decode.Extra exposing ((|:))
import LocalStorage exposing (localStorageGet, localStorageSet, localStorageClear, localStorageResponse)

type alias Config =
  { url : String
  , username : String
  , password : String }

type Message
  = LoadConfig JSOND.Value

initialConfig : Config
initialConfig =
  { url = ""
  , username = ""
  , password = ""
  }

update : Message -> Config -> (Config, Cmd Message)
update message config =
  case message of
    LoadConfig data ->
      let
        config = JSOND.decodeValue decodeConfig data
      in
        case config of
          Ok config ->
            (config, Cmd.none)
          Err _ ->
            (initialConfig, Cmd.none)



saveConfig : Config -> Cmd message
saveConfig config =
  localStorageSet ("config", encodeConfig config)

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

subscriptions : Config -> Sub Message
subscriptions config =
  localStorageResponse LoadConfig
