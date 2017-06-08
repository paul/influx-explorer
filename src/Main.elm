import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (field)
import Json.Decode.Extra exposing ((|:))

import BasicAuth



main =
    Html.program
        { init = init "SHOW MEASUREMENTS"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { query : String
    , data : Maybe (InfluxResult)
    , config : Config}

type alias Config =
  { url : String
  , username : String
  , password : String }

init : String -> (Model, Cmd Msg)
init query =
  ( Model query Nothing (Config "https://influxdb.textus-staging.net" "textus" "42ajtsOVahJeFWDrp")
  , Cmd.none
  )

type alias InfluxResult =
  { series : List Series }

type alias Series =
  { name : String
  , columns : Columns
  , values : Rows
  }

type alias Columns =
  List Column

type alias Column =
  String

type alias Rows =
  List Row

type alias Row =
  List Value

type alias Value =
  String


-- UPDATE


type Msg
    = LoadJson
    | LoadedJson (Result Http.Error (InfluxResult))
    | Url String
    | Username String
    | Password String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadJson ->
          ( model, getData model )

        LoadedJson (Err _) ->
          (model, Cmd.none)

        LoadedJson (Ok newData) ->
          ({ model | data = Just newData }, Cmd.none)

        Url newUrl ->
          let
              oldConfig = model.config
              newConfig = {oldConfig | url = newUrl}
          in
              ({ model | config = newConfig }, Cmd.none)

        Username newUsername ->
          let
              oldConfig = model.config
              newConfig = {oldConfig | username = newUsername}
          in
              ({ model | config = newConfig }, Cmd.none)

        Password newPassword ->
          let
              oldConfig = model.config
              newConfig = {oldConfig | password = newPassword}
          in
              ({ model | config = newConfig }, Cmd.none)




-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Hello, world!" ]
        , h2 [] [ text "Config" ]
        , div [] [
          label [] [
            text "Url"
            , input [ type_ "text", onInput Url, value model.config.url ] []
          ]
          , label [] [
            text "Username"
            , input [ type_ "text", onInput Username, value model.config.username ] []
          ]
          , label [] [
            text "Password"
            , input [ type_ "text", onInput Password, value model.config.password ] []
          ]
        ]
        , button [ onClick LoadJson ] [ text "Load" ]
        , text (toString model.data)
        , table [] [
          thead [] [
            tr [] [
              th [] [ text "header" ]
            ]
          ]
          , tbody [] [
            tr [] [
              td [] [ text "cell" ]
            ]
          ]
        ]
      ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- HTTP

getData : Model -> Cmd Msg
getData model =
  let
      url =
        model.config.url ++ "/query?db=telegraf&q=" ++ model.query
      request =
        Http.request
          { method = "GET"
          , headers =
            [ BasicAuth.buildAuthorizationHeader model.config.username model.config.password ]
          , url = url
          , body = Http.emptyBody
          , expect = Http.expectJson decodeData
          , timeout = Nothing
          , withCredentials = False
          }
  in
      Http.send LoadedJson request

decodeData : Decode.Decoder (InfluxResult)
decodeData =
  Decode.at ["results", "0"] resultDecoder

resultDecoder : Decode.Decoder InfluxResult
resultDecoder =
  Decode.succeed InfluxResult
  |: (field "series" (Decode.list seriesDecoder))

seriesDecoder : Decode.Decoder Series
seriesDecoder =
  Decode.succeed Series
    |: (field "name" Decode.string)
    |: (field "columns" columnsDecoder)
    |: (field "values" rowsDecoder)

columnsDecoder : Decode.Decoder Columns
columnsDecoder =
  Decode.list Decode.string

rowsDecoder : Decode.Decoder Rows
rowsDecoder =
  Decode.list (Decode.list Decode.string)

