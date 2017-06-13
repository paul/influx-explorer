import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (field)
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Encode
import BasicAuth

import ConfigStorage

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
    , data : InfluxResults
    , config : Config}

type alias Config =
  { url : String
  , username : String
  , password : String }

init : String -> (Model, Cmd Msg)
init query =
  ( Model query [] (Config "https://influxdb.textus-staging.net" "grafana" "")
  , Cmd.none
  )

type alias InfluxResults =
  List InfluxResult

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
    | LoadedJson (Result Http.Error (InfluxResults))
    | Url String
    | Username String
    | Password String
    | Query String


encodeConfig : Config -> Encode.Value
encodeConfig config =
  Encode.object
    [ ("url", Encode.string config.url),
      ("username", Encode.string config.username),
      ("password", Encode.string config.password)
    ]



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadJson ->
          let
            commands = Cmd.batch
              [ ConfigStorage.localStorageSet ("config", encodeConfig model.config)
              , getData model
              ]
          in
            ( model, commands )

        LoadedJson (Err _) ->
          (model, Cmd.none)

        LoadedJson (Ok newData) ->
          ({ model | data = newData }, Cmd.none)

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

        Query newQuery ->
          ({ model | query = newQuery }, Cmd.none)




-- VIEW


view : Model -> Html Msg
view model =
    div []
      <| [ h2 [] [ text "Config" ]
        , div []
          [ label [] [
              text "Url"
              , input [ type_ "text", onInput Url, value model.config.url ] []
            ]
          , label [] [
              text "Username"
              , input [ type_ "text", onInput Username, value model.config.username ] []
            ]
          , label [] [
              text "Password"
              , input [ type_ "password", onInput Password, value model.config.password ] []
            ]
          , label [] [
              text "Query"
              , input [ type_ "text", onInput Query, value model.query ] []
            ]
          ]
        , button [ onClick LoadJson ] [ text "Load" ]
        ]
        ++ List.map renderTables model.data

renderTables : InfluxResult -> Html Msg
renderTables result =
  div []
    <| List.map renderTable result.series

renderTable : Series -> Html Msg
renderTable series =
  div []
    <| [ h2 [] [ text series.name ]
       , table []
         [ thead []
           [ tr []
             <| List.map renderColumnHeader series.columns
           ]
         , tbody []
           <| List.map renderRow series.values
         ]
       ]

renderColumnHeader : Column -> Html Msg
renderColumnHeader column =
  th [] [ text column ]

renderRow : Row -> Html Msg
renderRow row =
  tr []
    <| List.map renderCell row

renderCell : Value -> Html Msg
renderCell value =
  td [] [ text value ]



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

decodeData : Decode.Decoder (InfluxResults)
decodeData =
  Decode.at ["results"] resultsDecoder

resultsDecoder : Decode.Decoder InfluxResults
resultsDecoder =
  Decode.list resultDecoder

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

