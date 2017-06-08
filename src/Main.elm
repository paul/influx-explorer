import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (field)
import Json.Decode.Extra exposing ((|:))



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
    , data : Maybe (List InfluxResult)
    , config : Config}

type alias Config =
  { url : String
  , username : String
  , password : String }

init : String -> (Model, Cmd Msg)
init query =
  ( Model query Nothing (Config "" "" "")
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
          ( model, getData model.query )

        LoadedJson (Ok newData) ->
          ({ model | data = newData }, Cmd.none)

        LoadedJson (Err _) ->
          (model, Cmd.none)

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
            , input [ type_ "text", onInput Url ] [ text model.config.url ]
          ]
          , label [] [
            text "Username"
            , input [ type_ "text", onInput Username ] [ text model.config.username ]
          ]
          , label [] [
            text "Password"
            , input [ type_ "text", onInput Password ] [ text model.config.password ]
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

getData : String -> Cmd Msg
getData query =
  let
      url =
        "https://influxdb.textus-staging.net/query?db=telegraf&q=" ++ query
      request =
        Http.request
          { method = "GET"
          , headers =
            [ Http.header "Authorization" "Basic dGV4dHVzOjQyYWp0c09WYWhKZUZXRHJw" ]
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
  -- Decode.at ["results", "0", "series", "0", "columns", "0"] Decode.string
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

