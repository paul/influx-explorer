import Html exposing (..)
--import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
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
    , data : Maybe InfluxResults }

init : String -> (Model, Cmd Msg)
init query =
  ( Model query Nothing
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
  { name: String
  }

type alias Rows =
  List Row

type alias Row =
  List Value

type alias Value = String


-- UPDATE


type Msg
    = LoadJson
    | LoadedJson (Result Http.Error InfluxResults)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadJson ->
          ( model, getData model.query )

        LoadedJson (Ok newData) ->
          ({ model | data = newData }, Cmd.none)

        LoadedJson (Err _) ->
          (model, Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "Hello, world!"
        , text (toString model.data)
        , button [ onClick LoadJson ] [ text "Load" ]
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

decodeData : Decode.Decoder InfluxResults
decodeData =
  -- Decode.at ["results", "0", "series", "0", "columns", "0"] Decode.string
  resultsDecoder

resultsDecoder : Decode.Decoder InfluxResults
resultsDecoder =
  Decode.succeed InfluxResults
    field "results" (Decode.list resultDecoder)

resultDecoder : Decode.Decoder InfluxResult
resultDecoder =
  Decode.succeed InfluxResult
    field "series" (Decode.list seriesDecoder)

seriesDecoder : Decode.Decoder (List Series)
seriesDecoder =
  Decode.map3 Series
    (field "name" Decode.string)
    (field "columns" (Decode.list columnsDecoder))
    (field "values" (Decode.list rowsDecoder))

columnsDecoder : Decode.Decoder Columns
columnsDecoder =
  Decode.succeed Columns
    field "columns" (Decode.list columnDecoder)

columnDecoder : Decode.Decoder Column
columnDecoder =
  Decode.succeed Column
    Column

rowsDecoder : Decode.Decoder Rows
rowsDecoder =
  Decode.succeed Rows
    field "values" (Decode.list rowDecoder)

rowDecoder : Decode.Decoder Row
rowDecoder =
  Decode.succeed Row
    Decode.list valueDecoder

valueDecoder : Decode.Decoder Value
valueDecoder =
  Decode.succeed Value
    Decode.string






