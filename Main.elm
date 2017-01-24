import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing (string)
import Json.Decode.Pipeline as JsonPipeline exposing (decode, required)
import Task

-- Main
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { searchString : String
  , title : String
  , posterUrl : String
  }

init : (Model, Cmd Msg)
init =
  ( Model "Frozen" "" ""
  , getMoviePoster "Frozen"
  )

-- Update
type Msg
  = GetPoster
  | NewImg (Result Http.Error Movie)
  | UpdateSearchString String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetPoster ->
      { model | posterUrl = "waiting.gif"
      , title = ""
      } ! [ getMoviePoster model.searchString ]

    NewImg (Ok movie) ->
      ( Model model.searchString movie.title movie.posterUrl, Cmd.none)

    NewImg (Err _) ->
      let
        errorMessage =
          "We couldn't find that movie 😳 "
        errorImage =
          "oh-no.jpg"
      in
        ( Model model.searchString errorMessage errorImage, Cmd.none )

    UpdateSearchString newSearchString ->
      { model | searchString = newSearchString} ! []

-- View
view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Enter a movie title"
            , value model.searchString
            , autofocus True
            , onInput UpdateSearchString
            ] []
    , button [ onClick GetPoster ] [ text "Get poster" ]
    , br [] []
    , h1 [] [ text model.title ]
    , img [ src model.posterUrl ] []
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- HTTP
getMoviePoster : String -> Cmd Msg
getMoviePoster searchString =
    let
      url =
        "//www.omdbapi.com/?t=" ++ searchString
    in
      Http.send NewImg (Http.get url decodeMovieUrl)

type alias Movie =
  { title : String
  , posterUrl : String
  }

decodeMovieUrl : Json.Decoder Movie
decodeMovieUrl =
  decode Movie
    |> JsonPipeline.required "Title" string
    |> JsonPipeline.required "Poster" string

