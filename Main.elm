module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)

-- MSG

type Msg
    = None

-- MODEL

type alias Model = 
    { background_photo: String
    }

type alias Menu = {}

type alias Dish = {}

type alias ContactInfo = {}

initialModel : Model
initialModel = { background_photo = "photos/3-23-2018.JPG" }

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model , Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    div [] [ img [class "img", src model.background_photo][]
    ]

init : (Model, Cmd Msg)
init =
    ( initialModel, Cmd.none )

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }

