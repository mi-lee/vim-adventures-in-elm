import Browser
import Html exposing (Html, Attribute, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
-- import Debug

main =
    Browser.sandbox {init = init, update = update, view = view }

type alias Model =
    { world : String,
      point: Int
    }

init : Model
init =
    { world = "Hello World\nNewline?\nExcellent",
      point = 7}

type Msg
    = Char

update : Msg -> Model -> Model
update msg model =
    model



view : Model -> Html Msg
view model =
    div [ style "white-space" "pre",
          style "font-family" "monospace"]
        [ text (String.slice 0 model.point model.world),
          span [ style "background-color" "fuchsia" ]
               [ text (String.slice model.point (model.point + 1) model.world)],
          text (String.dropLeft (model.point + 1) model.world)]
