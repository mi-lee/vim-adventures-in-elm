module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Events as Events
import Html exposing (Attribute, Html, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Decode as Decode
import String


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \() -> init
        , subscriptions = subscriptions
        , update = update
        }


type alias Model =
    { world : String
    , point : Int
    }

wall = "#"
newLine = "\n"

-- Operator functions
-- Moves point forward/backward by a character
forward: Model -> Model
forward model =
    if (String.slice (model.point + 1) (model.point + 2) model.world) == wall then
      { model | point = model.point}
    else
      { model | point = model.point + 1} -- TODO test for EoL, EoF
backward model =
    if (String.slice (model.point - 1) model.point model.world) == wall then
        { model | point = model.point } -- TODO test for EoL, BoF
    else
        { model | point = model.point - 1}
-- Moves point up/down lines
upward model =
    model -- TODO traverse back to EoL to measure column(get-column), traverse back to next EoL and forward to appropriate column,
downward model =
    model -- TODO " forward

-- Moves point to beginning/end of lines
startline model =
    { model | point = model.point + 2}
endline model =
    { model | point = model.point - 2}

-- Utility functions
getcolumn: Model -> Int -- Gets column of point
getcolumn model =
    1

-- Traverses string
--  for number of characters
--  until it encounters a character that returns True
-- Reports the distance travelled? Or new Model?
-- traverse: Model -> Int -> (Char -> Bool) -> Model

init : ( Model, Cmd Msg )
init =
    ( { world = "+----+----+----+\n#              #\n+----+----+    +\n|         |      \n+    +----+----+----+"
      , point = 18
      }
    , Cmd.none
    )


type Msg
    = KeyPress String
    | ClearPressed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPress code ->
            case code of
                "h" -> (backward model, Cmd.none)
                "l" -> (forward model, Cmd.none)
                "I" -> (startline model, Cmd.none)
                "A" -> (endline model, Cmd.none)
                _   -> ( { model | world = model.world }, Cmd.none )
        ClearPressed ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyPress (Decode.map KeyPress keyDecoder)
        , Events.onKeyUp (Decode.succeed ClearPressed)
        ]


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


view : Model -> Html Msg
view model =
    div
        [ style "white-space" "pre"
        , style "font-family" "monospace"
        ]
        [ text (String.slice 0 model.point model.world)
        , span [ style "background-color" "fuchsia" ]
            [ text (String.slice model.point (model.point + 1) model.world) ]
        , text (String.dropLeft (model.point + 1) model.world)
        ]
