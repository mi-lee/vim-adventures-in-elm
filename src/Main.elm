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

-- Operator functions
-- Moves point forward/backward by a character
forward: Model -> Model
forward model =
    { model | point = model.point + 1} -- TODO test for EoL, EoF
backward model =
    { model | point = model.point - 1} -- TODO test for EoL, BoF

-- Moves point up/down lines
upward model =
    model -- TODO traverse back to EoL to measure column(get-column), traverse back to next EoL and forward to appropriate column,
downward model =
    model -- TODO " forward

-- Moves point to beginning/end of lines
startline model =
    model -- TODO traverse back to EoL
endline model =
    model -- TODO traverse forward to EoL

-- Utility functions
getcolumn: Model -> Int -- Gets column of point
getcolumn model =
    1 -- TODO traverse back to EoL to measure

-- Traverses string
--  for number of characters
--  until it encounters a character that returns True
-- Reports the distance travelled? Or new Model?
-- traverse: Model -> Int -> (Char -> Bool) -> Model

init : ( Model, Cmd Msg )
init =
    ( { world = "Hello World\nNewline?\nExcellent"
      , point = 7
      }
    , Cmd.none
    )


type Msg
    = KeyDowns String
    | ClearPressed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDowns code ->
            case code of
                "h" -> (backward model, Cmd.none)
                "l" -> (forward model, Cmd.none)
                _   -> ( { model | world = model.world ++ code }, Cmd.none )
        ClearPressed ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown (Decode.map KeyDowns keyDecoder)
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
