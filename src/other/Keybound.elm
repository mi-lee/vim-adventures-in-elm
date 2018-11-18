module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Events as Events
import Debug
import Html exposing (Html, Attribute, div, span, text)
import Html.Attributes exposing (..)
import Json.Decode as Decode


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \() -> init
        , subscriptions = subscriptions
        , update = update
        }



-- See https://github.com/elm/browser/blob/1.0.0/notes/keyboard.md

type alias Model =
    { world : String,
      point: Int
    }


type Msg
    = KeyDowns String
    | ClearPressed


init : ( Model, Cmd Msg )
init =
    ( { world = "####\n   #\n## #\n##  \n####\n",
      point = 10} , Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDowns code ->
            ( if List.member code [model.world] then
                model

              else
                model
            , Cmd.none
            )

        -- Flush the whole model on `keyup`, helps to remove not pressed keys, if focus was lost from the window.
        ClearPressed ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ style "white-space" "pre",
          style "font-family" "monospace"]
        [ text (String.slice 0 model.point model.world),
          span [ style "background-color" "fuchsia" ]
               [ text (String.slice model.point (model.point + 1) model.world)],
          text (String.dropLeft (model.point + 1) model.world)]



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown (Decode.map KeyDowns keyDecoder)
        , Events.onKeyUp (Decode.succeed ClearPressed)
        ]


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string
