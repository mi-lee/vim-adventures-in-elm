module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Events as Events
import Html exposing (Attribute, Html, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Decode as Decode
import String
import Debug
import Array
import Basics

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

-- Regular utilities
-- Character utilities
wall = "#"
newLine = "\n"


type Motion = Lateral | Vertical
obstructs: Motion -> String -> Bool
obstructs op char =
    case char of
        "#" -> True
        "\n" -> True
        _ -> case op of
                 Lateral ->
                     case char of
                         "|" -> True
                         _ -> False
                 Vertical ->
                     case char of
                         "-" -> True
                         _ -> False

-- String utilities
--- TODO It's worth discussing any differences between Strings and list of Chars
--- or how we might use the type system to organize the various char operations

-- Returns the string/char?? at index location.
--- Bamboozling that we need write this
access: String -> Int -> String
access string index =
    String.slice index (index + 1) string


-- Model utilities
scan: Model-> Int -> String
scan model distance =
    access model.world (model.point + distance)
-- Usage: scan model 0 is under the cursor, scan model +/-1 is forward/backward
-- TODO who wants scan to check for world boundaries? (0 and length of world)


seek: Model-> Int -> Model
seek model distance =
    { model | point  = model.point + distance }

lt a b =
    b < a
gt a b =
    b > a
-- Robust utility function that find char and begining/endlines could be built on
-- Returns the stream of distances to the given char
-- Figure out how get as a stream(lazy list) TODO
-- Figure out how to wrap TODO
locateb: Model -> String -> List Int
locateb model char =
    (List.reverse (List.filter (lt model.point) (String.indexes char model.world)))
locatef model char =
    (List.filter (gt model.point) (String.indexes char model.world))

-- Operator functions
-- Moves point forward/backward by a character
 -- TODO? || model.point < 1 This check may be taken up in scan
type alias Operator = Model -> Model
forward: Operator
forward model =
    if scan model 1 |> obstructs Lateral then
        model
    else
        seek model 1
backward: Operator
backward model =
    if scan model -1 |> obstructs Lateral then
        model
    else
        seek model -1

-- Moves point up/down lines
upward model =
    let columnPoint = Maybe.withDefault 0 (List.head (List.reverse (String.indexes newLine (String.left model.point model.world))))
        row =  model.point - columnPoint
        nextNewLineCharacter = Maybe.withDefault 0 (Array.get ((getcolumn model) - 2) (Array.fromList (String.indexes newLine model.world)))
    in
    { model | point = nextNewLineCharacter + row }
downward model =
    let columnPoint = Maybe.withDefault 0 (List.head (List.reverse (String.indexes newLine (String.left model.point model.world))))
        row =  model.point - columnPoint
        nextNewLineCharacter = Maybe.withDefault 0 (Array.get (getcolumn model) (Array.fromList (String.indexes newLine model.world)))
    in
    { model | point = nextNewLineCharacter + row }

-- Moves point to beginning/end of lines
-- TODO Should they travel through obstructions?
startline model =
    case List.head (locateb model newLine) of
        Just a  -> {model | point = a+1 }
        Nothing -> {model | point = 0}
endline model =
    case List.head (locatef model newLine) of
        Just a  -> {model | point = a-1 }
        Nothing -> {model | point = String.length model.world}

-- Utility functions
getcolumn: Model -> Int -- Gets column of point
-- Gets the string from the left of the point.
-- Gets all indexes of new line characters from that string then takes the length of that list.
getcolumn model =
    List.length (String.indexes newLine (String.left model.point model.world))

-- Traverses string
--  for number of characters
--  until it encounters a character that returns True
-- Reports the distance travelled? Or new Model?
-- traverse: Model -> Int -> (Char -> Bool) -> Model

init : ( Model, Cmd Msg )
init =
    ( { world = "\n+----+----+----+\n#              #\n+----+----+    +\n|         |      \n+    +----+----+----+"
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
                "k" -> (upward model, Cmd.none)
                "j" -> (downward model, Cmd.none)
                "^" -> (startline model, Cmd.none)
                "$" -> (endline model, Cmd.none)
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
        [ style "white-space" "pre-wrap"
        , style "font-family" "monospace"
        ]
        [ text (String.slice 0 model.point model.world)
        , span [ style "background-color" "fuchsia" ]
            [ text (String.slice model.point (model.point + 1) model.world) ]
        , text (String.dropLeft (model.point + 1) model.world)
        ]
