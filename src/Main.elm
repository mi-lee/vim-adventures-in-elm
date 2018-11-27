module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)
--  Elm modules import & interfaces {{{
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


-- }}}

-- String/Char/Misc utilities and aliases {{{
wall = "#"
newLine = "\n"

--- TODO It's worth discussing any differences between Strings and list of Chars
--- or how we might use the type system to organize the various char operations

-- Returns the string/char?? at index location.
--- Bamboozling that we need write this.
access: String -> Int -> String
access string index =
    String.slice index (index + 1) string

-- Aliases for comparison operators, feel free to refactor code so we don't need these if you find it more aesthetic
lt a b =
    b < a
gt a b =
    b > a
-- }}}
-- Model: State, Selectors, Mutators {{{
type alias Model =
    { world : String
    , point : Int
    , numprefix : Int
    }

scan: Model-> Int -> String
scan model distance =
    access model.world (model.point + distance)
-- Usage: scan model 0 is under the cursor, scan model +/-1 is forward/backward
-- TODO who wants scan to check for world boundaries? (0 and length of world)

seek: Model-> Int -> Model
seek model distance =
    { model | point  = model.point + distance }

type Direction = Forward | Backward
incr: Direction -> Int
incr dir =
    case dir of
        Forward -> 1
        Backward -> -1

pointrelative: Model -> Int -> Int
pointrelative model index =
  abs (model.point - index)
locate: Direction -> Model -> String -> List Int
locate dir model char =
    let index = String.indexes char model.world in
    List.map (pointrelative model) <| case dir of
        Forward -> List.filter (gt model.point) index
        Backward -> List.reverse (List.filter (lt model.point) index)
-- Returns the stream of distances to the given char
-- Figure out how get as a stream(lazy list) so that seeking small distances isn't O(n) TODO
-- Figure out how to wrap around world TODO
getcolumn: Model -> Int -- Gets column of point
getcolumn model =
   case (locate Backward model newLine) of
      first :: rest -> first
      [] -> model.point

-- }}}
-- Operator functions {{{
type alias Operator = Model -> Model
type Motion = Lateral  -- Moves point forward/backward by a character
            | Vertical -- Moves point to the previous/next row
            | Line     -- Moves point to the begining/end of rows
-- Gameplay: Motion Obstruction {{{
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
                 _ -> False
-- }}}
-- Lateral: forward & backward {{{
column: Direction -> Operator
column dir model =
  let step = incr dir in
  if scan model step |> obstructs Lateral then
      model
  else
      seek model step
--- }}}
-- Vertical: upward & downward {{{
upward model =
    case (List.take 2 (locate Backward model newLine)) of
        a :: b :: rest -> if (scan model (a-b) |> obstructs Vertical) || (a > (b-a)) then
                                    model
                                else
                                    seek model (a-b)
        a :: rest -> if (scan model a |> obstructs Vertical) || (model.point-a < a) then
                                    model
                                else
                                    seek model a
        [] -> model

downward model =
    let col = getcolumn model in
    case (List.take 2 (locate Forward model newLine)) of
        a :: b :: rest ->
            if (b-a) < col || (scan model (col + a) |> obstructs Vertical) then
                model
            else
                seek model (a+col)
        a :: rest -> let dest = (col + a) in
            if String.length model.world < model.point + dest ||
               (scan model dest |> obstructs Vertical) then
                           model
                       else
                           seek model dest
        [] -> model
-- TODO the way that these functions access world and point to test edge conditions is an ugly and leaky abstraction.
-- row: Direction -> Operator
-- row dir model =
    -- case (List.take 2 (locate dir model newLine)) of
        -- a :: b :: rest ->
            -- let dest = case dir of
                          -- Forward -> a + getcolumn model
                          -- Backward -> a-b
            -- in if (scan model dest |> obstructs Vertical) || 
        -- a :: rest -> model
        -- [] -> model
-- }}}
-- Line: startline & endline {{{
startline model =
    case List.head (locate Backward model newLine) of
        Just a  -> seek model (-a+1)
        Nothing -> {model | point = 0}
endline model =
    case List.head (locate Forward model newLine) of
        Just a  -> seek model (a-1)
        Nothing -> {model | point = String.length model.world - 1}
-- TODO Should they travel through obstructions?
-- }}}
-- Find char {{{
findforward: Model -> String -> Model
findforward model char =
    case locate Forward model char of
        car :: cdr -> seek model car
        [] -> model

-- }}}
-- Jump between matching brackets {{{
jumpmatch: Model -> Model
jumpmatch model =
    case scan model 0 of
        "(" -> findforward model ")"
        ")" -> findforward model ")"
        "{" -> findforward model "}"
        "}" -> findforward model ")"
        "<" -> findforward model ">"
        ">" -> findforward model ")"
        "[" -> findforward model "]"
        "]" -> findforward model ")"
        _ -> model
-- }}}
-- Functional Operators {{{
pushNumericPrefix: Int -> Model -> Model
pushNumericPrefix num model =
    { model | numprefix = 10*model.numprefix + num }
clearNumericPrefix model =
    { model | numprefix = 0 }

repeatOp: Operator -> Int -> Operator
repeatOp op times =
    if times > 1 then
        op >> repeatOp op (times - 1)
    else
        op

prefixCompose: Operator -> Model -> Model
prefixCompose op model =
    clearNumericPrefix (repeatOp op model.numprefix model)
-- }}}
-- }}}

-- Game Initialization {{{
init : ( Model, Cmd Msg )
init =
    ( { world = "\n+----+----+----+\n#              #\n+----+----+    +\n|         |      \n+    +----+----+----+"
      , point = 18
      , numprefix = 0
      }
    , Cmd.none
    )


--- }}}
-- Controller: Update via messages optained from input  {{{
type Msg
    = KeyPress String
    | ClearPressed

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPress code ->
            case code of
                "h" -> (prefixCompose (column Backward) model, Cmd.none)
                "l" -> (prefixCompose (column Forward) model, Cmd.none)
                "k" -> (prefixCompose upward model, Cmd.none)
                "j" -> (prefixCompose downward model, Cmd.none)
                "^" -> (prefixCompose startline model, Cmd.none)
                "$" -> (prefixCompose endline model, Cmd.none)
                "0" -> (pushNumericPrefix 0 model, Cmd.none)
                "1" -> (pushNumericPrefix 1 model, Cmd.none)
                "2" -> (pushNumericPrefix 2 model, Cmd.none)
                "3" -> (pushNumericPrefix 3 model, Cmd.none)
                "4" -> (pushNumericPrefix 4 model, Cmd.none)
                "5" -> (pushNumericPrefix 5 model, Cmd.none)
                "6" -> (pushNumericPrefix 6 model, Cmd.none)
                "7" -> (pushNumericPrefix 7 model, Cmd.none)
                "8" -> (pushNumericPrefix 8 model, Cmd.none)
                "9" -> (pushNumericPrefix 9 model, Cmd.none)
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
-- }}}
-- Runtime Loop: View function and UI listeners {{{
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
        , div [] -- TODO for testing, can clean up UI later
              [ text (String.fromInt model.numprefix) ]
        ]
-- }}}
-- vim:foldmethod=marker:foldlevel=0
