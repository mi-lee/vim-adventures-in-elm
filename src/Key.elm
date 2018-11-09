module Key exposing (..)


type Key
    = Space
    | ArrowLeft
    | ArrowRight
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        32 ->
            Space

        37 ->
            ArrowLeft

        39 ->
            ArrowRight

        _ ->
            Unknown
