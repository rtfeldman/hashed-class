module Css.Class.Internal exposing (toClassname)

{-| This is separate from Class.elm so we can test `toClassname` without
exposing it publicly.
-}

import Css exposing (Snippet, Style)
import FNV
import Hex


toClassname : List Style -> String
toClassname styles =
    -- TODO Replace this comically inefficient implementation
    -- with crawling these union types and building up a hash along the way.
    styles
        |> Css.everything
        |> List.singleton
        |> Css.stylesheet
        |> List.singleton
        |> Css.compile
        |> .css
        |> FNV.hashString
        |> Hex.toString
        |> String.cons '_'
