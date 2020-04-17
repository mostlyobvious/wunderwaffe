module Main exposing (main)

import Browser
import Html exposing (h1, text)
import Html.Attributes exposing (class)

main =
  h1 [ class "text-5xl font-bold text-center" ] [ text "Hello, Elm!" ]