module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (normalize)
import Test exposing (..)


suite : Test
suite =
    describe "normalize"
        [ test "replaces whitespace" <|
            \_ ->
                normalize "white space"
                    |> Expect.equal "white-space"
        , test "lowercase" <|
            \_ ->
                normalize "LoweR"
                    |> Expect.equal "lower"
        , test "trim" <|
            \_ ->
                normalize " spacy "
                    |> Expect.equal "spacy"
        , test "dot" <|
            \_ ->
                normalize "19.05.2016"
                    |> Expect.equal "19-dot-05-dot-2016"
        , test "slash" <|
            \_ ->
                normalize "a/b"
                    |> Expect.equal "a-slash-b"
        , test "percent" <|
            \_ ->
                normalize "70%"
                    |> Expect.equal "70-percent"
        , test "plus" <|
            \_ ->
                normalize "rethink + react"
                    |> Expect.equal "rethink-plus-react"
        , test "commas" <|
            \_ ->
                normalize "newrelic, honeybadger"
                    |> Expect.equal "newrelic-honeybadger"
        , test "skipped" <|
            \_ ->
                normalize "#prepen'(d)?!"
                    |> Expect.equal "prepend"
        ]
