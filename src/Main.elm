module Main exposing (init, main)

import Browser
import Date
import Html exposing (Html, a, div, input, label, pre, text, textarea)
import Html.Attributes exposing (class, disabled, href)
import Html.Events exposing (onInput)
import Iso8601
import Task
import Template exposing (render, template, withString, withValue)
import Time exposing (now, utc)
import Url.Builder exposing (crossOrigin, string)


type Msg
    = ChangeTitle String
    | ChangeBody String
    | ChangeAuthor String
    | ReceiveTime Time.Posix


type alias Model =
    { title : String
    , body : String
    , author : String
    , timestamp : Maybe Time.Posix
    }


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { title = ""
      , body = ""
      , author = "Kaka Dudu"
      , timestamp = Nothing
      }
    , Time.now |> Task.perform ReceiveTime
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveTime time ->
            ( { model | timestamp = Just time }, Cmd.none )

        ChangeTitle title_ ->
            ( { model | title = title_ }, Cmd.none )

        ChangeBody body_ ->
            ( { model | body = body_ }, Cmd.none )

        ChangeAuthor author_ ->
            ( { model | author = author_ }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "static" ]
        [ div [ class "absolute bottom-0 left-0 border-t border-gray-300 w-full bg-white py-4 px-4" ] [ draftLink model ]
        , div [ class "grid grid-cols-2 gap-4" ]
            [ div [ class "py-16 px-10" ]
                [ div
                    [ class "flex flex-wrap -mx-3 mb-6" ]
                    [ div [ class "w-full px-3" ]
                        [ label
                            [ class "block uppercase tracking-wide text-gray-700 text-xs font-bold mb-2" ]
                            [ text "Title" ]
                        , input
                            [ onInput ChangeTitle
                            , class "appearance-none block w-full bg-gray-100 text-gray-700 border border-gray-200 rounded py-3 px-4 mb-3 leading-tight focus:outline-none focus:bg-white"
                            ]
                            []
                        ]
                    ]
                , div
                    [ class "flex flex-wrap -mx-3 mb-6" ]
                    [ div [ class "w-full px-3" ]
                        [ label
                            [ class "block uppercase tracking-wide text-gray-700 text-xs font-bold mb-2" ]
                            [ text "Author" ]
                        , input
                            [ onInput ChangeAuthor
                            , class "appearance-none block w-full bg-gray-100 text-gray-700 border border-gray-200 rounded py-3 px-4 mb-3 leading-tight focus:outline-none focus:bg-white"
                            ]
                            []
                        ]
                    ]
                , div [ class "flex flex-wrap -mx-3 mb-6" ]
                    [ div [ class "w-full px-3" ]
                        [ label [ class "block uppercase tracking-wide text-gray-700 text-xs font-bold mb-2" ] [ text "Body" ]
                        , textarea
                            [ onInput ChangeBody
                            , class "appearance-none block w-full h-64 bg-gray-100 text-gray-700 border border-gray-200 rounded py-3 px-4 mb-3 leading-tight focus:outline-none focus:bg-white"
                            ]
                            []
                        ]
                    ]
                ]
            , div [ class "py-16 px-10 bg-gray-200 min-h-screen" ]
                [ pre [ class "text-sm" ]
                    [ text (articlePreview model) ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


draftLink model =
    case model.timestamp of
        Nothing ->
            a [ disabled True ] [ text "Draft Article" ]

        Just time ->
            a
                [ href (articleUrl (filename model.title time) (rawArticle model))
                , class "bg-blue-500 hover:bg-blue-700 text-white font-semibold py-2 px-4 rounded"
                ]
                [ text "Draft Article" ]


articleUrl : String -> String -> String
articleUrl filename_ content =
    crossOrigin "https://github.com"
        [ "arkency", "posts", "new", "master", "posts" ]
        [ string "filename" filename_
        , string "value" content
        ]


articlePreview : Model -> String
articlePreview model =
    case model.timestamp of
        Nothing ->
            ""

        Just time ->
            "# "
                ++ filename model.title time
                ++ "\n\n"
                ++ rawArticle model


rawArticle : Model -> String
rawArticle model =
    let
        yamlFrontMatterTemplate time =
            template ""
                |> withString """---
title: """
                |> withValue .title
                |> withString """
created_at: """
                |> withString (Iso8601.fromTime time)
                |> withString """
author: """
                |> withValue .author
                |> withString """
tags: []
publish: false
---

"""
    in
    case model.timestamp of
        Nothing ->
            ""

        Just time ->
            let
                articleTemplate =
                    yamlFrontMatterTemplate time
                        |> withValue .body
            in
            render model articleTemplate


filename : String -> Time.Posix -> String
filename title timestamp =
    String.join "-"
        [ Date.toIsoString (Date.fromPosix utc timestamp)
        , "something-something"
        , ".md"
        ]
