module Main exposing (init, main)

import Browser
import Html exposing (Html, a, div, input, label, pre, text, textarea)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onInput)
import Template exposing (render, template, withString, withValue)
import Url.Builder exposing (crossOrigin, string)


type Msg
    = ChangeTitle String
    | ChangeBody String
    | ChangeAuthor String

type alias Model =
    { filename : String
    , title : String
    , body : String
    , author : String
    , timestamp : String
    }



main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


articleUrl : Model -> String
articleUrl model =
    crossOrigin "https://github.com"
        [ "arkency", "posts", "new", "master", "posts" ]
        [ string "filename" model.filename
        , string "value" (article model)
        ]


articlePreview : Model -> String
articlePreview model =
    "# " ++ model.filename ++ "\n\n" ++ article model


article : Model -> String
article model =
    let
        articleTemplate =
            template "---"
                |> title
                |> createdAt
                |> author
                |> tags
                |> publish
                |> withString "\n---"
                |> body

        publish template =
            template |> withString "\npublish: false"

        tags template =
            template |> withString "\ntags: []"

        author template =
            template |> withString "\nauthor: " |> withValue .author

        title template =
            template |> withString "\ntitle: " |> withValue .title

        createdAt template =
            template |> withString "\ncreated_at: " |> withValue .timestamp

        body template =
            template |> withString "\n\n" |> withValue .body
    in
    render model articleTemplate


init : Model
init =
    { filename = "2020-04-17-something-something.md"
    , title = ""
    , body = ""
    , author = "Kaka Dudu"
    , timestamp = "2020-04-17 18:22:48 +0200"
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeTitle title_ ->
            { model | title = title_ }

        ChangeBody body_ ->
            { model | body = body_ }

        ChangeAuthor author_ ->
            { model | author = author_ }


view : Model -> Html Msg
view model =
    div [ class "static" ]
        [ div [ class "absolute bottom-0 left-0 border-t border-gray-300 w-full bg-white py-4 px-4" ]
            [ a
                [ href (articleUrl model)
                , class "bg-blue-500 hover:bg-blue-700 text-white font-semibold py-2 px-4 rounded"
                ]
                [ text "Draft Article" ]
            ]
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
