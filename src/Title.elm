module Title exposing (title)

import Html exposing (h1, header, text)
import Html.Attributes exposing (class)

title =
    header
        [ class "header title" ]
        [ h1 [] [ text "elm-snake" ] ]
