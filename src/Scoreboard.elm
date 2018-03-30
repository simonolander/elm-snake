module Scoreboard exposing (scoreboard)

import Model exposing (Model)
import Html exposing (Html, aside, h1, li, ol, p, text)
import Html.Attributes exposing (class)

scoreboard : Model -> Html msg
scoreboard model =
    aside
        [ class "aside aside-2 instructions" ]
        [ h1 [] [ text "Scoreboard" ]
        , ol [] [ li [] [ text "hej" ] ]
        ]
