module Title exposing (title)

import Html exposing (h1, header, text)
import Html.Attributes exposing (class)

{-| Render the title text.
-}
title =
    header
        [ class "header title" ]
        [ h1 [] [ text "elm-snake" ] ]
