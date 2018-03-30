module Footer exposing (footer)

import Html
import Html.Attributes exposing (class, href)

footer =
     Html.footer
        [ class "footer" ]
        [ Html.span [] [ Html.text "@ Simon Olander Sahlén" ]
        , Html.br [] []
        , Html.a [ href "https://github.com/simonolander/elm-snake"] [ Html.text "Source code" ]
        ]
