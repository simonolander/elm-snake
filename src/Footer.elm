module Footer exposing (footer)

import Html
import Html.Attributes exposing (class, href)


{-| The footer is easy. We just print the author's name and a link to his repo.
-}
footer =
     Html.footer
        [ class "footer" ]
        [ Html.span [] [ Html.text "@ Simon Olander Sahlén" ]
        , Html.br [] []
        , Html.a [ href "https://github.com/simonolander/elm-snake"] [ Html.text "Source code" ]
        ]
