module Instructions exposing (instructions)

import Html exposing (aside, h1, p, text)
import Html.Attributes exposing (class)

instructions =
    aside
        [ class "aside aside-1 instructions" ]
        [ h1 [] [ text "Instructions" ]
        , p [] [ text "WASD or arrow keys to move" ]
        , p [] [ text "Spacebar to pause" ]
        , p [] [ text "Collect the apples for points" ]
        , p [] [ text "Don't hit yourself or the wall" ]
        ]
