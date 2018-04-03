module Instructions exposing (instructions)

import Html exposing (aside, h1, p, text)
import Html.Attributes exposing (class)

{-| Instructions are just some info on how to play the game.
-}
instructions =
    aside
        [ class "aside aside-1 instructions" ]
        [ h1 [] [ text "Instructions" ]
        , p [] [ text "WASD or arrow keys to move" ]
        , p [] [ text "Spacebar to pause" ]
        , p [] [ text "'n' to change name" ]
        , p [] [ text "Collect the apples for points" ]
        , p [] [ text "Don't hit yourself or the wall" ]
        ]
