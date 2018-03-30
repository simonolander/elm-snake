module Scoreboard exposing (scoreboard, updateScore)

import Model exposing (Score)
import Html exposing (Html, aside, h1, input, li, ol, p, text)
import Html.Attributes exposing (class, placeholder)

scoreboard : Model.Scoreboard -> Html msg
scoreboard scoreboard =
    aside
        [ class "aside aside-2 instructions" ]
        [ h1 [] [ text "Scoreboard" ]
        , toOl (scoreboard.currentScore :: scoreboard.scores)
        ]

toOl : List Score -> Html msg
toOl list =
    List.take 10 list
    |> List.map toLi
    |> ol []


toLi : Score -> Html msg
toLi score =
    li [] [ text ( score.name ++ (": ") ++ (toString score.score) ) ]


updateScore : Model.Snake -> Model.Scoreboard -> Model.Scoreboard
updateScore snake scoreboard =
    let
        oldCurrentScore = scoreboard.currentScore
        newCurrentScore = { oldCurrentScore | score = (List.length snake) - 1}
    in
        { scoreboard | currentScore = newCurrentScore }


