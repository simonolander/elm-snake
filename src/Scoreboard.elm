module Scoreboard exposing (scoreboard, updateScore)

import Model exposing (Score)
import Html exposing (Html, aside, h1, input, li, ol, p, text)
import Html.Attributes exposing (class, placeholder)

scoreboard : Model.Scoreboard -> Html msg
scoreboard scoreboard =
    aside
        [ class "aside aside-2 instructions" ]
        [ h1 [] [ text "Scoreboard" ]
        , toOl scoreboard
        ]

toOl : Model.Scoreboard -> Html msg
toOl scoreboard =
    let
        addDefaultClass = flip (,) []
        scoreComparison (a,_) (b,_) =
            case compare a.score b.score of
                EQ -> EQ
                LT -> GT
                GT -> LT
        toLi (score, classes) =
            li classes [ text ( String.padRight 12 ' ' score.name ++ (toString score.score) ) ]
        scores =
            ( scoreboard.currentScore, [ class "currentScore" ] ) :: ( List.map addDefaultClass scoreboard.scores )
            |> List.sortWith scoreComparison
            |> List.take 10
            |> List.map toLi
    in
        ol [] scores


updateScore : Model.Snake -> Model.Scoreboard -> Model.Scoreboard
updateScore snake scoreboard =
    let
        oldCurrentScore = scoreboard.currentScore
        newCurrentScore = { oldCurrentScore | score = (List.length snake) - 1}
    in
        { scoreboard | currentScore = newCurrentScore }


