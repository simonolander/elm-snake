module Scoreboard exposing (scoreboard, updateScore)

import Model exposing (Score)
import Html exposing (Html, aside, h1, input, li, ol, p, text)
import Html.Attributes exposing (class, placeholder)


{-| Renders the score board together with some header and classes.
-}
scoreboard : Model.Scoreboard -> Html msg
scoreboard scoreboard =
    aside
        [ class "aside aside-2 instructions" ]
        [ h1 [] [ text "Scoreboard" ]
        , toOl scoreboard
        ]

{-| Takes the scoreboard, sorts it, and renders it as a
    numbered list. The player's score is blue.
-}
toOl : Model.Scoreboard -> Html msg
toOl scoreboard =
    let
        addDefaultClass = flip (,) []
        scoreComparison (a,_) (b,_) =
            case compare a.score b.score of
                EQ -> EQ
                LT -> GT
                GT -> LT
        topTenOldScores =
            List.map addDefaultClass scoreboard.scores
            |> List.sortWith scoreComparison
            |> List.take 10
        topTenScores =
            topTenOldScores
            |> (::) ( scoreboard.currentScore, [ class "currentScore" ] )
            |> List.sortWith scoreComparison
        highScore =
            List.head topTenScores
            |> Maybe.map Tuple.first
            |> Maybe.map .score
            |> Maybe.withDefault 0
        scorePadLength = highScore |> toString |> String.length
        toLi (score, classes) =
            li classes [ text ( String.padRight 12 ' ' score.name ++ (toString score.score |> String.padLeft scorePadLength ' ') ) ]
        scores =
            List.map toLi topTenScores
    in
        ol [] scores


{-| Computes the current score from the snake.
-}
updateScore : Model.Snake -> Model.Scoreboard -> Model.Scoreboard
updateScore snake scoreboard =
    let
        oldCurrentScore = scoreboard.currentScore
        newCurrentScore = { oldCurrentScore | score = (List.length snake) - 1}
    in
        { scoreboard | currentScore = newCurrentScore }


