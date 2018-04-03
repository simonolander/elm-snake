module Rest exposing (getScores, postScore)

import Http
import Json.Decode
import Json.Encode
import Model


{-| The url to our web server where we host the scores
-}
url : String
url = "https://97oxr397sj.execute-api.us-west-2.amazonaws.com/staging/elm-snake-scores"


{-| A json decoder is a function-ish that lets us extract data form json strings.
    This one produces a Score if the json is like {"score": 3, "name": "anon"},
    or otherwise it returns some kind of error.
-}
scoreDecoder : Json.Decode.Decoder Model.Score
scoreDecoder =
    let
        toScore : Int -> String -> Model.Score
        toScore score name = { score = score, name = name }
    in
    Json.Decode.map2
        toScore
        (Json.Decode.field "score" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)


{-| Gets the scores from the server and decodes them into a List Score
    packed into a ReceiveScores message.
-}
getScores : Cmd Model.Msg
getScores =
    Http.get url (Json.Decode.list scoreDecoder) |> Http.send Model.ReceiveScores


{-| Posts a score to the server, and returns a list of the new scores.
-}
postScore : Model.Score -> Cmd Model.Msg
postScore { score, name } =
    let
        object =
            Json.Encode.object
                [ ("score", Json.Encode.int score)
                , ("username", Json.Encode.string name)
                ]
        body =
            Http.jsonBody
                object
    in
        Http.post url body (Json.Decode.list scoreDecoder) |> Http.send Model.ReceiveScores
