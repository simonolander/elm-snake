module Rest exposing (..)

import Http
import Json.Decode
import Json.Encode
import Model


url : String
url = "https://97oxr397sj.execute-api.us-west-2.amazonaws.com/staging/elm-snake-scores"


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


getScores : Cmd Model.Msg
getScores =
    Http.get url (Json.Decode.list scoreDecoder) |> Http.send Model.ReceiveScores


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
