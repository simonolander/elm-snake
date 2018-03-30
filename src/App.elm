module App exposing (..)

import Model exposing (..)
import Array exposing (Array)
import Color
import Html exposing (Html, article, aside, aside, div, footer, header, node, pre, program, text)
import Collage
import Keyboard
import Render exposing (getRenderParams, render)
import Scoreboard exposing (scoreboard, updateScore)
import Snake exposing (generateApple)
import Time exposing (Time, second, millisecond)
import Random
import Dict exposing (Dict)
import Element exposing (Element)
import Html.Attributes exposing (attribute, class, style)
import Instructions exposing (instructions)
import Title exposing (title)
import Update exposing (update)


init : ( Model, Cmd Msg )
init =
    let
        world = { width = 26, height = 26 }
        snake = [(world.width // 2, world.height // 2)]
    in
    ( { keyCode = 0
      , direction = Right
      , time = 0
      , world = world
      , snake = snake
      , apple = Nothing
      , rp = getRenderParams world
      , scoreboard = { scores = [], currentScore = { score = 0, name = "Simon" } }
      , gameState = NotStarted
      }, generateApple world )


-- VIEW


view : Model -> Html Msg
view model =
    gameView model

gameView model =
    div [ class "wrapper" ]
    [ node "link" [ attribute "rel" "stylesheet", attribute "type" "text/css", attribute "href" "/src/css/main.css"] []
    , title
    , article [ class "main" ] [render model]
    , instructions
    , scoreboard model.scoreboard
    , footer [ class "footer" ] [ text "Footer" ]
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        NotStarted ->
            Sub.batch [ Keyboard.downs KeyMsg ]
        GameOver ->
            Sub.batch [ Keyboard.downs KeyMsg ]
        Running ->
            Sub.batch [ Keyboard.downs KeyMsg
                      , Time.every (50 * Time.millisecond) Tick
                      ]
        Paused ->
            Sub.batch [ Keyboard.downs KeyMsg ]
        EnterName ->
            Sub.batch [ Keyboard.downs KeyMsg ]


-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

