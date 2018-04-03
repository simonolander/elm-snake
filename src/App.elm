module App exposing (..)

import Footer exposing (footer)
import Html exposing (Html, article, div, node, program)
import Html.Attributes exposing (attribute, class)
import Instructions exposing (instructions)
import Keyboard
import Model exposing (..)
import Names exposing (generateName)
import Render exposing (getRenderParams, render)
import Rest exposing (getScores)
import Scoreboard exposing (scoreboard)
import Snake exposing (generateApple)
import Time
import Title exposing (title)
import Update exposing (update)


{-| The init method specifies the initial model and the commands to run initially.
    When the game starts, we need to setup the world, some zero score, and compute the render parameters.
    We also need to generate a random apple, pick a name for our player, and get the scoreboard from the
    server.
-}
init : ( Model, Cmd Msg )
init =
    let
        world = { width = 26, height = 26 }
        snake = [(world.width // 2, world.height // 2)]
        model = { keyCode = 0
                , direction = Right
                , time = 0
                , world = world
                , snake = snake
                , apple = Nothing
                , rp = getRenderParams world
                , scoreboard = { scores = [], currentScore = { score = 0, name = "anon" } }
                , gameState = NotStarted
                }
    in
    ( model
    , Cmd.batch
        [ generateApple snake model.world
        , generateName
        , getScores
        ]
    )


{-| The view method takes the model and renders it into Html. We also sneakily include our .css here.
-}
view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
    [ node "link" [ attribute "rel" "stylesheet", attribute "type" "text/css", attribute "href" "/src/css/main.css"] []
    , title
    , article [ class "main" ] [render model]
    , instructions
    , scoreboard model.scoreboard
    , footer
    ]


{-| Subscriptions let us get notified of events that happen outside our control.
    Depending on the current GameState, we are interested in different events.
    We always want to be notified of key presses, but are only interested in time
    when the game is actually running.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        NotStarted ->
            Sub.batch [ Keyboard.downs KeyMsg ]
        GameOver ->
            Sub.batch [ Keyboard.downs KeyMsg ]
        Paused ->
            Sub.batch [ Keyboard.downs KeyMsg ]
        EnterName ->
            Sub.batch [ Keyboard.downs KeyMsg ]
        Running ->
            Sub.batch [ Keyboard.downs KeyMsg
                      , Time.every (50 * Time.millisecond) Tick
                      ]


{-| The main method creates our program. Here we specify our
        - Initial State
        - View function
        - Update function
        - Subscriptions
-}
main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

