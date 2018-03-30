module Update exposing (update)

import Keyboard
import Time
import Model exposing (..)
import Snake exposing (..)
import Scoreboard exposing (..)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg code -> keyMsg code model
        Tick time -> tick time model
        NewApple apple ->
            ( { model | apple = Just apple }, Cmd.none )


keyMsg : Keyboard.KeyCode -> Model -> ( Model, Cmd Msg )
keyMsg code model =
    case model.gameState of
        NotStarted ->
            case code of
                32 -> ( { model | keyCode = code, gameState = Running }, Cmd.none )
                default -> ( { model | keyCode = code }, Cmd.none )
        GameOver ->
            let
                scoreboard = model.scoreboard
                currentScore = scoreboard.currentScore
            in
                case code of
                    32 -> ( { model
                            | keyCode = code
                            , gameState = Running
                            , direction = Right
                            , time = 0
                            , snake = [(model.world.width // 2, model.world.height // 2)]
                            , scoreboard = { scoreboard
                                           | scores = ( scoreboard.currentScore :: scoreboard.scores )
                                           , currentScore = { currentScore
                                                            | score = 0
                                                            }
                                           }
                            }, generateApple model.world )
                    default -> ( { model | keyCode = code }, Cmd.none )
        Running ->
            case code of
                32 -> ( { model | keyCode = code, gameState = Paused }, Cmd.none )
                default ->
                    let
                        direction =
                            toDirection code
                            |> Maybe.andThen (changeDirection model.snake)
                            |> Maybe.withDefault model.direction
                    in
                        ( { model | keyCode = code, direction = direction }, Cmd.none )
        Paused ->
            case code of
                32 -> ( { model | keyCode = code, gameState = Running }, Cmd.none )
                default -> ( { model | keyCode = code }, Cmd.none )
        EnterName -> ( model, Cmd.none )


tick : Time.Time -> Model -> ( Model, Cmd Msg )
tick time model =
    case model.gameState of
        Running ->
            case moveSnake model of
                Move snake ->
                    ( { model | time = time, snake = snake }, Cmd.none )
                Apple snake ->
                    ( { model | time = time, snake = snake, apple = Nothing, scoreboard = updateScore snake model.scoreboard }, generateApple model.world )
                Fail snake ->
                    ( { model
                      | time = time
                      , snake = snake
                      , gameState = GameOver
                      }, Cmd.none )
        default -> ( { model | time = time }, Cmd.none )
