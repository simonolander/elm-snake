module App exposing (..)
import Html exposing (Html, div, text, program, pre)
import Keyboard
import Time exposing (Time, second, millisecond)
import Random

-- CONSTANTS


keyCodeW = 87
keyCodeA = 65
keyCodeS = 83
keyCodeD = 68
keyCodeLeft = 37
keyCodeUp = 38
keyCodeRight = 39
keyCodeDown = 40


-- MODEL


type Direction = Left | Up | Right | Down
type SnakeMove = Apple Snake | Move Snake | Fail Snake
type alias Point = { x: Int, y: Int }
type alias Apple = Point
type alias Snake = List Point
type alias World = { width: Int, height: Int }
type alias GameOver = Bool

type alias Model =
    { keyCode: Keyboard.KeyCode
    , direction: Direction
    , time: Time
    , snake: Snake
    , world: World
    , gameOver: GameOver
    , apple: Apple
    }


init : ( Model, Cmd Msg )
init =
    ( { keyCode = 0
      , direction = Right
      , time = 0
      , world = { width = 50, height = 50 }
      , snake = [{ x = 25, y = 25 }, { x = 24, y = 25 }]
      , gameOver = False
      , apple = { x = 30, y = 30 }
      }, Cmd.none )



-- MESSAGES


type Msg
    = KeyMsg Keyboard.KeyCode
    | Tick Time



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ pre [] [text (toString model)]
        , pre [] [model.keyCode |> toDirection |> toString |> text]
        , pre [] [renderWorld model |> text]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg code ->
            let
                wantedDirection = Maybe.withDefault model.direction (toDirection code)
                actualDirection = changeDirection model.direction wantedDirection
            in
                ( { model | keyCode = code, direction = actualDirection }, Cmd.none )
        Tick time ->
            case moveSnake model of
                Move snake ->
                    ( { model | time = time, snake = snake }, Cmd.none )
                Apple snake ->
                    ( { model | time = time, snake = snake }, Cmd.none )
                Fail snake ->
                    ( { model | time = time, snake = snake, gameOver = True }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg
        , Time.every (30 * Time.millisecond) Tick
        ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- FUNCTIONS

toDirection : Keyboard.KeyCode -> Maybe Direction
toDirection code =
    case code of
        87 -> Just Up       -- W
        65 -> Just Left     -- A
        83 -> Just Down     -- S
        68 -> Just Right    -- D
        37 -> Just Left     -- Left
        38 -> Just Up       -- Up
        39 -> Just Right    -- Right
        40 -> Just Down     -- Down
        default -> Nothing

changeDirection : Direction -> Direction -> Direction
changeDirection from to =
    case (from, to) of
        (Up, Down) -> from
        (Down, Up) -> from
        (Left, Right) -> from
        (Right, Left) -> from
        default -> to

renderWorld : Model -> String
renderWorld model =
    List.range 0 (model.world.height - 1)
    |> List.map (renderWorldLine model)
    |> String.join (String.fromList ['\n'])

renderWorldLine : Model -> Int -> String
renderWorldLine model y = List.range 0 (model.world.width - 1) |> List.map (renderWorldCell model y) |> String.fromList

renderWorldCell : Model -> Int -> Int -> Char
renderWorldCell { snake, apple } y x =
    let
        point = { x = x, y = y }
    in
        if point == apple
            then ''
        else if List.member point snake
            then 'O'
            else ' '

movePoint : Direction -> Point -> Point
movePoint dir p =
    case dir of
        Left ->
            { p | x = p.x - 1}
        Up ->
            { p | y = p.y - 1}
        Right ->
            { p | x = p.x + 1}
        Down ->
            { p | y = p.y + 1}

moveSnake : Model -> SnakeMove
moveSnake {world, snake, direction, apple} =
    let
        maybeHead = List.head snake
    in
        case maybeHead of
            Just head ->
                let
                    newHead = movePoint direction head
                    newTail = List.take (List.length snake - 1) snake
                in
                    if List.member newHead newTail
                    then
                        Fail snake
                    else if newHead.x < 0 || newHead.x >= world.width || newHead.y < 0 || newHead.y >= world.height
                    then
                        Fail snake
                    else if newHead == apple
                    then
                        Apple (newHead :: snake)
                    else
                        Move (newHead :: newTail)
            Nothing ->
                Fail snake
