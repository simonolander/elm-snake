module App exposing (..)
import Html exposing (Html, div, text, program, pre)
import Keyboard
import Time exposing (Time, second, millisecond)
import Random
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

-- CONSTANTS


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
    , apple: Maybe Apple
    }


init : ( Model, Cmd Msg )
init =
    ( { keyCode = 0
      , direction = Right
      , time = 0
      , world = { width = 25, height = 25 }
      , snake = [{ x = 12, y = 12 }, { x = 11, y = 12 }]
      , gameOver = False
      , apple = Nothing
      }, generateApple { width = 25, height = 25 } )



-- MESSAGES


type Msg
    = KeyMsg Keyboard.KeyCode
    | Tick Time
    | NewApple Apple



-- VIEW


view : Model -> Html Msg
view model =
    gameView model


debugView model =
    div [] [ div [] [text (toString model)]
           , renderWorld model
           ]

gameView model =
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row [ Row.centerXl ]
              [ Grid.col [] [ text "1 of 3"]
              , Grid.col [ Col.xs8, Col.middleLg] [ renderWorld model ]
              , Grid.col [] [ text "3 of 3"]
              ]

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
                    ( { model | time = time, snake = snake, apple = Nothing }, generateApple model.world )
                Fail snake ->
                    ( { model | time = time, snake = snake, gameOver = True }, Cmd.none )
        NewApple apple ->
            ( { model | apple = Just apple }, Cmd.none )



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


renderWorld : Model -> Html msg
renderWorld model = pre [] [worldAsString model |> text]


worldAsString : Model -> String
worldAsString model =
    List.range 0 (model.world.height)
    |> List.map (worldLineAsString model)
    |> (::) ( "+" ++ ( String.repeat (model.world.width + 1) "-" ) ++ "+" )
    |> flip (++) [ "+" ++ ( String.repeat (model.world.width + 1) "-" ) ++ "+" ]
    |> String.join (String.fromList ['\n'])


worldLineAsString : Model -> Int -> String
worldLineAsString model y = "|" ++ (List.range 0 (model.world.width) |> List.map (worldCellAsString model y) |> String.fromList) ++ "|"


worldCellAsString : Model -> Int -> Int -> Char
worldCellAsString { snake, apple } y x =
    let
        point = { x = x, y = y }
    in
        if apple |> Maybe.map ((==) point) |> Maybe.withDefault False
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
                    else if newHead.x < 0 || newHead.x > world.width || newHead.y < 0 || newHead.y > world.height
                    then
                        Fail snake
                    else if apple |> Maybe.map ((==) newHead) |> Maybe.withDefault False
                    then
                        Apple (newHead :: snake)
                    else
                        Move (newHead :: newTail)
            Nothing ->
                Fail snake


generateApple : World -> Cmd Msg
generateApple { width, height } =
    Random.pair (Random.int 0 width) (Random.int 0 height)
    |> Random.map (\(x, y) -> { x = x, y = y })
    |> Random.generate NewApple
