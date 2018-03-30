module Model exposing (..)

import Keyboard
import Time exposing (Time)

type Direction = Left | Up | Right | Down
type SnakeMove = Apple Snake | Move Snake | Fail Snake
type SnakePiece = Head Direction
    | Tail Direction
    | Body Direction Direction
    | Egg

type alias Dimension = { width: Float, height: Float}
type alias Point = (Int, Int)
type alias Apple = Point
type alias Snake = List Point
type alias World = { width: Int, height: Int }
type alias GameOver = Bool
type alias RenderParams =
    { collage: Dimension
    , board: Dimension
    , unit: Dimension
    , borderThicknessRatio: Float
    }

type alias Model =
    { keyCode: Keyboard.KeyCode
    , direction: Direction
    , time: Time
    , snake: Snake
    , world: World
    , gameOver: GameOver
    , apple: Maybe Apple
    , rp: RenderParams
    }

type Msg
    = KeyMsg Keyboard.KeyCode
    | Tick Time
    | NewApple Apple
