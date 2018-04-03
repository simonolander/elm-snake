module Model exposing (..)

import Http
import Keyboard
import Time exposing (Time)


{-| The model type specifies how the state looks in our application.
    All the state needs to be in the model type for us to save it.
-}
type alias Model =
    { keyCode: Keyboard.KeyCode
    , direction: Direction
    , time: Time
    , snake: Snake
    , world: World
    , apple: Maybe Apple
    , rp: RenderParams
    , scoreboard: Scoreboard
    , gameState: GameState
    }


{-| The Msg type is super important. It determines what kinds of events can occur in the application that changes the model state.
    We have five kinds of messages.

    - KeyMsg is triggered when the player presses a key on the keyboard, and contains the key pressed.
    - Tick is triggered every n milliseconds, and contains the current timestamp.
    - NewApple is triggered when we have generated a new apple.
    - NewName is triggered when we have generated a new name.
    - ReceiveScores is triggered when we receive the scores from the server.
        It is either a list of scores or some kind of error
-}
type Msg
    = KeyMsg Keyboard.KeyCode
    | Tick Time
    | NewApple Apple
    | NewName String
    | ReceiveScores (Result Http.Error (List Score) )


{-| Types lets us define data bearers. They start with a tag and then maybe some data.
    Types can be recursive and/or generic, e.g.
        type List a = End a | More a (List a)
    which would be a fine type definition for a linked list.
        ["1", "2", "3"] == More "1" (More "2" (End "3"))
-}
type GameState = NotStarted | GameOver | Running | Paused | EnterName
type Direction = Left | Up | Right | Down
type SnakeMove = Apple Snake | Move Snake | Fail Snake
type SnakePiece = Head Direction
    | Tail Direction
    | Body Direction Direction
    | Egg


{-| Type aliases are not really types. They are just bundles of types or other names for a type.
-}
type alias Point = (Int, Int)
type alias Apple = Point
type alias Snake = List Point
type alias Dimension =
    { width: Float
    , height: Float
    }
type alias World =
    { width: Int
    , height: Int
    }
type alias Score =
    { score: Int
    , name: String
    }
type alias Scoreboard =
    { scores: List Score
    , currentScore: Score
    }
type alias RenderParams =
    { collage: Dimension
    , board: Dimension
    , unit: Dimension
    , borderThicknessRatio: Float
    }
