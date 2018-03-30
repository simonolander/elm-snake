module Snake exposing (..)

import Model exposing (..)
import Keyboard
import Random

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


changeDirection : Snake -> Direction -> Maybe Direction
changeDirection snake direction =
    case snake of
        (h::h2::_) ->
            if movePoint direction h == h2
            then Nothing
            else Just direction
        (h::_) -> Just direction
        [] -> Debug.log "changeDirection: Empty snake" Nothing


movePoint : Direction -> Point -> Point
movePoint dir (x, y) =
    case dir of
        Left -> (x - 1, y)
        Up -> (x, y - 1)
        Right -> (x + 1, y)
        Down -> (x, y + 1)


moveSnake : Model -> SnakeMove
moveSnake {world, snake, direction, apple} =
    let
        maybeHead = List.head snake
    in
        case maybeHead of
            Just head ->
                let
                    newHead = movePoint direction head
                    (x, y) = newHead
                    newTail = List.take (List.length snake - 1) snake
                in
                    if List.member newHead newTail
                    then
                        Fail snake
                    else if x < 0 || x > world.width || y < 0 || y > world.height
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
    |> Random.generate NewApple
