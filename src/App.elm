module App exposing (..)
import Array exposing (Array)
import Color
import Html exposing (Html, div, text, program, pre)
import Collage
import Keyboard
import Time exposing (Time, second, millisecond)
import Random
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Dict exposing (Dict)
import Element exposing (Element)

-- CONSTANTS

-- MODEL


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


init : ( Model, Cmd Msg )
init =
    let
        world = { width = 26, height = 26 }
    in
    ( { keyCode = 0
      , direction = Right
      , time = 0
      , world = world
      , snake = [(world.width // 2, world.height // 2)]
      , gameOver = False
      , apple = Nothing
      , rp = getRenderParams world
      }, generateApple world )



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
           , renderWorldPre model
           ]

gameView model =
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row [ Row.centerXl ]
              [ Grid.col [] [ text "1 of 3"]
              , Grid.col [ Col.xs8, Col.middleLg] [ renderWorldCollage model ]
              , Grid.col [] [ text "3 of 3"]
              ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg code ->
            let
                direction =
                    toDirection code
                    |> Maybe.andThen (changeDirection model.snake)
                    |> Maybe.withDefault model.direction
            in
                ( { model | keyCode = code, direction = direction }, Cmd.none )
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
--    if model.gameOver
--    then Sub.none
--    else
        Sub.batch
            [ Keyboard.downs KeyMsg
            , Time.every (50 * Time.millisecond) Tick
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


changeDirection : Snake -> Direction -> Maybe Direction
changeDirection snake direction =
    case snake of
        (h::h2::_) ->
            if movePoint direction h == h2
            then Nothing
            else Just direction
        (h::_) -> Just direction
        [] -> Debug.log "changeDirection: Empty snake" Nothing


renderWorldPre : Model -> Html msg
renderWorldPre model = pre [] [worldAsString model |> text]


getRenderParams : World -> RenderParams
getRenderParams world =
    let
        borderThicknessRatio = 0.05
        collage = { width = 600, height = 600 }
        board = { width = collage.width * (1 - 2 * borderThicknessRatio), height = collage.height * (1 - 2 * borderThicknessRatio) }
        unit = { width = board.width / toFloat world.width, height = board.height / toFloat world.height }
    in
        { collage = collage
        , board = board
        , unit = unit
        , borderThicknessRatio = borderThicknessRatio
        }

renderWorldCollage : Model -> Html msg
renderWorldCollage model =
    let
        rp = model.rp
    in
        Collage.collage (ceiling rp.collage.width) (ceiling rp.collage.height) [
            Collage.group
                [ renderWorldBorders model
                , renderSnake model
                , renderApple model
                ]
        ] |> Element.toHtml

renderWorldBorders : Model -> Collage.Form
renderWorldBorders model =
    let
        rp = model.rp
        w = rp.collage.width
        h = rp.collage.height
        sw = w * rp.borderThicknessRatio
        sh = h * rp.borderThicknessRatio
        ox = w / 2 - sw / 8
        oy = h / 2 - sh / 8
        color = Color.black
    in
        Collage.group
        [ Collage.rect sw h |> Collage.filled color |> Collage.moveX -ox
        , Collage.rect w sh |> Collage.filled color |> Collage.moveY oy
        , Collage.rect sw h |> Collage.filled color |> Collage.moveX ox
        , Collage.rect w sh |> Collage.filled color |> Collage.moveY -oy
        ]


renderSnake : Model -> Collage.Form
renderSnake model =
    let
        rp = model.rp
        pointToBoard (x, y) = ((toFloat x - toFloat model.world.width / 2) * rp.unit.width, -(toFloat y - toFloat model.world.height / 2) * rp.unit.height)
        move (p, shape) = Collage.move (pointToBoard p) shape
        filled = Collage.filled Color.red
        snake = model.snake
        snakePieces = getSnakePieces snake
        snakePieceShapes = List.map (renderSnakePiece model) snakePieces
        snakeForms = snakePieceShapes |> List.map filled |> zip snake |> List.map move
    in
    Collage.group snakeForms


renderSnakePiece : Model -> SnakePiece -> Collage.Shape
renderSnakePiece model snakePiece =
    let
        rp = model.rp
        ratio = 0.8
        x = rp.unit.width
        rx = x / 2
        dx = rx * (1 - ratio)
        x1 = -rx
        x2 = x1 + dx
        x4 = rx
        x3 = x4 - dx
        y = rp.unit.height
        ry = y / 2
        dy = ry * (1 - ratio)
        y1 = -ry
        y2 = y1 + dy
        y4 = ry
        y3 = y4 - dy
        arc cx cy hw hh rad arcLength =
            let
                n = ceiling (50 * 2 * pi / arcLength)
                f i = ( cx + hw * cos (rad + arcLength * toFloat i / toFloat n)
                      , cy + hh * sin (rad + arcLength * toFloat i / toFloat n)
                      )
            in
                List.map f (List.range 0 (n - 1))
    in
        case snakePiece of
            Body Left Right -> Collage.polygon [(x1, y2), (x4, y2), (x4, y3), (x1, y3)]
            Body Right Left -> Collage.polygon [(x1, y2), (x4, y2), (x4, y3), (x1, y3)]
            Body Up Down    -> Collage.polygon [(x2, y1), (x3, y1), (x3, y4), (x2, y4)]
            Body Down Up    -> Collage.polygon [(x2, y1), (x3, y1), (x3, y4), (x2, y4)]
            Body Left Down  -> Collage.polygon [(x2, y1), (x3, y1), (x3, y3), (x1, y3), (x1, y2), (x2, y2)]
            Body Down Left  -> Collage.polygon [(x2, y1), (x3, y1), (x3, y3), (x1, y3), (x1, y2), (x2, y2)]
            Body Right Down -> Collage.polygon [(x2, y1), (x3, y1), (x3, y2), (x4, y2), (x4, y3), (x2, y3)]
            Body Down Right -> Collage.polygon [(x2, y1), (x3, y1), (x3, y2), (x4, y2), (x4, y3), (x2, y3)]
            Body Up Left    -> Collage.polygon [(x1, y2), (x3, y2), (x3, y4), (x2, y4), (x2, y3), (x1, y3)]
            Body Left Up    -> Collage.polygon [(x1, y2), (x3, y2), (x3, y4), (x2, y4), (x2, y3), (x1, y3)]
            Body Up Right   -> Collage.polygon [(x2, y2), (x4, y2), (x4, y3), (x3, y3), (x3, y4), (x2, y4)]
            Body Right Up   -> Collage.polygon [(x2, y2), (x4, y2), (x4, y3), (x3, y3), (x3, y4), (x2, y4)]
            Head Left       -> (x1, y3) :: (x1, y2) :: arc 0 0 (rx * ratio) (ry * ratio) (-pi / 2) pi |> Collage.polygon
            Head Right      -> (x4, y2) :: (x4, y3) :: arc 0 0 (rx * ratio) (ry * ratio) (pi / 2) pi |> Collage.polygon
            Head Up         -> (x3, y4) :: (x2, y4) :: arc 0 0 (rx * ratio) (ry * ratio) pi pi |> Collage.polygon
            Head Down       -> (x2, y1) :: (x3, y1) :: arc 0 0 (rx * ratio) (ry * ratio) 0 pi |> Collage.polygon
            Tail Left       -> (x1, y3) :: (x1, y2) :: arc 0 0 (rx * ratio) (ry * ratio) (-pi / 2) pi |> Collage.polygon
            Tail Right      -> (x4, y2) :: (x4, y3) :: arc 0 0 (rx * ratio) (ry * ratio) (pi / 2) pi |> Collage.polygon
            Tail Up         -> (x3, y4) :: (x2, y4) :: arc 0 0 (rx * ratio) (ry * ratio) pi pi |> Collage.polygon
            Tail Down       -> (x2, y1) :: (x3, y1) :: arc 0 0 (rx * ratio) (ry * ratio) 0 pi |> Collage.polygon
            Egg -> Collage.polygon [(x2, y2), (x3, y2), (x3, y3), (x2, y3)]
            default -> Debug.log (toString snakePiece) (Collage.polygon [(x2, y2), (x3, y2), (x3, y3), (x2, y3)])

renderApple : Model -> Collage.Form
renderApple model =
    let
        rp = model.rp
        pointToBoard (x, y) = ((toFloat x - toFloat model.world.width / 2) * rp.unit.width, -(toFloat y - toFloat model.world.height / 2) * rp.unit.height)
        apple p = Collage.oval (rp.unit.width * 0.8) (rp.unit.height * 0.8) |> Collage.filled Color.green |> Collage.move (pointToBoard p)
    in
        Collage.group (case model.apple of
            Just p -> [ apple p ]
            Nothing -> []
        )


worldAsString : Model -> String
worldAsString model =
    let
        snakeDict = getSnakeCharacters model.snake
    in
        List.range 0 (model.world.height)
        |> List.map (worldLineAsString model snakeDict)
        |> (::) ( "+" ++ ( String.repeat (model.world.width + 1) "-" ) ++ "+" )
        |> flip (++) [ "+" ++ ( String.repeat (model.world.width + 1) "-" ) ++ "+" ]
        |> String.join (String.fromList ['\n'])


worldLineAsString : Model -> Dict Point Char -> Int -> String
worldLineAsString model charDict y = "|" ++ (List.range 0 (model.world.width) |> List.map (worldCellAsString model charDict y) |> String.fromList) ++ "|"


worldCellAsString : Model -> Dict Point Char -> Int -> Int -> Char
worldCellAsString { snake, apple } charDict y x =
    let
        point = (x, y)
    in
        if apple |> Maybe.map ((==) point) |> Maybe.withDefault False
            then ''
        else
            case Dict.get point charDict of
                Just c -> c
                Nothing -> ' '


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


{-|
    ╔═══►
    ╚═══╗
    ╔══╗║
    ║╔╗ ║
    ╚╝╚═╝
-}
getSnakeCharacters : Snake -> Dict Point Char
getSnakeCharacters snake =
    snake
    |> zipListWithNeighbours
    |> List.map ( \(mh, c, mt) -> (c, getSnakeCharacter (getSnakePiece mh c mt)) )
    |> Dict.fromList


getSnakePieces : Snake -> List SnakePiece
getSnakePieces snake =
    zipListWithNeighbours snake |> List.map ( \(mh, c, mt) -> (getSnakePiece mh c mt) )


getSnakePiece : Maybe Point -> Point -> Maybe Point -> SnakePiece
getSnakePiece maybeHead (cx, cy) maybeTail =
    case (maybeHead, maybeTail) of
          (Just (hx, hy), Just (tx, ty)) ->
              case ((tx - cx, ty - cy), (hx - cx, hy - cy)) of
                  ((-1, 0), (1, 0)) -> Body Left Right    -- ═
                  ((-1, 0), (0, -1)) -> Body Left Up      -- ╝
                  ((-1, 0), (0, 1)) -> Body Left Down     -- ╗
                  ((0, -1), (0, 1)) -> Body Up Down       -- ║
                  ((0, -1), (-1, 0)) -> Body Up Left      -- ╝
                  ((0, -1), (1, 0)) -> Body Up Right      -- ╚
                  ((1, 0), (-1, 0)) -> Body Right Left    -- ═
                  ((1, 0), (0, -1)) -> Body Right Up      -- ╚
                  ((1, 0), (0, 1)) -> Body Right Down     -- ╔
                  ((0, 1), (0, -1)) -> Body Down Up       -- ║
                  ((0, 1), (-1, 0)) -> Body Down Left     -- ╗
                  ((0, 1), (1, 0)) -> Body Down Right     -- ╔
                  default -> Debug.log (toString ((tx - cx, ty - cy), (hx - cx, hy - cy))) Egg
          (Nothing, Just (tx, ty)) ->
              case (tx - cx, ty - cy) of
                  (-1, 0) -> Head Left    -- ═
                  (1, 0) -> Head Right    -- ═
                  (0, -1) -> Head Up      -- ║
                  (0, 1) -> Head Down     -- ║
                  default -> Debug.log (toString (tx - cx, ty - cy)) Egg
          (Just (hx, hy), Nothing) ->
              case (hx - cx, hy - cy) of
                  (-1, 0) -> Tail Left    -- ═
                  (1, 0) -> Tail Right    -- ═
                  (0, -1) -> Tail Up      -- ║
                  (0, 1) -> Tail Down     -- ║
                  default -> Debug.log (toString (hx - cx, hy - cy)) Egg
          (Nothing, Nothing) -> Egg


getSnakeCharacter : SnakePiece -> Char
getSnakeCharacter snakePiece =
    case snakePiece of
        Body Left Right  -> '═'
        Body Left Up     -> '╝'
        Body Left Down   -> '╗'
        Body Up Down     -> '║'
        Body Up Left     -> '╝'
        Body Up Right    -> '╚'
        Body Right Left  -> '═'
        Body Right Up    -> '╚'
        Body Right Down  -> '╔'
        Body Down Up     -> '║'
        Body Down Left   -> '╗'
        Body Down Right  -> '╔'
        Head Left -> '═'
        Head Right -> '═'
        Head Up   -> '║'
        Head Down  -> '║'
        Tail Left -> '═'
        Tail Right -> '═'
        Tail Up   -> '║'
        Tail Down  -> '║'
        default-> '@'


zip : List a -> List b -> List (a, b)
zip l1 l2 =
    case (l1, l2) of
        (h1::t1, h2::t2) -> (h1, h2) :: (zip t1 t2)
        default -> []


zip3 : List a -> List b -> List c -> List (a, b, c)
zip3 l1 l2 l3 =
    case (l1, l2, l3) of
        (h1::t1, h2::t2, h3::t3) -> (h1, h2, h3) :: (zip3 t1 t2 t3)
        default -> []


zipListWithNeighbours : List a -> List (Maybe a, a, Maybe a)
zipListWithNeighbours list =
    let
        justs = List.map (Just) list
        fronts = Nothing :: justs
        backs = List.drop 1 (justs ++ [ Nothing ])
    in
        zip3 fronts list backs


zmap : List a -> (a -> b) -> List (a, b)
zmap list f = zip list (List.map f list)
