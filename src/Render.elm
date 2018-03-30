module Render exposing (render, getRenderParams)

import Collage
import Color
import Element
import Html
import Model exposing (..)
import Text
import Util exposing (..)


getRenderParams : World -> RenderParams
getRenderParams world =
    let
        borderThicknessRatio = 0.0125
        collage = { width = 600, height = 600 }
        board = { width = collage.width * (1 - 2 * borderThicknessRatio), height = collage.height * (1 - 2 * borderThicknessRatio) }
        unit = { width = board.width / toFloat (world.width + 1), height = board.height / toFloat (world.height + 1) }
    in
        { collage = collage
        , board = board
        , unit = unit
        , borderThicknessRatio = borderThicknessRatio
        }


render : Model -> Html.Html msg
render model =
    let
        rp = model.rp
        borders = renderWorldBorders model
        snake = renderSnake model
        apple = renderApple model
        text =
            case model.gameState of
                NotStarted -> renderText "elm-snake" "Press SPACE to begin"
                GameOver -> renderText "Game Over" "Press SPACE to restart"
                Paused -> renderText "Paused" "Press SPACE to resume"
                EnterName -> renderText "Enter Name" ("> " ++ model.scoreboard.currentScore.name ++ "_")
                Running -> Collage.group []
        collage =
            Collage.collage
                (ceiling rp.collage.width)
                (ceiling rp.collage.height)
                [ Collage.group
                    [ renderWorldBorders model
                    , renderSnake model
                    , renderApple model
                    , text
                    ]
                ]
    in
        Element.toHtml collage


renderText : String -> String -> Collage.Form
renderText title subTitle =
    let
        titleHeight = 40
        subTitleHeight = 20
        titleText = Text.fromString title
            |> Text.height titleHeight
            |> Text.monospace
        subTitleText = Text.fromString subTitle
            |> Text.height subTitleHeight
            |> Text.monospace
    in
        Collage.group
            [ titleText |> Collage.text |> Collage.moveY ( titleHeight / 2 )
            , subTitleText |> Collage.text |> Collage.moveY ( -(titleHeight / 2) )
            ]



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
        color = Color.lightCharcoal
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
            Body Left Down  -> (x1, y3) :: (x1, y2) :: (x2, y2) :: (x2, y1) :: (x3, y1) :: arc 0 0 (rx * ratio) (rx * ratio) 0 (pi / 2) |> Collage.polygon
            Body Down Left  -> (x1, y3) :: (x1, y2) :: (x2, y2) :: (x2, y1) :: (x3, y1) :: arc 0 0 (rx * ratio) (rx * ratio) 0 (pi / 2) |> Collage.polygon
            Body Right Down -> (x2, y1) :: (x3, y1) :: (x3, y2) :: (x4, y2) :: (x4, y3) :: arc 0 0 (rx * ratio) (rx * ratio) (pi / 2) (pi / 2) |> Collage.polygon
            Body Down Right -> (x2, y1) :: (x3, y1) :: (x3, y2) :: (x4, y2) :: (x4, y3) :: arc 0 0 (rx * ratio) (rx * ratio) (pi / 2) (pi / 2) |> Collage.polygon
            Body Up Left    -> (x3, y4) :: (x2, y4) :: (x2, y3) :: (x1, y3) :: (x1, y2) :: arc 0 0 (rx * ratio) (rx * ratio) (-pi / 2) (pi / 2) |> Collage.polygon
            Body Left Up    -> (x3, y4) :: (x2, y4) :: (x2, y3) :: (x1, y3) :: (x1, y2) :: arc 0 0 (rx * ratio) (rx * ratio) (-pi / 2) (pi / 2) |> Collage.polygon
            Body Up Right   -> (x4, y2) :: (x4, y3) :: (x3, y3) :: (x3, y4) :: (x2, y4) :: arc 0 0 (rx * ratio) (rx * ratio) pi (pi / 2) |> Collage.polygon
            Body Right Up   -> (x4, y2) :: (x4, y3) :: (x3, y3) :: (x3, y4) :: (x2, y4) :: arc 0 0 (rx * ratio) (rx * ratio) pi (pi / 2) |> Collage.polygon
            Head Left       -> (x1, y3) :: (x1, y2) :: arc 0 0 (rx * ratio) (ry * ratio) (-pi / 2) pi |> Collage.polygon
            Head Right      -> (x4, y2) :: (x4, y3) :: arc 0 0 (rx * ratio) (ry * ratio) (pi / 2) pi |> Collage.polygon
            Head Up         -> (x3, y4) :: (x2, y4) :: arc 0 0 (rx * ratio) (ry * ratio) pi pi |> Collage.polygon
            Head Down       -> (x2, y1) :: (x3, y1) :: arc 0 0 (rx * ratio) (ry * ratio) 0 pi |> Collage.polygon
            Tail Left       -> (x1, y3) :: (x1, y2) :: arc 0 0 (rx * ratio) (ry * ratio) (-pi / 2) pi |> Collage.polygon
            Tail Right      -> (x4, y2) :: (x4, y3) :: arc 0 0 (rx * ratio) (ry * ratio) (pi / 2) pi |> Collage.polygon
            Tail Up         -> (x3, y4) :: (x2, y4) :: arc 0 0 (rx * ratio) (ry * ratio) pi pi |> Collage.polygon
            Tail Down       -> (x2, y1) :: (x3, y1) :: arc 0 0 (rx * ratio) (ry * ratio) 0 pi |> Collage.polygon
            Egg -> arc 0 0 (rx * ratio) (ry * ratio) 0 (2*pi) |> Collage.polygon
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
