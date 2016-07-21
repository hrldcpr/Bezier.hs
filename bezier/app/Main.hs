import Text.Printf

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Bezier hiding (Point)

bezier2d :: [Point] -> Parametric Point
bezier2d ps = point2d <$> bezier (list2d <$> ps)
  where point2d [x, y] = (x, y)
        list2d  (x, y) = [x, y]

data World = World {points :: [Point], time :: Float, depth :: Int, verbose :: Bool, animating :: Bool}
initialWorld :: World
initialWorld = World {points=[(-200, -200)], time=0, depth=0, verbose=False, animating=True}

main :: IO ()
main = play (InWindow "Bézier" (600, 600) (0,  0))
       black 50 initialWorld
       picture touched next

next :: Float -> World -> World
next dt world | animating world = world {time=(time world) + dt}
              | otherwise = world

touched :: Event -> World -> World
touched (EventKey (MouseButton LeftButton) Down _ p) world = world {points=(points world) ++ [p]}
touched (EventKey (SpecialKey KeyUp) Down _ _) world = world {depth=(depth world) + 1}
touched (EventKey (SpecialKey KeyDown) Down _ _) world = world {depth=(depth world) - 1}
touched (EventKey (Char 'v') Down _ _) world = world {verbose=not (verbose world)}
touched (EventKey (SpecialKey KeySpace) Down _ _) world = world {animating=not (animating world)}
touched (EventKey (SpecialKey KeyDelete) Down _ _) world = world {points=init (points world)}
touched _ world = world

dim' :: Color -> Color
dim' c = makeColor (r / k) (g / k) (b / k) a
  where (r, g, b, a) = rgbaOfColor c
        k = 1.5

picture :: World -> Picture
picture world = Pictures [
      go blue green (depth world) (points world),
      Color white $ Line $ c <$> ts,
      Color white $ Pictures $ [Translate x y $ ThickCircle w r | (x, y) <- points world],
      Color green $ Translate 0 200 $ Scale 0.3 0.3 $ Text $ printf "t=%.1f" t,
      Color green $ Translate cx cy $ ThickCircle w r
    ]
  where
    w = 2
    r = 5
    c = bezier2d $ points world
    ts = [0, 0.01 .. 1]
    t = (1 + cos (time world)) / 2
    (cx, cy) = c t
    go _ _ 0 _ = Blank
    go _ _ _ [_] = Blank
    go curveColor lineColor d ps = let
        go' = go (dim' curveColor) (dim' lineColor) (d - 1)
        a = bezier2d (init ps)
        b = bezier2d (tail ps)
        (ax, ay) = a t
        (bx, by) = b t
      in Pictures [
          go' (init ps),
          go' (tail ps),
          Color lineColor $ Line [a t, b t],
          if (verbose world || d == 1 || length ps <= 3) then (Color curveColor $ Line $ a <$> ts) else Blank,
          if (verbose world || d == 1 || length ps <= 3) then (Color curveColor $ Line $ b <$> ts) else Blank,
          Color lineColor $ Translate ax ay $ ThickCircle w r,
          Color lineColor $ Translate bx by $ ThickCircle w r,
          Color lineColor $ Translate (line1d ax bx t) (line1d ay by t) $ ThickCircle w r
        ]
