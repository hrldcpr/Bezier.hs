
import Text.Printf

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Bezier

bezier2d ps t =
  let [x, y] = bezier [[x, y] | (x, y) <- ps] t
  in (x, y)

data World = World {points :: [Point], time :: Float, depth :: Int, verbose :: Bool, animating :: Bool}

initialWorld = World [(-400, -400)] 0 0 False True

main =
  play (InWindow "Bézier" (1000, 1000) (0,  0))
       black 50 initialWorld
       picture touched next

next dt (World ps t d v True) = World ps (t + dt) d v True
next _ world = world

touched (EventKey (MouseButton LeftButton) Down _ p) (World ps t d v a) = World (ps ++ [p]) t d v a
touched (EventKey (SpecialKey KeyUp) Down _ _) (World ps t d v a) = World ps t (d + 1) v a
touched (EventKey (SpecialKey KeyDown) Down _ _) (World ps t d v a) = World ps t (d - 1) v a
touched (EventKey (Char 'v') Down _ _) (World ps t d v a) = World ps t d (not v) a
touched (EventKey (SpecialKey KeySpace) Down _ _) (World ps t d v a) = World ps t d v (not a)
touched (EventKey (SpecialKey KeyDelete) Down _ _) (World ps t d v a) = World (init ps) t d v a
touched _ world = world

dim' c = let
    k = 1.5
    (r, g, b, a) = rgbaOfColor c
  in
     makeColor (r / k) (g / k) (b / k) a

picture (World ps t d v a) = Pictures [
      go blue green d ps,
      Color white $ Line $ map c ts,
      Color white $ Pictures $ [Translate x y $ ThickCircle w r | (x, y) <- ps],
      Color green $ Translate 0 300 $ Scale 0.3 0.3 $ Text $ printf "t=%.1f" t',
      Color green $ Translate cx cy $ ThickCircle w r
    ]
  where
    w = 2
    r = 5
    c = bezier2d ps
    ts = [0, 0.01 .. 1]
    t' = (1 + cos t) / 2
    (cx, cy) = c t'
    go _ _ 0 _ = Blank
    go _ _ _ [_] = Blank
    go curveColor lineColor d ps = let
        go' = go (dim' curveColor) (dim' lineColor) (d - 1)
        a = bezier2d (init ps)
        b = bezier2d (tail ps)
        (ax, ay) = a t'
        (bx, by) = b t'
      in Pictures [
          go' (init ps),
          go' (tail ps),
          Color lineColor $ Line [a t', b t'],
          if (v || d == 1 || length ps <= 3) then (Color curveColor $ Line $ map a ts) else Blank,
          if (v || d == 1 || length ps <= 3) then (Color curveColor $ Line $ map b ts) else Blank,
          Color lineColor $ Translate ax ay $ ThickCircle w r,
          Color lineColor $ Translate bx by $ ThickCircle w r,
          Color lineColor $ Translate ((1 - t')*ax + t'*bx) ((1 - t')*ay + t'*by) $ ThickCircle w r
        ]
