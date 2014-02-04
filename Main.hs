
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Bezier

bezier2d ps t =
  let [x, y] = bezier [[x, y] | (x, y) <- ps] t
  in (x, y)

data World = World {points :: [Point], time :: Float, depth :: Int}

initialWorld = World [(-400, -400), (-400, 400)] 0 0

main =
  play (InWindow "Bézier" (1000, 1000) (0,  0))
       black 50 initialWorld
       picture touched next

next dt (World ps t d) = World ps (t + dt) d

touched (EventKey (MouseButton LeftButton) Down _ p) (World ps t d) = World (ps ++ [p]) t d
touched (EventKey (SpecialKey KeyUp) Down _ _) (World ps t d) = World ps t (d + 1)
touched (EventKey (SpecialKey KeyDown) Down _ _) (World ps t d) = World ps t (d - 1)
touched _ world = world

dim' c = let
    k = 1.5
    (r, g, b, a) = rgbaOfColor c
  in
     makeColor (r / k) (g / k) (b / k) a

picture (World ps t d) = Pictures [
      go green blue d ps,
      Color white $ Line $ map c ts,
      Color white $ Pictures $ [Translate x y $ ThickCircle w r | (x, y) <- ps]
    ]
  where
    w = 2
    r = 5
    c = bezier2d ps
    ts = [0, 0.01 .. 1]
    t' = (1 + cos t) / 2
    go _ _ 0 _ = Blank
    go _ _ _ [_] = Blank
    go white blue d ps = let
        go' = go (dim' white) (dim' blue) (d - 1)
        a = bezier2d (init ps)
        b = bezier2d (tail ps)
        (ax, ay) = a t'
        (bx, by) = b t'
      in Pictures [
          go' (init ps),
          go' (tail ps),
          Color white $ Line $ map a ts,
          Color white $ Line $ map b ts,
          Color blue $ Line [a t', b t'],
          Color blue $ Translate ((1 - t')*ax + t'*bx) ((1 - t')*ay + t'*by) $ ThickCircle w r
        ]
