
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Bezier


bezier2d ps t =
  let [x, y] = bezier [[x, y] | (x, y) <- ps] t
  in (x, y)

data World = World [Point] Float

initialWorld = World [(-200, -200), (-200, 200)] 0

main =
  play (InWindow "Bézier" (600, 600) (0,  0))
       black 50 initialWorld
       picture touched next

next dt (World ps t) = World ps (t + dt)

touched (EventKey (MouseButton LeftButton) Down _ p) (World ps t) = World (ps ++ [p]) t
touched _ world = world

picture (World ps t) = let
    t' = (1 + cos t) / 2
    ts = [0, 0.01 .. 1]
    a = bezier2d (init ps)
    b = bezier2d (tail ps)
    c = bezier2d ps
    (ax, ay) = a t'
    (bx, by) = b t'
    w = 2
    r = 5
  in Color white $ Pictures $
     [Color green $ Line $ map a ts,
      Color red $ Line $ map b ts,
      Color blue $ Line [a t', b t'],
      Line $ map c ts,
      Pictures $ [Translate x y $ ThickCircle w r | (x, y) <- ps],
      Color green $ Translate ax ay $ ThickCircle w r,
      Color red $ Translate bx by $ ThickCircle w r,
      Color blue $ Translate ((1 - t')*ax + t'*bx) ((1 - t')*ay + t'*by) $ ThickCircle w r
     ]
