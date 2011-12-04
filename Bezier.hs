module Bezier (bezier) where

-- point of arbitrary dimension is just a list of coordinates:
type Point = []

-- parametric line between two points:
line :: Num a => Point a -> Point a -> a -> Point a
line p q t = map interpolate (zip p q)
  where interpolate (a1, a2) = (1 - t)*a1 + t*a2

-- bezier of just one point is fixed at that point,
-- and bezier of a list of points is just linear interpolation between
-- bezier of the initial part of the list and bezier of the tail of
-- the list:
bezier :: Num a => [Point a] -> a -> Point a
bezier [p] t = p
bezier ps  t = line (bezier (init ps) t)
                    (bezier (tail ps) t)
                    t
