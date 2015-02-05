module Bezier (bezier) where

type Point = [Float]
type Curve = Float -> Point

-- bezier of one point is fixed at that point, and bezier of N points is interpolation
-- between bezier of first N-1 points and bezier of last N-1 points:
bezier :: [Point] -> Curve
bezier [p] = const p
bezier ps  = interpolation (bezier $ init ps) (bezier $ tail ps)

-- linear interpolation between two curves:
-- TODO make this point-free, a la http://stackoverflow.com/questions/4333864/trick-for-reusing-arguments-in-haskell
interpolation :: Curve -> Curve -> Curve
interpolation f g t = zipWith go (f t) (g t)
  where go a b = (1 - t)*a + t*b
