module Bezier (bezier) where

import Control.Applicative

type Point = [Float]
type Curve = Float -> Point

-- bezier of one point is fixed at that point, and bezier of N points is the linear
-- interpolation between bezier of first N-1 points and bezier of last N-1 points:
bezier :: [Point] -> Curve
bezier [p] = const p
bezier ps  = linterpolate <*> (bezier $ init ps) <*> (bezier $ tail ps)

-- linear interpolation between two points:
linterpolate :: Float -> Point -> Point -> Point
linterpolate t p q = zipWith go p q
  where go a b = (1 - t)*a + t*b
