module Bezier (bezier) where

import Control.Applicative

type Time = Float
type Point = [Float]

-- bezier of one point is fixed at that point, and bezier of N points is the linear
-- interpolation between bezier of first N-1 points and bezier of last N-1 points:
bezier :: [Point] -> Time -> Point
bezier [p] = const p
bezier ps  = interpolate <*> (bezier $ init ps) <*> (bezier $ tail ps)

-- linear interpolation between two points:
interpolate :: Time -> Point -> Point -> Point
interpolate t p q = zipWith go p q
  where go a b = (1 - t)*a + t*b
