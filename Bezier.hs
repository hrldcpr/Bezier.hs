module Bezier where

import Control.Monad (zipWithM)

-- a multi-dimensional coordinate:
type Point = [Float]

-- a value that varies over time:
type Parametric a = Float -> a

-- linear interpolation between two numbers, from t=0 to t=1:
line1d :: Float -> Float -> Parametric Float
line1d a b = \t -> (1 - t)*a + t*b

-- line between two points is linear interpolation on each dimension:
line :: Point -> Point -> Parametric Point
line p q = zipWithM line1d p q

-- bezier of one point is fixed at that point, and bezier of n points is the
-- line between bezier of first n-1 points and bezier of last n-1 points:
bezier :: [Point] -> Parametric Point
bezier [p] = const p
bezier ps  = do p <- bezier (init ps)
                q <- bezier (tail ps)
                line p q
