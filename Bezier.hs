module Bezier where

import Control.Monad (zipWithM)

type Point = [Float]  -- a multi-dimensional coordinate
type Parametric a = Float -> a  -- a value that varies over time

-- linear interpolation between two numbers, from t=0 to t=1
line1d :: Float -> Float -> Parametric Float
line1d a b = \t -> (1 - t)*a + t*b

-- line between two points is linear interpolation on each dimension
line :: Point -> Point -> Parametric Point
line p q = zipWithM line1d p q

-- bezier of one point is fixed at that point, and bezier of n points is the
-- line between bezier of first n-1 points and bezier of last n-1 points
bezier :: [Point] -> Parametric Point
bezier [p] = return p
bezier ps  = do p <- bezier (init ps)
                q <- bezier (tail ps)
                line p q
