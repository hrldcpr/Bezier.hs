module Bezier (bezier) where

import Control.Monad (zipWithM)

type Point = [Float]
type Parametric a = Float -> a

-- bezier of one point is fixed at that point, and bezier of N points is the linear
-- interpolation between bezier of first N-1 points and bezier of last N-1 points:
bezier :: [Point] -> Parametric Point
bezier [p] = const p
bezier ps  = do l <- bezier (init ps)
                r <- bezier (tail ps)
                line l r

-- line between two points:
line :: Point -> Point -> Parametric Point
line p q = zipWithM line1d p q

-- linear interpolation between two numbers:
line1d :: Float -> Float -> Parametric Float
line1d a b = \t -> (1 - t)*a + t*b
