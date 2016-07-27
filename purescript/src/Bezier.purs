module Bezier where

import Prelude
import Data.Array (zipWithA) as Array
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, head)

import Data.NonEmpty.Array (forArray, tail', zipWithA)


type Point = Array Number  -- a multi-dimensional coordinate
type Parametric a = Number -> a  -- a value that varies over time

-- linear interpolation between two numbers, from t=0 to t=1
line1d :: Number -> Number -> Parametric Number
line1d a b = \t -> (1.0 - t)*a + t*b

-- line between two points is linear interpolation on each dimension
line :: Point -> Point -> Parametric Point
line p q = Array.zipWithA line1d p q

-- bezier of one point is fixed at that point, and at time t,
-- bezier of n points is bezier of the points at time t along
-- the n-1 lines between the points
bezier :: NonEmpty Array Point -> Parametric Point
bezier ps = case tail' ps of
  Nothing -> pure $ head ps
  Just tail -> bezier =<< zipWithA line ps tail

maybezier :: Array Point -> Maybe (Parametric Point)
maybezier = forArray bezier
