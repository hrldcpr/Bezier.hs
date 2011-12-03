module Bezier (bezier) where

-- linear interpolation between two points of arbitrary dimension:
interpolate a b t =
  map interpolate' (zip a b)
  where interpolate' (a', b') = (1 - t)*a' + t*b'

-- bezier curve of just one point is fixed at that point:
bezier [point] t = point
-- bezier curve of a list of points is just linear interpolation
-- between bezier curve of the initial part of the list and bezier
-- curve of the tail of the list:
bezier points  t =
  interpolate (bezier (init points) t)
              (bezier (tail points) t)
              t
