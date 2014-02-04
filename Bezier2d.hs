module Bezier2d (bezier) where

interpolate a b t = (1 - t)*a + t*b

line (px, py) (qx, qy) t =
  (interpolate px qx t, interpolate py qy t)

bezier [p] t = p
bezier ps  t = line (bezier (init ps) t)
                    (bezier (tail ps) t)
                    t
