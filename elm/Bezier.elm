module Bezier (bezier) where

type alias Point = (Float, Float)
type alias Time = Float

bezier : List Point -> Time -> Maybe Point
bezier points time =
  let
    line1d a b = (1 - time)*a + time*b
    line (px, py) (qx, qy) = (line1d px qx, line1d py qy)
  in case points of
    [] ->
      Nothing
    [point] ->
      Just point
    _::tail ->
      bezier (List.map2 line points tail) time
