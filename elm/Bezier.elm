module Bezier (Point, bezier) where

import Time exposing (Time)

import Cons exposing (Cons)


type alias Point = (Float, Float)


bezier : Cons Point -> Time -> Point
bezier points time =
  case Cons.tail' points of
    Nothing ->
      Cons.head points
    Just tail ->
      let
        line1d a b = (1-time)*a + time*b
        line (px, py) (qx, qy) = (line1d px qx, line1d py qy)
       in
         bezier (Cons.map2 line points tail) time
