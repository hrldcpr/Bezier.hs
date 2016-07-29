module Main where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Data.Array
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Graphics.Canvas
import Math

import Animation
import Bezier (Parametric, Point, bezier)

points :: NonEmpty Array Point
points = { x: 0.0, y: 0.0 } :| [{ x: 300.0, y: 0.0}, { x: 300.0, y: 300.0}, { x: 300.0, y: 600.0}, { x: 600.0, y: 600.0 }]

curve :: Parametric Point
curve = bezier points

steps :: Int -> Array Number
steps n = (\i -> toNumber i / toNumber n) <$> range 0 n

draw :: CanvasElement -> Milliseconds
        -> Eff (animation :: ANIMATION, canvas :: CANVAS, console :: CONSOLE) Unit
draw canvas time = void do
  ctx <- getContext2D canvas

  { width, height } <- getCanvasDimensions canvas
  clearRect ctx { x: 0.0, y: 0.0, w: width, h: height }

  let ps = curve <$> steps 100
  strokePath ctx $ for_ ps \p -> do
    lineTo ctx p.x p.y
    moveTo ctx p.x p.y

  let t = (1.0 + cos (time / 1000.0)) / 2.0
      p = curve t
  setFillStyle "cyan" ctx
  fillPath ctx $ arc ctx { x: p.x, y: p.y, r: 3.0, start: 0.0, end: 2.0 * pi}

  requestAnimationFrame $ draw canvas

main :: Eff (animation :: ANIMATION, canvas :: CANVAS, console :: CONSOLE) Unit
main = do
  c <- getCanvasElementById "canvas"
  case c of
    Nothing -> pure unit
    Just canvas -> do
      requestAnimationFrame $ draw canvas
      pure unit
