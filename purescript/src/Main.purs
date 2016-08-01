module Main where

import Prelude
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Data.Array (range)
import Data.Foldable (class Foldable, for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe, maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Graphics.Canvas (CANVAS, CanvasElement, Context2D,
                        arc, clearRect, fillPath, getCanvasDimensions, getCanvasElementById,
                        getContext2D, lineTo, moveTo, setFillStyle, setStrokeStyle, strokePath)
import Math (cos, pi)

import Animation (ANIMATION, Milliseconds, requestAnimationFrame)
import Bezier (Point, bezier, line2d)
import Data.NonEmpty.Array (tail', zipWithA)


doMaybe :: forall eff a b. (a -> Eff eff b) -> Maybe a -> Eff eff Unit
doMaybe f = maybe (pure unit) (f >>> void)

maybeDo :: forall eff a b. Maybe a -> (a -> Eff eff b) -> Eff eff Unit
maybeDo = flip doMaybe

points :: NonEmpty Array Point
points = { x: 100.0, y: 500.0 } :| [{ x: 100.0, y: 100.0}, { x: 300.0, y: 100.0 }, { x: 500.0, y: 300.0}, { x: 400.0, y: 500.0 }]

steps :: Int -> Array Number
steps n = (\i -> toNumber i / toNumber n) <$> range 0 n

circle :: forall eff. Context2D -> { x :: Number, y :: Number, r :: Number } -> Eff (canvas :: CANVAS | eff) Unit
circle ctx { x, y, r } = void $ arc ctx { x, y, r, start: 0.0, end: 2.0 * pi }

line :: forall eff f. Foldable f => Context2D -> f Point -> Eff (canvas :: CANVAS | eff) Unit
line ctx ps = for_ ps \p -> do
  lineTo ctx p.x p.y
  moveTo ctx p.x p.y

draw :: CanvasElement -> Milliseconds
        -> Eff (animation :: ANIMATION, canvas :: CANVAS, console :: CONSOLE) Unit
draw canvas time = void do
  ctx <- getContext2D canvas

  { width, height } <- getCanvasDimensions canvas
  clearRect ctx { x: 0.0, y: 0.0, w: width, h: height }

  let t = (1.0 + cos (time / 1000.0)) / 2.0
      go ps = void $ do
        setStrokeStyle "orange" ctx
        strokePath ctx $ line ctx ps

        setStrokeStyle "cyan" ctx
        strokePath ctx $ line ctx $ bezier ps <$> steps 100

        setFillStyle "black" ctx
        for_ ps \p -> fillPath ctx $ circle ctx { x: p.x, y: p.y, r: 3.0 }

        maybeDo (tail' ps) \tl -> go $ zipWithA line2d ps tl $ t
  go points

  requestAnimationFrame $ draw canvas

main :: Eff (animation :: ANIMATION, canvas :: CANVAS, console :: CONSOLE) Unit
main = getCanvasElementById "canvas" >>= doMaybe (requestAnimationFrame <<< draw)
