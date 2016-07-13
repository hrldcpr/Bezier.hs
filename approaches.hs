
type Time = Float
type Point = [Float]


--
-- Simple
--

bezier :: [Point] -> Time -> Point
bezier [p] t = p
bezier ps  t = line (bezier (init ps) t) (bezier (tail ps) t) t

line :: Point -> Point -> Float -> Point
line p q = \t -> zipWith line1d p q
  where line1d a b = (1 - t)*a + t*b


--
-- Applicative Functor
--

import Control.Applicative (<*>)

type Parametric a = Float -> a

bezier :: [Point] -> Parametric Point
bezier [p] = const p
bezier ps  = line <*> (bezier (init ps)) <*> (bezier (tail ps))

line :: Parametric (Point -> Point -> Point)
line t p q = zipWith line1d p q
  where line1d a b = (1 - t)*a + t*b


--
-- Reader Monad
--

import Control.Monad.Reader

type Parametric a = Reader Float a

bezier :: [Point] -> Parametric Point
bezier [p] = return p
bezier ps  = do l <- bezier (init ps)
                r <- bezier (tail ps)
                line l r

line :: Point -> Point -> Parametric Point
line p q = zipWithM line1d p q

line1d :: Float -> Float -> Parametric Float
line1d a b = reader $ \t -> (1 - t)*a + t*b


--
-- Function Monad
--

import Control.Monad (zipWithM)

type Parametric a = Float -> a

bezier :: [Point] -> Parametric Point
bezier [p] = return p
bezier ps  = do l <- bezier (init ps)
                r <- bezier (tail ps)
                line l r

line :: Point -> Point -> Parametric Point
line p q = zipWithM line1d p q

line1d :: Float -> Float -> Parametric Float
line1d a b = \t -> (1 - t)*a + t*b


--
-- Applicative Function Monad
--

import Control.Monad (join, zipWithM)

type Parametric a = Float -> a

bezier :: [Point] -> Parametric Point
bezier [p] = const p
bezier ps  = join $ line <$> bezier (init ps) <*> bezier (tail ps)

line :: Point -> Point -> Parametric Point
line p q = zipWithM line1d p q

line1d :: Float -> Float -> Parametric Float
line1d a b = \t -> (1 - t)*a + t*b


--
-- De Casteljau, from Wikipedia
--

bezier :: [Point] -> Parametric Point
bezier [p] = return p
bezier ps = bezier =<< zipWithM line ps (tail ps)
