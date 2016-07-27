module Data.NonEmpty.Array where

import Prelude
import Data.Array (uncons)
import Data.Array (zipWith) as Array
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (sequence)


fromArray :: forall a. Array a -> Maybe (NonEmpty Array a)
fromArray xs = case uncons xs of
  Nothing -> Nothing
  Just {head: x, tail: xs} -> Just (x :| xs)

forArray :: forall a b. (NonEmpty Array a -> b) -> Array a -> Maybe b
forArray f = fromArray >>> map f

zipWith :: forall a b c. (a -> b -> c) -> NonEmpty Array a -> NonEmpty Array b -> NonEmpty Array c
zipWith f (x:|xs) (y:|ys) = f x y :| Array.zipWith f xs ys

zipWithA :: forall f a b c. Applicative f => (a -> b -> f c) -> NonEmpty Array a -> NonEmpty Array b -> f (NonEmpty Array c)
zipWithA f xs ys = sequence $ zipWith f xs ys
