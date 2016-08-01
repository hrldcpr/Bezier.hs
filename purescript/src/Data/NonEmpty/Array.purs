module Data.NonEmpty.Array where

import Prelude
import Data.Array (uncons, (:))
import Data.Array (init, zipWith) as Array
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, tail, (:|))
import Data.Traversable (sequence)


fromArray :: forall a. Array a -> Maybe (NonEmpty Array a)
fromArray xs = case uncons xs of
  Nothing -> Nothing
  Just {head: x, tail: xs} -> Just (x :| xs)

toArray :: forall a. NonEmpty Array a -> Array a
toArray (x :| xs) = x : xs

forArray :: forall a b. (NonEmpty Array a -> b) -> Array a -> Maybe b
forArray f = fromArray >>> map f

zipWith :: forall a b c. (a -> b -> c) -> NonEmpty Array a -> NonEmpty Array b -> NonEmpty Array c
zipWith f (x:|xs) (y:|ys) = f x y :| Array.zipWith f xs ys

zipWithA :: forall f a b c. Applicative f => (a -> b -> f c) -> NonEmpty Array a -> NonEmpty Array b -> f (NonEmpty Array c)
zipWithA f xs ys = sequence $ zipWith f xs ys

init :: forall a. NonEmpty Array a -> Array a
init (x :| xs) = case Array.init xs of
  Nothing -> []
  Just xs_init -> x : xs_init

tail' :: forall a. NonEmpty Array a -> Maybe (NonEmpty Array a)
tail' = tail >>> fromArray

init' :: forall a. NonEmpty Array a -> Maybe (NonEmpty Array a)
init' = init >>> fromArray
