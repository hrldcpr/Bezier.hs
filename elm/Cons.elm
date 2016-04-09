module Cons
  ( Cons
  , map
  ) where


import List


type Cons a = Cons a (List a)


-- Contructor and accessors

cons : a -> List a -> Cons a
cons = Cons

head : Cons a -> a
head (Cons head _) = head

tail : Cons a -> List a
tail (Cons _ tail) = tail


-- Convenience functions

singleton : a -> Cons a
singleton x = Cons x []

fromList : List a -> Maybe (Cons a)
fromList l =
  case l of
    [] -> Nothing
    head::tail -> Just <| Cons head tail

toList : Cons a -> List a
toList (Cons head tail) = head::tail

tail' : Cons a -> Maybe (Cons a)
tail' = tail >> fromList


-- List methods that maintain Cons

map : (a -> b) -> Cons a -> Cons b
map f (Cons head tail) = Cons (f head) (List.map f tail)

map2 : (a -> b -> c) -> Cons a -> Cons b -> Cons c
map2 f (Cons x xs) (Cons y ys) = Cons (f x y) (List.map2 f xs ys)


-- List methods

length = toList >> List.length
