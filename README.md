# Lines of Lines / Bézier Curves in Haskell
![animated screenshot](https://x.st/images/bezier.gif)

A line usually goes from one point to another, but can we draw a line from one line to another?

It turns out we can, and it's called a Bézier curve.


## A Line Between Two Points

Let's start with the usual line between two points.

Given the one-dimensional `line1d` from a number *a* at `Time` *t=0* to a number *b* at *t=1*, the multi-dimensional `line` between two points *p* and *q* is the combination of the one-dimensional lines on each dimension:
```haskell
type Time = Float
type Point = [Float]
type Curve = Time -> Point

line :: Point -> Point -> Curve
line p q t = zipWith line1d p q
  where line1d a b = (1 - t)*a + t*b
```

We can try it out in ghci and see if it works:
```haskell
λ> let l = line [0, 0] [1, 2]
λ> l 0
[0, 0]
λ> l 1
[1, 2]
λ> l 0.5
[0.5, 1]
```
![animation of the example line](TODO)
So far so good!


## A Line Between Two Lines

Now instead of fixed points, let's have our line function take *curves* instead of points:
```haskell
linear :: Curve -> Curve -> Curve
linear f g t = zipWith line1d (f t) (g t)
  where line1d a b = (1 - t)*a + t*b
```

Finally we can see what a line between two lines looks like:
```haskell
λ> let f = line [0, 0] [0, 1]
λ> let g = line [1, 0] [1, 1]
λ> let h = linear f g
λ> h 0
[0, 0]
λ> h 1
[1, 1]
λ> h 0.5
-- TODO [x, y]
![animation of the example lines](TODO)
Cool!


## A Line Between Multiple Points

If we have three points, we can draw two lines to connect them together.
```haskell
λ> let points = [[0, 0], [1, 1], [1, 0]]
λ> let l01 = line points!!0 points!!1
λ> let l12 = line points!!1 points!!2
```
![picture of the two joined lines](TODO)

But we know how to make a line between two lines:
```haskell
λ> let l02 = linear l01 l12
```
![animation of the example lines](TODO)

What if we have four points?
```haskell
λ> let ps = [[0, 0], [0, 1], [1, 1], [1, 0]]

λ> let l01 = line ps!!0 ps!!1
λ> let l12 = line ps!!1 ps!!2
λ> let l23 = line ps!!2 ps!!3

λ> let l02 = linear l01 l12
λ> let l13 = linear l12 l23

λ> let l03 = linear l02 l13
```
![animation of the example lines](TODO)
Crazy! This is what a Bézier curve is.

We can generalize this process for any list of points, where the Beziér of a single point is fixed at that point, and the Beziér of *n* points is the line between the Beziér of the first *n-1* points and the Beziér of the last n-1 points:
```haskell
bezier :: [Point] -> Curve
bezier [p] = const p
bezier ps  = linear (bezier (init ps)) (bezier (tail ps))
```

Now we can make crazy curves through a bunch of points:
```haskell
λ> let f = bezier [[0, 0], [1, 1], [2, 0], [1, 0], [2, 1], [3, 0]]
```
![animation of the example curve](TODO)


## Refactoring

So far the code looks like this:
```haskell
type Time = Float
type Point = [Float]
type Curve = Time -> Point

linear :: Curve -> Curve -> Curve
linear f g t = zipWith line1d (f t) (g t)
  where line1d a b = (1 - t)*a + t*b

bezier :: [Point] -> Curve
bezier [p] = const p
bezier ps  = linear (bezier (init ps)) (bezier (tail ps))
```

This seems fine, but `linear` is quite specialized. What if we just want to use a standard `line` between two points, can we still write `bezier` in a nice way?

We'll start off simple and improve it:
```haskell
type Time = Float
type Point = [Float]
type Curve = Time -> Point

line :: Point -> Point -> Curve
line p q t = zipWith line1d p q
  where line1d a b = (1 - t)*a + t*b

bezier :: [Point] -> Curve
bezier [p] t = p
bezier ps  t = line (bezier (init ps) t) (bezier (tail ps) t) t
```
Not terrible, but we end up referencing `t` a bunch of times. It turns out there's a way to pass `t` around implicitly, using the *function monad*. Since `Curve` is already a function, we don't even have to use any different types:
```haskell
type Time = Float
type Point = [Float]
type Curve = Time -> Point

line :: Point -> Point -> Curve
line p q t = zipWith line1d p q
  where line1d a b = (1 - t)*a + t*b

bezier :: [Point] -> Curve
bezier [p] = const p
bezier ps  = do p <- bezier (init ps)
                q <- bezier (tail ps)
                line p q
```

We can actually take this a step further, because the monad is `(Time ->)`, so `line1d` is also in the monad, even though its output is `Float` whereas the other functions output `Point`. So, making the function monad explicit as any `Parametric` function from `Time`, we get:
```haskell
import Control.Monad (zipWithM)

type Time = Float
type Point = [Float]
type Parametric a = Time -> a

line1d :: Float -> Float -> Parametric Float
line1d a b = \t -> (1 - t)*a + t*b

line :: Point -> Point -> Parametric Point
line p q = zipWithM line1d p q

bezier :: [Point] -> Parametric Point
bezier [p] = const p
bezier ps  = do p <- bezier (init ps)
                q <- bezier (tail ps)
                line p q
```
I like this way because both `line` and `bezier` are point-free—i.e. don't mention *t* at all—and all three functions operate in the same `Parametric` monad, and are useful independent building blocks.

Just for fun, a completely equivalent way to write `bezier` using applicative functor operators:
```haskell
bezier :: [Point] -> Parametric Point
bezier [p] = const p
bezier ps  = join $ line <$> bezier (init ps) <*> bezier (tail ps)
```
The `join` is unfortunate though, but is necessary because `line` takes two arguments, which doesn't play perfectly with the applicative machinery.

Note that in the function monad, `const` and `pure` and `return` are all the exact same thing, so we could have written it with any of these. To me `const` is clearest, though `pure` sounds coolest.


# Bezier.hs

A quick demonstration of how simple Bezier curves are, and how easily you can implement them in Haskell.

For example, to play with the Bezier curve on the four points of the unit square you would run:

    > ghci Bezier.hs
    >> let squareCurve = bezier [[0.0, 0.0], [0.0, 1.0], [1.0, 1.0], [1.0, 0.0]]

And then you can see where the curve is at various points along its path from t=0 to t=1:

    >> squareCurve 0.0
      [0.0,0.0]
    >> squareCurve 0.25
      [0.15625,0.5625]
    >> squareCurve 0.5
      [0.5,0.75]
    >> squareCurve 0.75
      [0.84375,0.5625]
    >> squareCurve 1.0
      [1.0,0.0]

To run the graphical version, `cabal install gloss` (which may require `brew install llvm` first, I can't remember...) And then:

    > ghc Bezier.hs Main.hs
    > ./Main

Click to add a point, 'd' to delete it, space to pause, up and down to increase level of detail, and 'v' to toggle whether sub-curves are drawn.
