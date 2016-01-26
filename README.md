# Bezier.hs
![animated screenshot](https://x.st/images/bezier.gif)

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
