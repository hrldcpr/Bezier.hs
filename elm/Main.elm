import Time exposing (Time)
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import String
import List

import Bezier exposing (..)


ps = [(50, 50), (50, 150), (150, 150), (150, 50)]
curve = bezier ps

main : Signal Html
main =
  Signal.map view (Time.every Time.second)

view : Time -> Html
view time =
  Svg.svg
    [width "200", height "200", viewBox "0 0 200 200"]
    [Svg.path
            [fill "none", stroke "black",
             steps 100 |> List.concatMap (curve >> toList) |> pathString |> d]
            []
    ] ++

viewPoint : Time -> Maybe Svg
viewPoint time =
  let
    t = (1 + cos time) / 2
  in
    Maybe.map
     Svg.circle
       [fill "black", r "5"]
       []
    ]

toList : Maybe a -> List a
toList x =
  case x of
    Nothing -> []
    Just x -> [x]

steps : Float -> List Float
steps n =
  let scale i = i / n
  in List.map scale [0..n]

pathString : List Point -> String
pathString points =
  case points of
    [] ->
      ""
    head::tail ->
      ("M" ++ pointString head) :: List.map lineString tail |> String.join " "

lineString : Point -> String
lineString point = "L" ++ pointString point

pointString : Point -> String
pointString (x, y) = String.join "," [toString x, toString y]
