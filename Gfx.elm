module Gfx where

import Color
import Graphics.Element exposing (Element)
import Graphics.Collage as Collage
import Array
import Random
import Debug

canvas : List Collage.Form -> Element
canvas = Collage.collage width width

width = 600

rings : Random.Seed -> (Collage.Form, Random.Seed)
rings seed = 
  let
    (_, s0) = Random.generate (Random.int 101 201) seed
    (ring1, s1) = rndShape s0 250
    (ring2, s2) = rndShape s1 150
  in
    (Collage.group [ring1, ring2], s2)

rndShape : Random.Seed -> Int -> (Collage.Form, Random.Seed)
rndShape seed dist = 
  let
    rInt = Random.int
    (_, s0) = Random.generate (rInt 101 201) seed
    (color, s1) = rndColor s0
    (sides, s2) = Random.generate (rInt 3 8) s1
    (sized, s3) = Random.generate (rInt 10 80) s2
    (width, s4) = Random.generate (rInt (dist-50) (dist+50)) s3
    (angle, s5) = Random.generate (rInt 0 90) s4
    rndShape = Collage.ngon sides (toFloat sized)
  in
    ( fourfold 
      (Collage.filled color rndShape) 
      (toFloat width) (toFloat angle)
    , s5)

choices : Array.Array Color.Color
choices = Array.fromList
  [ Color.red
  , Color.green
  , Color.green
  , Color.blue
  , Color.purple
  , Color.orange
  ]
    
rndColor : Random.Seed -> (Color.Color, Random.Seed)
rndColor seed =
  let
    n = (Array.length choices) - 1
    (index, newSeed) = log seed |> Random.generate (Random.int 0 n)
    color = Array.get index choices |> log
  in
    case color of
      Nothing ->
        Debug.log ("Bad index " ++ (toString n)) (Color.black, newSeed)
      Just col ->
        (col, newSeed)


-- fourfold duplicates the shape four times.
fourfold : Collage.Form -> Float -> Float -> Collage.Form
fourfold shape size angle = Collage.group 
  [ Collage.move (0, size) shape
  , Collage.move (0, -size) shape
  , Collage.move (size, 0) shape
  , Collage.move (-size, 0) shape
  ] |> Collage.rotate (degrees angle)

log : a -> a
log x = Debug.log "" x
