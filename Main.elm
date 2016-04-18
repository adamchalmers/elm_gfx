import Gfx exposing (rndShape, canvas, rings)
import Task
import Random
import Signal
import Mouse
import Graphics.Element exposing (show)

seedToPic n = 
  canvas [rings (Random.initialSeed n) |> fst]

fst (a,b) = a
snd (a,b) = b

main = Signal.map2 (+) Mouse.x Mouse.y |> Signal.map seedToPic