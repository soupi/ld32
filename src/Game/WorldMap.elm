{--
- Made by suppi
-
-
- Template based on Elm v0.14.1 Game Skeleton
- found here: https://github.com/elm-lang/elm-lang.org/blob/258181d16df10d7b51721f1f5005baca03c8b7e7/frontend/public/examples/Intermediate/Bounce.elm
-}


module Game.WorldMap where

-- Core Packages
import Graphics.Element (..)
import Graphics.Collage (rect, filled, outlined)
import Color
import Maybe

-- Packages
import Array2D

-- Game
import Game.Input  as Input
import Game.Utils  as Utils
import Game.Object as Object

-- Debug
import Debug

{-- Model --------------------------------------------------------------------

What information do you need to represent an Object?

------------------------------------------------------------------------------}

type alias WorldMap = Array2D.Array2D Bool


{-- Functions ----------------------------------------------------------------

How do we use the model?

------------------------------------------------------------------------------}

create : Int -> Int -> WorldMap
create w h = Array2D.repeat w h False

get : Int -> Int -> WorldMap -> Maybe Bool
get = Array2D.get

isEmptyRowFromTo : Int -> Int -> Int -> WorldMap -> Bool
isEmptyRowFromTo row c1 c2 map =
  if | c1 >  c2 -> isEmptyRowFromTo row c2 c1 map
     | c1 == c2 -> Maybe.withDefault False <| get row c1 map
     | c1 <  c2 -> case get row c1 map of
                     Nothing    -> False
                     Just True  -> False
                     Just False -> isEmptyRowFromTo row (c1 + 1) c2 map

isEmptyColFromTo : Int -> Int -> Int -> WorldMap -> Bool
isEmptyColFromTo col r1 r2 map =
  if | r1 >  r2 -> isEmptyColFromTo col r2 r1 map
     | r1 == r2 -> Maybe.withDefault False <| get r1 col map
     | r1 <  r2 -> case get r1 col map of
                     Nothing    -> False
                     Just True  -> False
                     Just False -> isEmptyColFromTo col (r1 + 1) r2 map

isValidStep : WorldMap -> (Int, Int) -> Bool
isValidStep map (x,y) = Maybe.withDefault False <| Maybe.map not <| get x y map

isValidObjectLocation : WorldMap -> Object.Object a -> Bool
isValidObjectLocation map obj = Object.checkBounds (isValidStep map << Utils.unscaleP) obj


size : WorldMap -> (Int, Int)
size map = if Array2D.length map == 0 then (0, 0)
                                      else (Array2D.length1 map, Array2D.length map // Array2D.length1 map)

scaledSize : WorldMap -> (Float, Float)
scaledSize map = (Utils.apply2 Utils.scale (size map))



{-- View ---------------------------------------------------------------------

How do we display the map?

------------------------------------------------------------------------------}

display map =
  filled Color.blue <|
  uncurry rect <| Utils.apply2 Utils.scale <| size map
