{--
- Made by suppi
-
-
- Template based on Elm v0.14.1 Game Skeleton
- found here: https://github.com/elm-lang/elm-lang.org/blob/258181d16df10d7b51721f1f5005baca03c8b7e7/frontend/public/examples/Intermediate/Bounce.elm
-}


module Game.WorldMap where

-- Core Packages
import Graphics.Element as Element
import Graphics.Collage as Collage
import Random
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

create : Int -> Int -> Int -> (WorldMap, Collage.Form)
create w h seed =
  let (map,_)  = createMaze (0,w-1) (0, h-1) (Random.initialSeed seed) (Array2D.repeat w h False) 2
      form     = worldMapForm map
  in
     (map, form)



createMaze : (Int, Int) -> (Int, Int) -> Random.Seed -> WorldMap -> Int -> (WorldMap, Random.Seed)
createMaze (lw',hw') (lh',hh') seed map maxD =
  let (lw,hw,lh,hh) = (lw' + 0, hw' - 0, lh' + 0, hh' - 0)
      _ = Debug.log "values" (lw,hw,lh,hh)
  in
  if lw >= hw || lh >= hh || maxD == 0
  then (map, seed)
  else
    let (row,seed')     = Random.generate (Random.int (lw+1) (hw+1)) seed
        (col,seed'')    = Random.generate (Random.int (lh+1) (hh-1)) seed'
        (cdiv,seed''')  = Random.generate (Random.int (lh+1) (hh-1)) seed''
        (rdiv1,seed'''') = Random.generate (Random.int (lw+1) (row-1)) seed'''
        (rdiv2,seed''''') = Random.generate (Random.int (row+1) (hw-1)) seed''''
        divRow          = fillRowWithWallExcept row lh hh cdiv map
        dividedMaze     = fillColWithWallExcept col lw hw rdiv1 rdiv2 divRow
        (maze1,s1) = createMaze (lw, row - 1) (lh, col - 1) seed'''' dividedMaze (maxD - 1)
        (maze2,s2) = createMaze (lw, row - 1) (col + 1, hh) s1 maze1 (maxD - 1)
        (maze3,s3) = createMaze (row + 1, hw) (lh, col - 1) s2 maze2 (maxD - 1)
        maze       = createMaze (row + 1, hw) (col + 1, hh) s3 maze3 (maxD - 1)
    in
       maze


fillRowWithWallExcept : Int -> Int -> Int -> Int -> WorldMap -> WorldMap
fillRowWithWallExcept row c1 c2 except map =
  if | c1 > c2      -> map
     | c1 == except -> fillRowWithWallExcept row (c1+1) c2 except map
     | otherwise    -> fillRowWithWallExcept row (c1+1) c2 except <|
                    Array2D.set row c1 True map

fillColWithWallExcept : Int -> Int -> Int -> Int -> Int -> WorldMap -> WorldMap
fillColWithWallExcept col r1 r2 except1 except2 map =
  if | r1 > r2      -> map
     | r1 == except1 || r1 == except2 -> fillColWithWallExcept col (r1+1) r2 except1 except2 map
     | otherwise    -> fillColWithWallExcept col (r1+1) r2 except1 except2 <|
                    Array2D.set r1 col True map


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


worldMapForm map = Collage.toForm <|
  uncurry Collage.collage (Utils.apply2 truncate <| scaledSize map) <|
  Array2D.toList (Array2D.indexedMap (wallOrEmpty (Utils.apply2 (flip (/) 2) <| scaledSize map)) map)



{-- View ---------------------------------------------------------------------

How do we display the map?

------------------------------------------------------------------------------}

display map form =
  Collage.toForm <| uncurry Collage.collage (Utils.apply2 truncate <| scaledSize map) <|
  [Collage.filled (Color.rgb 74 85 86) <| uncurry Collage.rect <| scaledSize map]
    ++ [form]

wallOrEmpty : (Float, Float) -> Int -> Int -> Bool -> Collage.Form
wallOrEmpty (halfSizeW,halfSizeH) x y square = case square of
  False -> Collage.toForm Element.empty
  True  -> Collage.move (toFloat x * Utils.squareSize - halfSizeW + (Utils.squareSize/2), toFloat y * Utils.squareSize - halfSizeH + (Utils.squareSize/2)) <|
    Collage.toForm <|
      uncurry Element.image (Utils.apply2 ((*) Utils.squareSize) (1,1)) <|
        "../../assets/imgs/wall.png"
