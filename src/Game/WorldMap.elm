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
import Matrix

-- Game
import Game.Input  as Input
import Game.Utils  as Utils
import Game.Object as Object

-- Debug
import Debug

{-- Model --------------------------------------------------------------------

What information do you need to represent an Object?

------------------------------------------------------------------------------}

type alias WorldMap = Matrix.Matrix Bool


{-- Functions ----------------------------------------------------------------

How do we use the model?

------------------------------------------------------------------------------}

create : Int -> Int -> Int -> (WorldMap, Collage.Form, List (Int, Int), Random.Seed)
create w h seed =
  let (map, list, seed')  = createMaze (0,w-1) (0, h-1) (-1, -1) (Random.initialSeed seed) (Matrix.repeat w h False) 2
      form          = worldMapForm map
  in
     (map, form, list, seed')


retryIfEquals : a -> Random.Generator a -> Random.Seed -> (a, Random.Seed)
retryIfEquals x gen seed =
  let (y,seed') = Random.generate gen seed
  in
     if x /= y then (y,seed') else retryIfEquals x gen seed'

createMaze : (Int, Int) -> (Int, Int) -> (Int, Int) -> Random.Seed -> WorldMap -> Int -> (WorldMap, List (Int, Int), Random.Seed)
createMaze (lw,hw) (lh,hh) (rowHole, colHole) seed map maxDepth =
  let _ = Debug.log "size" (lw,hw,lh,hh)
  in
  if lw+3 >= hw || lh+3 >= hh || maxDepth == 0
  then (map, [(lw+1, lh+1)], seed)
  else
    let (row,seed')     = retryIfEquals colHole (Random.int (lw+2) (hw-2)) seed
        (col,seed'')    = retryIfEquals rowHole (Random.int (lh+2) (hh-2)) seed'
        (cdiv,seed''')  = retryIfEquals col (Random.int (lh+3) (hh-3)) seed''
        (rdiv1,seed4)   = retryIfEquals row (Random.int (lw+3) (row-3)) seed'''
        (rdiv2,seed5)   = retryIfEquals row (Random.int (row+3) (hw-3)) seed4
        divRow          = fillRowWithWallExcept row lh hh col cdiv seed5 map
        dividedMaze     = fillColWithWallExcept col lw hw row rdiv1 rdiv2 seed5 divRow
        (maze1, l1, s1) = createMaze (lw, row-1) (lh, col-1) (cdiv, rdiv1) seed5 dividedMaze (maxDepth - 1)
        (maze2, l2, s2) = createMaze (lw, row-1) (col+1, hh) (cdiv, rdiv1) s1 maze1 (maxDepth - 1)
        (maze3, l3, s3) = createMaze (row+1, hw) (lh, col-1) (cdiv, rdiv2) s2 maze2 (maxDepth - 1)
        (maze , l4, s4) = createMaze (row+1, hw) (col+1, hh) (cdiv, rdiv2) s3 maze3 (maxDepth - 1)
    in
        (maze, l1 ++ l2 ++ l3 ++ l4, s4)


fillRowWithWallExceptLogic : Int -> Int -> Int -> Int -> Bool -> WorldMap -> (WorldMap, Bool)
fillRowWithWallExceptLogic row c1 c2 except success map =
  if | c1 > c2      -> (map, success)
     | c1 == except -> if Maybe.withDefault True (Matrix.get (row-1) c1 map) == False &&
                          Maybe.withDefault True (Matrix.get (row+1) c1 map) == False
                       then
                          fillRowWithWallExceptLogic row (c1+1) c2 except True map
                       else
                          fillRowWithWallExceptLogic row (c1+1) c2 (c1+1) False <|
                          Matrix.set row c1 True map
     | otherwise    -> fillRowWithWallExceptLogic row (c1+1) c2 except success <|
                    Matrix.set row c1 True map


fillRowWithWallExcept col start end div except seed map =
  let (maze, _)  = fillWithWallExceptRand fillRowWithWallExceptLogic col start end except seed  map
  in
     maze

fillColWithWallExcept : Int -> Int -> Int -> Int -> Int -> Int -> Random.Seed -> WorldMap -> WorldMap
fillColWithWallExcept col start end div except1 except2 seed map =
  let (maze1, seed')  = fillWithWallExceptRand fillColWithWallExceptLogic col start div except1 seed  map
      (maze2, seed'') = fillWithWallExceptRand fillColWithWallExceptLogic col div   end except2 seed' maze1
  in
     maze2

fillWithWallExceptRand f col start end except seed map =
  if   start == except
  then (map, seed)
  else
    case f col start end except False map of
       (newmaze, True)  -> (newmaze, seed)
       (newmaze, False) -> let (newExcept, seed') = Random.generate (Random.int start (except-1)) seed
                           in
                               fillWithWallExceptRand f col start end newExcept seed' map


fillColWithWallExceptLogic : Int -> Int -> Int -> Int -> Bool -> WorldMap -> (WorldMap, Bool)
fillColWithWallExceptLogic col start end except success map =
  if | start > end     -> (map, success)
     | start == except -> if   Maybe.withDefault True (Matrix.get start (col-1) map) == False &&
                               Maybe.withDefault True (Matrix.get start (col+1) map) == False
                          then
                               fillColWithWallExceptLogic col (start+1) end except True map
                          else
                               fillColWithWallExceptLogic col (start+1) end (except+1) success <|
                               Matrix.set start col True map
     | otherwise       -> fillColWithWallExceptLogic col (start+1) end except success <|
                          Matrix.set start col True map


get : Int -> Int -> WorldMap -> Maybe Bool
get = Matrix.get

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
size = Matrix.size

scaledSize : WorldMap -> (Float, Float)
scaledSize map = (Utils.apply2 Utils.scale (size map))


worldMapForm map =
  Collage.toForm
  <| uncurry Collage.collage (Utils.apply2 truncate
  <| scaledSize map)
  <| List.concat
  <| Matrix.toList (Matrix.indexedMap (wallOrEmpty (Utils.apply2 (flip (/) 2) <| scaledSize map)) map)



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
