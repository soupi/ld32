{--
- Made by suppi
-
-
- Template based on Elm v0.14.1 Game Skeleton
- found here: https://github.com/elm-lang/elm-lang.org/blob/258181d16df10d7b51721f1f5005baca03c8b7e7/frontend/public/examples/Intermediate/Bounce.elm
-}


module Game.Guard where

-- Packages
import Graphics.Element as Element
import Graphics.Collage as Collage
import Graphics.Collage (rect, filled, outlined)
import Color
import Time
import Random
import Maybe

-- Game
import Game.WorldMap as WorldMap
import Game.Object   as Object
import Game.Banana   as Banana
import Game.Player   as Player
import Game.Input    as Input
import Game.Utils    as Utils

-- Debug
import Debug

{-- Part 2: Model the Guard --------------------------------------------------

What information do you need to represent the Guard?

------------------------------------------------------------------------------}

type alias Guard = Object.MovingObject { seed : Random.Seed, state : GuardState }

type Action = Walk (Int,Int)
            | ChangeDirection
            | Chase (Int, Int)
            | Trip
            | Wait

type GuardState = Walking (Int,Int) Float
                | Chasing (Int, Int) Float
                | Tripping Float


defaultGuard : (Float, Float) -> Int -> Guard
defaultGuard (x,y) seed = { x = x, y = y, vx = 0, vy = 0, seed = Random.initialSeed seed, state = Walking (0,0) 0 }

{-- Part 3: Update the Guard -------------------------------------------------

How does the Guard step from one state to another based on user input?

------------------------------------------------------------------------------}

getAction : WorldMap.WorldMap -> Player.Player -> Banana.Banana -> Guard -> Action
getAction map player banana guard = Debug.watch "guard action" <|
  case guard.state of
    Tripping timeLeft    -> if timeLeft > 0 then Wait else Walk (0, 0)
    Chasing dir timeLeft ->
      if | Maybe.withDefault False <| Object.isOverlapping guard `Maybe.map` banana -> Trip
         | timeLeft > 0 && WorldMap.isValidObjectLocation map (walk dir 0 guard) -> Chase dir
         | otherwise -> ChangeDirection
    Walking dir timeLeft ->
      if | Maybe.withDefault False <| Object.isOverlapping guard `Maybe.map` banana -> Trip
         | isInSight (Utils.unscaleP <| Object.getPos guard) dir map (Utils.unscaleP <| Object.getPos player) -> Chase dir
         | timeLeft > 0 && WorldMap.isValidObjectLocation map (walk dir 0 guard) -> Walk dir
         | otherwise -> ChangeDirection

isInSight : (Int, Int) -> (Int, Int) -> WorldMap.WorldMap -> (Int, Int) -> Bool
isInSight gLoc dir map pLoc = checkIfPlayerIsClose gLoc dir pLoc && checkIfNoWalls gLoc dir map pLoc

checkIfNoWalls : (Int, Int) -> (Int, Int) -> WorldMap.WorldMap -> (Int, Int) -> Bool
checkIfNoWalls (gx,gy) (dx,dy) map (px,py) = not <|
  if | dx /= 0 && dy /= 0 || dx == 0 && dy == 0 -> False
     | dx /= 0 -> WorldMap.isEmptyColFromTo gy gx (gx + px) map
     | dy /= 0 -> WorldMap.isEmptyRowFromTo gx gy (gy + py) map

checkIfPlayerIsClose : (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
checkIfPlayerIsClose (gx,gy) dir (px,py) =
  case dir of
    (0,  0) -> False
    (0, dy) -> if dy > 0 then gy < py && gy + (dy * 5) > py && gx == px
               else gy > py && gy + (dy * 5) < py && gx == px
    (dx, 0) -> if dx > 0 then gx < px && gx + (dx * 5) > px && gy == py
               else gx > px && gx + (dx * 5) < px && gy == py
    (dx,dy) -> False


act : Time.Time -> Action -> Guard -> Guard
act time action guard =
  case action of
    Walk dir     -> case guard.state of
        Walking _ t -> walk dir (t - time) guard
        _ -> walk dir 4 guard
    Trip         ->
      let (dx,seed')  = Random.generate (Random.int (-1) 1) guard.seed
          (dy,seed'') = Random.generate (Random.int (-1) 1) seed'
      in
         trip (dx,dy) { guard | vx <- (toFloat dx) * 7, vy <- (toFloat dy) * 7 }
         |> \g -> { g | state <- Tripping (Time.second * 3), seed <- seed'' }
    Wait -> case guard.state of
      Walking _ _ -> { guard | state <- Tripping 0 }  -- invariant. Shouldn't happen.
      Tripping t  -> (\g -> { g | state <- Tripping (t - time) }) <| walk (0,0) 0 guard -- (for banana slip)
    Chase dir     -> case guard.state of
        Chasing _ t -> chase dir t guard
        _ -> { guard | state <- Chasing dir (Time.second * 5) }
    ChangeDirection ->
      newDirection guard


newDirection guard =
  let (res,seed')  = Random.generate (Random.int 0    1) guard.seed
      (dir,seed'') = Random.generate (Random.int (-1) 1) seed'
      newdir       = if res == 0 then (0, dir) else (dir,0)
      newG         = (\g -> { g | seed <- seed'' }) <| walk newdir (Time.second * 3) <| Object.stop guard
  in
     newG


walk : (Int,Int) -> Float -> Guard -> Guard
walk ((dx,dy) as dir) time guard =
  let
      newGuard = Object.walk dir 1.05 1 guard
  in
    { newGuard | state <- Walking (dx,dy) time }

chase : (Int,Int) -> Float -> Guard -> Guard
chase ((dx,dy) as dir) time guard =
  let
      newGuard = Object.walk dir 1.05 5 guard
  in
    { newGuard | state <- Chasing (dx,dy) time }


trip : (Int,Int) -> Guard -> Guard
trip ((dx,dy) as dir) guard = Object.walk dir 1.7 20 guard

isTripping : Guard -> Bool
isTripping guard = case guard.state of
  Tripping _ -> True
  _ -> False

{-- Part 4: Display the player -----------------------------------------------

How should the player be displayed to the user?

------------------------------------------------------------------------------}

display guard =
  let action = getActionVerb guard
  in
      Collage.move (guard.x,guard.y) <|
        Collage.toForm <|
          uncurry Element.image (Utils.apply2 ((*) Utils.squareSize) (1,1)) <|
            "../../assets/imgs/guard-" ++ action ++ ".png"



getActionVerb : Guard -> String
getActionVerb guard =
    case guard.state of
      Walking (x,y) _ ->
        if | y > 0 -> "up"
           | y < 0 -> "down"
           | x > 0 -> "right"
           | x < 0 -> "left"
           | otherwise -> "right"
      Chasing (x,y) _ ->
        if | y > 0 -> "up"
           | y < 0 -> "chase-down"
           | x > 0 -> "chase-right"
           | x < 0 -> "chase-left"
           | otherwise -> "chase-down"
      Tripping _ -> "trip"

