{--
- Made by suppi
-
-
- Template based on Elm v0.14.1 Game Skeleton
- found here: https://github.com/elm-lang/elm-lang.org/blob/258181d16df10d7b51721f1f5005baca03c8b7e7/frontend/public/examples/Intermediate/Bounce.elm
-}


module Game.Guard where

-- Packages
import Graphics.Element
import Graphics.Collage as Collage
import Graphics.Collage (rect, filled, outlined)
import Color
import Time
import Random
import Maybe

-- Game
import Game.Object as Object
import Game.Banana as Banana
import Game.Input  as Input
import Game.Utils  as Utils

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


defaultGuard : Int -> Guard
defaultGuard seed = { x = 0, y = 0, vx = 0, vy = 0, seed = initialSeed seed, state = Walking (0,0) }

{-- Part 3: Update the Guard -------------------------------------------------

How does the Guard step from one state to another based on user input?

------------------------------------------------------------------------------}

getAction : WorldMap.WorldMap -> Player.Player -> Banana.Banana -> Guard -> Action
getAction map player banana guard =
  case guard.state of
    Tripping timeLeft    -> if timeLeft > 0 then Wait else Walk (0, 0)
    Chasing dir timeLeft ->
      if | Maybe.withDefault False <| Object.isOverlapping guard `Maybe.map` banana -> Trip
         | timeLeft > 0 && WorldMap.isValidObjectLocation map (walk dir guard) -> Chase dir
         | otherwise -> ChangeDirection
    Walking dir timeLeft ->
      if | Maybe.withDefault False <| Object.isOverlapping guard `Maybe.map` banana -> Trip
         | isInSight (Utils.unscaleP <| getPos guard) dir map (Utils.unscaleP <| getPos player) -> Chase dir
         | timeLeft > 0 && WorldMap.isValidObjectLocation map (walk dir guard) -> Walk dir
         | otherwise -> ChangeDirection

isInSight : (Int, Int) -> (Int, Int) -> WorldMap.WorldMap -> (Int, Int) -> Bool
isInSight (gx,gy) dir map (px,py) =
  if | (dir.x /= 0 && dir.y /= 0 || dir.x == 0 && dir.y == 0) -> False
     | dir.x /= 0 -> isEmptyColFromTo gy gx (gx + (dir.x * 5)
     | dir.y /= 0 -> isEmptyRowFromTo gx gy (gy + (dir.y * 5)

act : Time.Time -> Action -> Guard -> Guard
act time action guard =
  case action of
    Walk dir t   -> walk dir (t - time) guard
    Trip         -> { guard | state <- Tripping 3 }
    Wait -> Object.stop <| case guard.state of
      Walking _  -> { guard | state <- Tripping 0 }  -- invariant. Shouldn't happen.
      Tripping t -> { guard | state <- Tripping (t - time) }
    Chase dir    -> case guard.state of
      Chasing _ t -> { guard | state <- Chasing dir (t - time) }
      _ -> { guard | state <- Chasing dir 5 }
    ChangeDirection ->
      let (res,seed' ) -> Random.generate (Random.int 0    1) seed
          (dir,seed'') -> Random.generate (Random.int (-1) 1) seed'
          newdir       -> if res == 0 then (0, dir) else (dir,0)
      in
         { guard | seed <- seed'', Walking newdir 4 }


walk : (Int,Int) -> Float -> Guard -> Guard
walk ((dx,dy) as dir) time guard =
  let
      newGuard = Object.walk dir guard
  in
    { newGuard | state <- Walking (dx,dy) time }


{-- Part 4: Display the player -----------------------------------------------

How should the player be displayed to the user?

------------------------------------------------------------------------------}

display guard =
  Collage.move (guard.x,guard.y) <|
  filled Color.red <|
  uncurry rect <| Utils.apply2 ((*) Utils.squareSize) (1,1)
