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

-- Game
import Game.Object as Object
import Game.Input  as Input
import Game.Utils  as Utils

-- Debug
import Debug

{-- Part 2: Model the Guard --------------------------------------------------

What information do you need to represent the Guard?

------------------------------------------------------------------------------}

type alias Guard = Object.MovingObject { seed : Random.Seed, state : GuardState }

type Action = Walk (Int,Int)
            | Chase
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

getAction : Input.Input -> WorldMap.WorldMap -> Player.Player -> Guard -> Action
getAction {time} map player guard =
  case guard.state of
    Dead -> Wait
    Acting _ timeLeft -> if timeLeft > 0 then Wait else Walk (userInput.direction.x, userInput.direction.y)
    Walking _         ->
      if | userInput.actionA && (not player.hasBanana) -> PickUpBanana
         | userInput.actionA &&  player.hasBanana      -> DropBanana
         | otherwise -> Walk (userInput.direction.x, userInput.direction.y)


act : Time.Time -> Action -> Guard -> Guard
act time action guard =
  case action of
    Wait -> Object.stop <| case guard.state of
      Walking _  -> { guard | state <- Tripping 0 }  -- invariant. Shouldn't happen.
      Trip       ->   guard
      Acting a v -> { player | state <- Acting a (v-time) }
    DropBanana     -> Object.stop { player | state <- Acting DroppingBanana  (Time.second * 0.3), hasBanana <- False }
    PickUpBanana   -> Object.stop { player | state <- Acting PickingUpBanana (Time.second * 0.5), hasBanana <- True  }
    Walk direction -> walk direction player


walk : (Int,Int) -> Guard -> Guard
walk ((dx,dy) as dir) guard =
  let
      newGuard = Object.walk dir guard
  in
    { newGuard | state <- Walking (dx,dy) }


{-- Part 4: Display the player -----------------------------------------------

How should the player be displayed to the user?

------------------------------------------------------------------------------}

display guard =
  Collage.move (guard.x,guard.y) <|
  filled Color.red <|
  uncurry rect <| Utils.apply2 ((*) Utils.squareSize) (1,1)
