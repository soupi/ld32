{--
- Made by suppi
-
-
- Template based on Elm v0.14.1 Game Skeleton
- found here: https://github.com/elm-lang/elm-lang.org/blob/258181d16df10d7b51721f1f5005baca03c8b7e7/frontend/public/examples/Intermediate/Bounce.elm
-}


module Game.Player where

-- Packages
import Graphics.Element as Element
import Graphics.Collage as Collage
import Graphics.Collage (rect, filled, outlined)
import Color
import Time
import Text

-- Game
import Game.Object as Object
import Game.Input  as Input
import Game.Utils  as Utils

-- Debug
import Debug

{-- Part 2: Model the player -------------------------------------------------

What information do you need to represent the player?

------------------------------------------------------------------------------}

type alias Player = Object.MovingObject { hasBanana : Bool, state : PlayerState }

type Action = Walk (Int,Int)
            | DropBanana
            | PickUpBanana
            | Wait

type Act = DroppingBanana | PickingUpBanana | Caught | Dying

type PlayerState = Walking (Int,Int)
                 | Acting Act Float
                 | Dead


defaultPlayer : Player
defaultPlayer = { x = 0, y = 0, vx = 0, vy = 0, hasBanana = True, state = Walking (0,0) }

{-- Part 3: Update the player -------------------------------------------------

How does the player step from one state to another based on user input?

------------------------------------------------------------------------------}

getAction : Input.Input -> Player -> Action
getAction {dimensions,time,userInput} player =
  case player.state of
    Dead -> Wait
    Acting _ timeLeft -> if timeLeft > 0 then Wait else Walk (userInput.direction.x, userInput.direction.y)
    Walking _         ->
      if | userInput.actionA && (not player.hasBanana) -> PickUpBanana
         | userInput.actionA &&  player.hasBanana      -> DropBanana
         | otherwise -> Walk (userInput.direction.x, userInput.direction.y)


act : Time.Time -> Action -> Player -> Player
act time action player =
  case action of
    Wait -> Object.stop <| case player.state of
      Walking _  -> { player | state <- Dead }  -- invariant. Shouldn't happen.
      Dead       ->   player
      Acting a v -> { player | state <- Acting a (v-time) }
    DropBanana     -> Object.stop { player | state <- Acting DroppingBanana  (Time.second * 1), hasBanana <- False }
    PickUpBanana   -> Object.stop { player | state <- Acting PickingUpBanana (Time.second * 2), hasBanana <- True  }
    Walk direction -> walk direction player

isPickingUpBanana player = case player.state of
  Acting PickingUpBanana _ -> True
  _ -> False

isDroppingBanana player = case player.state of
  Acting DroppingBanana _ -> True
  _ -> False


walk : (Int,Int) -> Player -> Player
walk ((dx,dy) as dir) player =
  let
      newPlayer = Object.walk dir 1.2 6 player
  in
    { newPlayer | state <- Walking (dx,dy) }


{-- Part 4: Display the player -----------------------------------------------

How should the player be displayed to the user?

------------------------------------------------------------------------------}


display player =
  let action = getActionVerb player
  in
      Collage.move (player.x,player.y) <|
        Collage.toForm <|
          uncurry Element.image (Utils.apply2 ((*) Utils.squareSize) (1,1)) <|
            "../../assets/imgs/bandit-" ++ action ++ ".png"



getActionVerb : Player -> String
getActionVerb player =
    case player.state of
      Walking (x,y) ->
        if | y > 0 -> "up"
           | y < 0 -> "down"
           | x > 0 -> "right"
           | x < 0 -> "left"
           | otherwise -> "down"
      Acting DroppingBanana _  -> "up"
      Acting PickingUpBanana _ -> "up"
      Acting Caught _ -> "left"
      Acting Dying _ -> "right"
      Dead -> "up"

