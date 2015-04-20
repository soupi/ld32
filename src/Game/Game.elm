{--
- Made by suppi
-
-
- Template based on Elm v0.14.1 Game Skeleton
- found here: https://github.com/elm-lang/elm-lang.org/blob/258181d16df10d7b51721f1f5005baca03c8b7e7/frontend/public/examples/Intermediate/Bounce.elm
-}


module Game.Game where

-- Packages
import Graphics.Element (..)
import Graphics.Collage (collage, toForm)
import Graphics.Collage as Collage
import Color
import Window
import Signal
import Text
import List

-- Game
import Game.Input    as Input
import Game.Player   as Player
import Game.WorldMap as WorldMap
import Game.Object   as Object
import Game.Banana   as Banana
import Game.Utils    as Utils
import Game.Maybe    as Maybe

import Game.Player (defaultPlayer)

-- Debug
import Debug


{-- Part 0: Signals -----------------------------------------------------------

What information do you need to represent all relevant user input?

------------------------------------------------------------------------------}


main : Signal.Signal Element
main =
    Signal.map2 display Window.dimensions gameState


gameState : Signal.Signal GameState
gameState =
    Signal.foldp stepGame defaultGame Input.input



{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

------------------------------------------------------------------------------}

type alias GameState =
  { player : Player.Player
  , banana : Banana.Banana
  , map    : WorldMap.WorldMap
  }

defaultGame : GameState
defaultGame =
  { player = ({ defaultPlayer | x <- Utils.scale 2, y <- Utils.scale 2 })
  , banana = Nothing
  , map    = WorldMap.create 20 20 }



{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

------------------------------------------------------------------------------}

stepGame : Input.Input -> GameState -> GameState
stepGame ({dimensions,time,userInput} as input) gameState =
    let action     = Player.getAction input gameState.player
        new_player = Player.act time action gameState.player
        valid_new_player = if WorldMap.isValidObjectLocation gameState.map new_player then new_player else Object.stop gameState.player
        (p1, banana)     = case Banana.logic (Debug.watch "banana" gameState.banana) valid_new_player action of
                Nothing -> (gameState.player, gameState.banana)
                Just b  -> (valid_new_player, b)
    in
       { gameState | player <- Debug.watch "player" p1
                   , banana <- Debug.watch "banana" banana }


{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

------------------------------------------------------------------------------}

display : (Int,Int) -> GameState -> Element
display (w,h) gameState =
  let size     = Utils.apply2 truncate <| WorldMap.scaledSize gameState.map
      halfSize = Utils.apply2 ((-) 0 << (\v -> v / 2)) <| WorldMap.scaledSize gameState.map
  in
     container w h middle <|
        uncurry collage size <|
          [WorldMap.display gameState.map
          ,Collage.move halfSize <| Banana.display gameState.banana
          ,Collage.move halfSize <| Debug.trace "player1" <| Player.display gameState.player]
