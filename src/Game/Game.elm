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
import AnimationFrame
import Time
import Window
import Signal
import Signal ((<~),(~))
import Text
import Keyboard
import Char

-- Game
import Game.Input  as Input
import Game.Player as Player

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

type alias GameState = { player : Player.Player }

defaultGame : GameState
defaultGame = { player = Player.defaultPlayer }



{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

------------------------------------------------------------------------------}

stepGame : Input.Input -> GameState -> GameState
stepGame ({dimensions,time,userInput} as input) gameState =
    let act        = Player.getAction input gameState.player
        new_player = if True {- act /= Player.PickUpBanana -} then Player.act time act gameState.player else gameState.player
    in
       { gameState | player <- new_player }



{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

------------------------------------------------------------------------------}

display : (Int,Int) -> GameState -> Element
display (w,h) gameState =
    --show gameState
    Text.asText gameState

