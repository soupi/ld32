{--
- Made by suppi
-
-
- Template based on Elm v0.14.1 Game Skeleton
- found here: https://github.com/elm-lang/elm-lang.org/blob/258181d16df10d7b51721f1f5005baca03c8b7e7/frontend/public/examples/Intermediate/Bounce.elm
-}


module Game where

import Graphics.Element (..)
import AnimationFrame
import Time
import Window
import Signal
import Signal ((<~),(~))
import Text
import Keyboard
import Char

import Debug


{-- Part 0: Signals -----------------------------------------------------------

What information do you need to represent all relevant user input?

------------------------------------------------------------------------------}


main : Signal.Signal Element
main =
    Signal.map2 display Window.dimensions gameState


gameState : Signal.Signal GameState
gameState =
    Signal.foldp stepGame defaultGame input


input : Signal.Signal Input
input =
    Signal.sampleOn delta (Signal.map3 Input Window.dimensions delta userInput)


delta : Signal.Signal Time.Time
delta = AnimationFrame.frame


userInput : Signal.Signal UserInput
userInput = Signal.sampleOn delta <|
            UserInput <~ Debug.watch "start"      Keyboard.enter
                       ~ Debug.watch "select"     Keyboard.space
                       ~ Debug.watch "actionA"   (Keyboard.isDown (Char.toCode 'z'))
                       ~ Debug.watch "actionB"   (Keyboard.isDown (Char.toCode 'x'))
                       ~ Debug.watch "direction"  Keyboard.arrows


{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

------------------------------------------------------------------------------}


type alias Input =
    { dimensions : (Int,Int)
    , time : Time.Time
    , userInput : UserInput
    }


type alias UserInput =
  { start     : Bool
  , select    : Bool
  , actionA   : Bool
  , actionB   : Bool
  , direction : { x : Int, y : Int }
  }



{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

------------------------------------------------------------------------------}

type alias GameState = {}

defaultGame : GameState
defaultGame =
    {}



{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

------------------------------------------------------------------------------}

stepGame : Input -> GameState -> GameState
stepGame {dimensions,userInput} gameState =
    gameState



{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

------------------------------------------------------------------------------}

display : (Int,Int) -> GameState -> Element
display (w,h) gameState =
    --show gameState
    Text.asText gameState

