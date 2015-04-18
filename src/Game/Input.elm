{--
- Made by suppi
-
-}


module Game.Input where

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

Which Signals that changes over time are relevant?

------------------------------------------------------------------------------}


input : Signal.Signal Input
input =
    Signal.sampleOn delta (Signal.map3 Input Window.dimensions delta userInput)


delta : Signal.Signal Time.Time
delta = AnimationFrame.frame


userInput : Signal.Signal UserInput
userInput = Signal.sampleOn delta <|
              UserInput <~ Keyboard.enter
                         ~ Keyboard.space
                         ~ Keyboard.isDown (Char.toCode 'Z')
                         ~ Keyboard.isDown (Char.toCode 'X')
                         ~ Keyboard.arrows


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

