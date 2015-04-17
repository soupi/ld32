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
import Text


{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type alias UserInput = {}


userInput : Signal UserInput
userInput =
    Signal.sampleOn delta <| Signal.constant {}


type alias Input =
    { dimensions : (Int,Int)
    , time : Time.Time
    , userInput : UserInput
    }



{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

Tasks: Redefine `GameState` to represent your particular game.
       Redefine `defaultGame` to represent your initial game state.

For example, if you want to represent many objects that just have a position,
your GameState might just be a list of coordinates and your default game might
be an empty list (no objects at the start):

    type GameState = { objects : [(Float,Float)] }
    defaultGame = { objects = [] }

------------------------------------------------------------------------------}

type alias GameState = {}

defaultGame : GameState
defaultGame =
    {}



{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

stepGame : Input -> GameState -> GameState
stepGame {dimensions,userInput} gameState =
    gameState



{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

Task: redefine `display` to use the GameState you defined in part 2.

------------------------------------------------------------------------------}

display : (Int,Int) -> GameState -> Element
display (w,h) gameState =
    --show gameState
    Text.asText gameState



{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and shows it on screen.

------------------------------------------------------------------------------}

delta : Signal Time.Time
delta = AnimationFrame.frame

input : Signal Input
input =
    Signal.sampleOn delta (Signal.map3 Input Window.dimensions delta userInput)


gameState : Signal GameState
gameState =
    Signal.foldp stepGame defaultGame input


main : Signal Element
main =
    Signal.map2 display Window.dimensions gameState

