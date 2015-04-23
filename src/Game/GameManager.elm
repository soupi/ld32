{--
- Made by suppi
-
-
- Template based on Elm v0.14.1 Game Skeleton
- found here: https://github.com/elm-lang/elm-lang.org/blob/258181d16df10d7b51721f1f5005baca03c8b7e7/frontend/public/examples/Intermediate/Bounce.elm
-}


module Game.GameManager where

-- Packages
import Graphics.Element as Element
import Graphics.Collage as Collage
import Color
import Window
import Signal
import Text
import List
import Random
import Time

-- Game
import Game.Game     as Game
import Game.Input    as Input
import Game.Utils    as Utils
import Game.WorldMap as WorldMap

-- Debug
import Debug


{-- Part 0: Signals -----------------------------------------------------------

What information do you need to represent all relevant user input?

------------------------------------------------------------------------------}


main : Signal.Signal Element.Element
main =
    Signal.map2 display Window.dimensions gameState


gameState : Signal.Signal GameState
gameState =
    Signal.foldp stepGame defaultGame Input.input



{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

------------------------------------------------------------------------------}

type Status = Ongoing Int | GameOver | Victory | Wait Int Float

type alias GameState =
  { game   : Game.GameState
  , level  : Int
  , seed   : Random.Seed
  , status : Status
  }

defaultGame : GameState
defaultGame =
  let seed = Random.initialSeed 5
      (r,s) = Random.generate (Random.int 1 999) seed
  in
      { game = Game.defaultGame r
      , seed = s
      , status = Ongoing 5
      }

{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

------------------------------------------------------------------------------}

stepGame : Input.Input -> GameState -> GameState
stepGame input gameState =
  case gameState.status of
    GameOver    -> gameState
    Victory     -> gameState
    Ongoing 0   -> gameState
    Wait n time -> if time > 0
                   then gameState
                   else
                        let (i, s') = Random.generate (Random.int 1 999) gameState.seed
                        in { gameState | game <- Game.defaultGame i
                                       , seed <- s'
                                       , status <- Ongoing (n-1) }

    Ongoing n -> case gameState.game.status of
        Game.Ongoing  -> { gameState | game = Game.stepGame input gameState.game }
        Game.GameOver -> { gameState | status = GameOver }
        Game.Victory  -> { gameState | status = Wait n (Time.second * 2) }



{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

------------------------------------------------------------------------------}

display : (Int,Int) -> GameState -> Element.Element
display (w,h) gameState =
  let size     = Utils.apply2 ((+) 20 << truncate) <| WorldMap.scaledSize gameState.game.map
      halfSize = Utils.apply2 ((-) 0 << (\v -> v / 2)) <| WorldMap.scaledSize gameState.game.map
  in
     Element.container w h Element.middle <|
     Game.display gameState.game

