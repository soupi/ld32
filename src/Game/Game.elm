{--
- Made by suppi
-
-
- Template based on Elm v0.14.1 Game Skeleton
- found here: https://github.com/elm-lang/elm-lang.org/blob/258181d16df10d7b51721f1f5005baca03c8b7e7/frontend/public/examples/Intermediate/Bounce.elm
-}


module Game.Game where

-- Packages
import Graphics.Element as Element
import Graphics.Collage as Collage
import Color
import Window
import Signal
import Text
import List

-- Game
import Game.WorldMap as WorldMap
import Game.Banana   as Banana
import Game.Player   as Player
import Game.Object   as Object
import Game.Guard    as Guard
import Game.Utils    as Utils
import Game.Input    as Input
import Game.Maybe    as Maybe

import Game.Player (defaultPlayer)
import Game.Guard  (defaultGuard)

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

type alias Goal = Object.Object {}

type Status = Ongoing | GameOver | Victory

type alias GameState =
  { player : Player.Player
  , banana : Banana.Banana
  , guards : List Guard.Guard
  , goal   : Goal
  , status : Status
  , map    : WorldMap.WorldMap
  , form   : Collage.Form
  }

defaultGame : GameState
defaultGame =
  let (map, form) = WorldMap.create 20 15 9
  in
    { player = ({ defaultPlayer | x <- Utils.scale 2, y <- Utils.scale 12 })
    , banana = Nothing
    , guards = defaultGuards
    , goal   = { x = 19 * Utils.squareSize, y = 11 * (Utils.squareSize) }
    , status = Ongoing
    , map    = map
    , form   = form }

defaultGuards =
  [defaultGuard (Utils.scale 3, Utils.scale 5) 5
  ,defaultGuard (Utils.scale 14, Utils.scale 6) 3
  ,defaultGuard (Utils.scale 9, Utils.scale 9) 27
  ,defaultGuard (Utils.scale 19, Utils.scale 2) 38
  ]


{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

------------------------------------------------------------------------------}

stepGame : Input.Input -> GameState -> GameState
stepGame input gameState =
  case gameState.status of
    GameOver -> gameState
    Victory  -> gameState
    Ongoing  ->
      let
          (banana, p1) = updateBananaPlayer input gameState
          guards       = guardsUpdate input gameState
          newGameState = { gameState | player <- Debug.watch "player" p1
                                     , banana <- Debug.watch "banana" banana
                                     , guards <- Debug.watch "guards" guards }
          status = checkStatus newGameState
      in
          { newGameState | status <- status }


collideGuardsPlayer : List Guard.Guard -> Player.Player -> Player.Player
collideGuardsPlayer guards player =
  if Utils.or <| List.map (\guard -> (not (Guard.isTripping guard)) && Object.isOverlapping player guard) guards
     then Player.die player
     else player

checkStatus : GameState -> Status
checkStatus gameState =
  if | gameState.player.state == Player.Win  -> Victory
     | gameState.player.state == Player.Dead -> GameOver
     | otherwise -> Ongoing


updateBananaPlayer : Input.Input -> GameState -> (Banana.Banana, Player.Player)
updateBananaPlayer ({time,userInput} as input) gameState =
    let action     = Player.getAction input gameState.player
        new_player = Player.act time action gameState.player
        valid_new_player = if WorldMap.isValidObjectLocation gameState.map new_player then new_player else Object.stop gameState.player
        collided_player  = collideGuardsPlayer gameState.guards valid_new_player
        player = if Object.isOverlapping collided_player gameState.goal then { player | state <- Player.Win } else collided_player
    in
        case Banana.logic gameState.banana player action of
          Nothing -> (gameState.banana, gameState.player)
          Just b  -> (b, player)

guardsUpdate : Input.Input -> GameState -> List Guard.Guard
guardsUpdate input gameState = List.map (guardUpdate input gameState) gameState.guards

guardUpdate : Input.Input -> GameState -> Guard.Guard -> Guard.Guard
guardUpdate ({time,userInput} as input) gameState guard =
  let
      action = Guard.getAction gameState.map gameState.player gameState.banana guard
      next   = Guard.act time action guard
      newG   = if WorldMap.isValidObjectLocation gameState.map next then next else { next | x <- guard.x, y <- guard.y, vx <- 0, vy <- 0 }
  in
      newG


{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

------------------------------------------------------------------------------}

display : (Int,Int) -> GameState -> Element.Element
display (w,h) gameState =
  let size     = Utils.apply2 truncate <| WorldMap.scaledSize gameState.map
      halfSize = Utils.apply2 ((-) 0 << (\v -> v / 2)) <| WorldMap.scaledSize gameState.map
  in
     Element.container w h Element.middle <|

        uncurry Collage.collage size <|
          [WorldMap.display gameState.map gameState.form
          ,Collage.move halfSize <| displayGoal gameState.goal
          ,Collage.move halfSize <| Banana.display gameState.banana
          ,Collage.move halfSize <| Debug.trace "player1" <| Player.display gameState.player]
          `List.append`
          List.map (Collage.move halfSize << Guard.display) gameState.guards
          `List.append`
          [displayStatus gameState.status]


displayGoal goal =
    Collage.move (goal.x,goal.y) <|
      Collage.toForm <|
        Element.image (truncate <| Utils.squareSize) (truncate <| Utils.squareSize)  "../../assets/imgs/goal.png"

displayStatus status = Collage.toForm <|
  case status of
    Ongoing  -> Element.empty
    Victory  -> Text.centered <| Text.color Color.yellow <| Text.fromString "Victory!"
    GameOver -> Text.centered <| Text.color Color.red    <| Text.fromString "GAME OVER"
