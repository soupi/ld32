{--
- Made by suppi
-
-
- Template based on Elm v0.14.1 Game Skeleton
- found here: https://github.com/elm-lang/elm-lang.org/blob/258181d16df10d7b51721f1f5005baca03c8b7e7/frontend/public/examples/Intermediate/Bounce.elm
-}


module Game.Banana where

-- Packages
import Graphics.Element (..)
import Graphics.Collage as Collage
import Color
import Text

-- Game
import Game.Utils  as Utils
import Game.Object as Object
import Game.Player as Player

-- Debug
import Debug

{-- Model --------------------------------------------------------------------

What information do you need to represent an Object?

------------------------------------------------------------------------------}

type alias Banana = Maybe (Object.Object {})

{-- Part 3: Update ------------------------------------------------------------

How does the game step from one state to another?

------------------------------------------------------------------------------}

-- Nothing: move is invalid
-- Just banana: move is valid - new state for banana
logic : Banana -> Player.Player -> Player.Action -> Maybe Banana
logic banana player action =
  let
      dropping = case banana of
                    Nothing -> Just <| Just {x=player.x, y=player.y}
                    Just _  -> Nothing
      picking  = case banana of
            Nothing -> Nothing
            Just b  -> if Object.isOverlapping player b then Just Nothing else Nothing
  in
      case action of
        Player.PickUpBanana -> picking
        Player.DropBanana   -> dropping
        _ -> Just banana


{-- Part 4: Display the Banana -----------------------------------------------

How should the Banana be displayed to the user?

------------------------------------------------------------------------------}

display : Banana -> Collage.Form
display banana = case banana of
  Nothing    -> Collage.toForm <| Text.asText "you have the banana"
  Just {x,y} ->
    Collage.move (x,y) <|
      Collage.filled Color.yellow <|
        Collage.rect Utils.squareSize Utils.squareSize
