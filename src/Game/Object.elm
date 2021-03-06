{--
- Made by suppi
-
-
- Template based on Elm v0.14.1 Game Skeleton
- found here: https://github.com/elm-lang/elm-lang.org/blob/258181d16df10d7b51721f1f5005baca03c8b7e7/frontend/public/examples/Intermediate/Bounce.elm
-}


module Game.Object where

-- Game
import Game.Input as Input
import Game.Utils as Utils
import Game.Utils exposing (changeIf)

-- Debug
import Debug

{-- Model --------------------------------------------------------------------

What information do you need to represent an Object?

------------------------------------------------------------------------------}

type alias Object a = { a | x:Float, y:Float }

type alias MovingObject a = Object { a | vx : Float, vy : Float }



{-- Update -------------------------------------------------------------------

How do we update the model?

------------------------------------------------------------------------------}

getPos : Object a -> Utils.Point
getPos {x,y} = {x=x,y=y}

addVelocity : Float -> Float -> Int -> Float -> Float
addVelocity accel limit dir vel =
  if | dir < 0  -> if vel - accel < limit then limit - frictionConst else vel - accel
     | dir > 0  -> if vel + accel > limit then limit + frictionConst else vel + accel
     | dir == 0 -> vel


move : MovingObject a -> MovingObject a
move ({x,y,vx,vy} as obj) =
  let (vx',vy') = if vx /= 0 && vy /= 0 then (vx * 0.7, vy * 0.7) else (vx,vy)
  in
    applyFriction <|
        { obj | x <- x + vx'
              , y <- y + vy' }

stop : MovingObject a -> MovingObject a
stop obj = { obj | vx <- 0, vy <- 0 }

init : MovingObject a -> MovingObject a
init obj = { obj | x <- 0, y <- 0, vx <- 0, vy <- 0 }

applyFriction : MovingObject a -> MovingObject a
applyFriction ({vx,vy} as obj) = { obj | vx <- friction vx
                                       , vy <- friction vy }

friction : Float -> Float
friction vel =
  if | vel == 0 -> 0
     | vel > 0  -> changeIf ((>) 0) 0 (vel - frictionConst)
     | vel < 0  -> changeIf ((<) 0) 0 (vel + frictionConst)


frictionConst : Float
frictionConst = 1

walk : (Int,Int) -> Float -> Float -> MovingObject a -> MovingObject a
walk (dx,dy) step limit obj = move <|
    { obj | vx    <- addV dx step limit obj.vx
          , vy    <- addV dy step limit obj.vy }


addV : Int -> Float -> Float -> Float -> Float
addV dir step abslimit vel =
  let
      limit = if | dir == 0 ->   0
                 | dir  > 0 ->  abslimit
                 | dir  < 0 -> -abslimit
  in
      addVelocity step limit dir vel




{-- Utils --------------------------------------------------------------------

How do we update the model?

------------------------------------------------------------------------------}

checkBounds : (Utils.Point -> Bool) -> Object a -> Bool
checkBounds test obj =
  let
      bounds = Utils.squareBounds <| getPos obj
  in
     Utils.and <|
       [test bounds.upperLeft
       ,test bounds.upperRight
       ,test bounds.lowerLeft
       ,test bounds.lowerRight]

isOverlapping : Object a -> Object b -> Bool
isOverlapping obj1 obj2 =
  let
      bounds = Utils.squareBounds <| getPos obj1
  in
      getPos obj2 `Utils.lessThan` bounds.upperRight
      && bounds.lowerLeft `Utils.lessThan` getPos obj2
