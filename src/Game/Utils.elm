module Game.Utils where

changeIf : (a -> Bool) -> a -> a -> a
changeIf test def x = if test x then def else x


squareSize = 44


apply2 : (a -> b) -> (a, a) -> (b, b)
apply2 f (x,y) = (f x, f y)

and : List Bool -> Bool
and list = case list of
  []         -> True
  False::_   -> False
  True::rest -> and rest

or : List Bool -> Bool
or list = case list of
  []         -> False
  True::rest -> True
  False::rest-> or rest


type alias Point = {x:Float,y:Float}

scale : Int -> Float
scale = (*) squareSize << toFloat

scaleF : Float -> Float
scaleF = (*) squareSize

scaleP : (Int, Int) -> Point
scaleP (x,y) = {x=scale x, y=scale y}

unscaleP : Point -> (Int, Int)
unscaleP {x,y} = (unscale x, unscale y)

unscale : Float -> Int
unscale x = if | x < 0     -> round (x - squareSize) // squareSize
               | otherwise -> round x // squareSize


lessThan : Point -> Point -> Bool
lessThan p1 p2 = p1.x <= p2.x && p1.y <= p2.y

type alias Bounds = { upperLeft  : Point
                    , upperRight : Point
                    , lowerLeft  : Point
                    , lowerRight : Point }

bounds : Float -> Float -> Point -> Bounds
bounds w h center =
  let (halfW, halfH) = (w / 4, h / 2)
      upperLeft  = { x = center.x - (halfW/1.1), y = center.y + (halfH/2) }
      upperRight = { x = center.x + (halfW/1.1), y = center.y + (halfH/2) }
      lowerLeft  = { x = center.x - (halfW/1.1), y = center.y - (halfH/1.1) }
      lowerRight = { x = center.x + (halfW/1.1), y = center.y - (halfH/1.1) }
  in
     { upperLeft  = upperLeft
     , upperRight = upperRight
     , lowerLeft  = lowerLeft
     , lowerRight = lowerRight }

squareBounds : Point -> Bounds
squareBounds = bounds (squareSize-2) (squareSize-2)


