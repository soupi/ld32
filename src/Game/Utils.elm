module Game.Utils where

changeIf : (a -> Bool) -> a -> a -> a
changeIf test def x = if test x then def else x


squareSize = 32

scale : Int -> Float
scale = (*) squareSize << toFloat

scaleF : Float -> Float
scaleF = (*) squareSize

unscale : Float -> Int
unscale x = if | x < 0     -> round (x - squareSize) // squareSize
               | otherwise -> round x // squareSize


apply2 : (a -> b) -> (a, a) -> (b, b)
apply2 f (x,y) = (f x, f y)

