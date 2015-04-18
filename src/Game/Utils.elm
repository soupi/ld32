module Game.Utils where


changeIf : (a -> Bool) -> a -> a -> a
changeIf test def x = if test x then def else x
