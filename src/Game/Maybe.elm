
module Game.Maybe where



infixl 4 <$>
(<$>) : (a -> b) -> Maybe a -> Maybe b
(<$>) f x =
  case x of
    Just x -> Just (f x)
    Nothing -> Nothing

infixl 4 <*>
(<*>) : Maybe (a -> b) -> Maybe a -> Maybe b
(<*>) f x = case (f, x) of
  (Just f, Just x) -> Just (f x)
  _                -> Nothing

