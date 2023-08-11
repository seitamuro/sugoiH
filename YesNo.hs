class YesNo a where
  yesno :: a -> Bool

instance YesNo Bool where
  yesno False = False
  yesno True = True

instance YesNo String where
  yesno [] = False
  yesno _ = True

instance YesNo Int where
  yesno x
    | x == 0 = False
    | otherwise = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True