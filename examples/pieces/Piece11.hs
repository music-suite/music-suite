

{-
Possibly for chamber orchestra
  1111 1111 pno perc str5

Based on melodic unfolding, i.e.

  fmap2 (^.voice) Data.List.unfoldr . (Just . )
    :: (b -> (Note a, b)) -> b -> Voice a

  or really
    :: State b (Note a) -> b -> Voice a
  

  Use a single unfolding function as per above
  Give voices varying initial state
  Split by varying amount
  Use various alignment (and basic tempo)
    C'est tout!


-----
  What kind of melody?

-}