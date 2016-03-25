{-
  partDefault :: [(Part, a)] -> Group (Part, a)
  partDefault xs = groupDefault $ fmap (\(p,x) -> (p^._instrument,(p,x))) xs
-}
