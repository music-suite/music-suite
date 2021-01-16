
module Data.FiniteSeq (List0To4, parseList, toList0To4) where


-- | A sequence of @n@ elements where @0 <= n <= 4@.
newtype List0To4 a = UnsafeList0To4 {_unsafeGetList0To4 :: [a]}
  deriving (Functor, Foldable, Traversable)

parseList :: [a] -> Maybe (List0To4 a)
parseList xs
  | length xs <= 4 = Just $ UnsafeList0To4 xs
  | otherwise = Nothing

-- | Equivalent to 'toList', but more effiecient.
toList0To4 :: List0To4 a -> [a]
toList0To4 (UnsafeList0To4 xs) = xs



