
module Data.LabelTree
  ( LabelTree(..),
    concatLT,
    fromListLT,
    foldLabelTree,
  )
where

-- Annotated tree
data LabelTree b a = Branch b [LabelTree b a] | Leaf a
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

-- forall x . concatLT x . fmap pure = id
concatLT :: b -> LabelTree b [a] -> LabelTree b a
concatLT b = foldLabelTree f Branch
  where
    f [x] = Leaf x
    f xs = Branch b (fmap Leaf xs)

fromListLT :: Monoid b => [a] -> LabelTree b a
fromListLT = Branch mempty . fmap Leaf

foldLabelTree :: (a -> c) -> (b -> [c] -> c) -> LabelTree b a -> c
foldLabelTree f _ (Leaf x) = f x
foldLabelTree f g (Branch b xs) = g b (fmap (foldLabelTree f g) xs)

