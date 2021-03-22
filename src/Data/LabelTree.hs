
module Data.LabelTree
  ( LabelTree (..),
    concatLT,
    fromListLT,
    foldLabelTree,
  )
where

import Test.QuickCheck

-- | A label-tree is a rose-tree where branches are annotated with type @b@ and
-- leaves are annotated with type @a@.
data LabelTree b a = Branch b [LabelTree b a] | Leaf a
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

instance (Arbitrary b, Arbitrary a) => Arbitrary (LabelTree b a) where
  arbitrary = go (5 :: Int)
    where
      go n = do
        p <- arbitrary
        if p && n > 0
          then do
            b <- arbitrary
            j <- (`mod` (5 :: Int)) <$> arbitrary
            xs <- sequence $ replicate j $ go (n - 1)
            pure $ Branch b xs
          else
            Leaf <$> arbitrary

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck

-- | Convert leaves to branches, using the given annotation.
-- Leaves containing singleton lists are left as leaves.
--
-- prop> \x t -> concatLT x (fmap pure t) == t
concatLT :: b -> LabelTree b [a] -> LabelTree b a
concatLT b = foldLabelTree f Branch
  where
    f [x] = Leaf x
    f xs = Branch b (fmap Leaf xs)

-- | Convert a list to a tree one level deep, using `mempty` as the annotation.
fromListLT :: Monoid b => [a] -> LabelTree b a
fromListLT = Branch mempty . fmap Leaf

-- | Fold a label tree.
foldLabelTree :: (a -> c) -> (b -> [c] -> c) -> LabelTree b a -> c
foldLabelTree f _ (Leaf x) = f x
foldLabelTree f g (Branch b xs) = g b (fmap (foldLabelTree f g) xs)
