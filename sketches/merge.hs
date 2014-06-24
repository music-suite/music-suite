{-# LANGUAGE 
    DeriveFunctor #-}

import Data.Stream (sort)

import Data.Semigroup

-- This works
--      foldr (<>) mempty $ fmap single $ [1..]

newtype Stream a = Stream { getStream :: [a] }
    deriving (Eq, Ord, Show, Functor)
instance Ord a => Semigroup (Stream a) where
    Stream ~a <> Stream ~b = Stream (a `m` b)
        where
            m = merge
            -- m = (<>)
unStream (Stream ~a) = a
inf = [1..]
infL = Stream [1..]

instance Ord a => Monoid (Stream a) where
    mempty = Stream mempty
    mappend = (<>)

merge2 a b = sort $ a ++ b


merge ~xs ~ys | null xs = ys
merge ~xs ~ys | null ys = xs
merge xs'@(x:xs) ys'@(y:ys) | x < y   =  x : merge xs ys'
                            | x >= y  =  y : merge xs' ys
                            
single x = [x]
singleL x = Stream [x]