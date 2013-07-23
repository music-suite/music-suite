{-# LANGUAGE 
    DeriveFunctor #-}

import Data.List (sort)

import Data.Semigroup

-- This works
--      foldr (<>) mempty $ fmap single $ [1..]

newtype List a = List { getList :: [a] }
    deriving (Eq, Ord, Show, Functor)
instance Ord a => Semigroup (List a) where
    List ~a <> List ~b = List (a `m` b)
        where
            m = merge
            -- m = (<>)
unlist (List ~a) = a
inf = [1..]
infL = List [1..]

instance Ord a => Monoid (List a) where
    mempty = List mempty
    mappend = (<>)

merge2 a b = sort $ a ++ b


merge ~xs ~ys | null xs = ys
merge ~xs ~ys | null ys = xs
merge xs'@(x:xs) ys'@(y:ys) | x < y   =  x : merge xs ys'
                            | x >= y  =  y : merge xs' ys
                            
single x = [x]
singleL x = List [x]