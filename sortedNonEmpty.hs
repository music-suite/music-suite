
{-# LANGUAGE
    CPP,
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    NoMonomorphismRestriction,
    GeneralizedNewtypeDeriving #-} 

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Ordered and NonEmpty as type functions.
--
-- > *Data.Sorted> return 1 :: [Int]
-- > [1]
-- > *Data.Sorted> return 1 :: NonEmpty [Int]
-- > 1 :| []
-- > *Data.Sorted> return 1 :: Ordered [Int]
-- > O {getO = [1]}
-- > *Data.Sorted> return 1 :: Ordered (NonEmpty [Int])
-- > O {getO = 1 :| []}
-- > *Data.Sorted> return 1 :: NonEmpty (Ordered [Int])
-- > O {getO = 1 :| []}
-- > *Data.Sorted> fromList [1] <> return 1 :: NonEmpty (Ordered [Int])
-- > 


--
-------------------------------------------------------------------------------------

-- TODO split
module Data.Sorted --(
--  )
where

import Control.Applicative
import Data.Default
import Data.Semigroup
import qualified Data.List          as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List.Ordered  as Ordered

type family NonEmpty (a :: *) :: *
type family Ordered  (a :: *) :: *

-- TODO use data families instead?

#define NE NonEmpty.NonEmpty
-- type    NE = NonEmpty.NonEmpty
type Order = OrderT []

type instance NonEmpty [a]      = NE a
type instance Ordered  [a]      = OrderT [] a
type instance Ordered  (NE a)   = OrderT NE a
type instance NonEmpty (OrderT [] a) = OrderT NE a
type instance Ordered  (OrderT l a)  = OrderT l a
type instance NonEmpty (NE a)   = NE a



newtype OrderT l a   = OrderT { getOrderT :: l a }
    deriving (Eq, Ord, Show, Functor, Applicative, Monad)
instance (List l, Ord a) => Semigroup (OrderT l a) where
    (<>) = merge
instance List l => List (OrderT l) where
    sort (OrderT a)          = OrderT a
    merge (OrderT a) (OrderT b)   = OrderT (merge a b)
    fromList            = OrderT . sort . fromList
    toList (OrderT a)        = toList a


class List l where
    sort        :: Ord a => l a -> l a
    merge       :: Ord a => l a -> l a -> l a
    mergeOrd    :: Ord a => l a -> l a -> l a
    toList      :: l a -> [a]
    fromList    :: (Ord a, Default a) => [a] -> l a
    showList'   :: (Show a, Ord a) => l a -> String
    mergeOrd    = merge
    showList'   = ("fromList " ++) . show . toList

instance List [] where        
    sort            = List.sort
    a `merge` b     = List.sort (a <> b)
    mergeOrd        = Ordered.merge
    fromList        = id
    toList          = id
instance List (NonEmpty.NonEmpty) where
    sort            = NonEmpty.sort
    a `merge` b     = NonEmpty.sort (a <> b)
    fromList        = NonEmpty.fromList . listDef
      where
        listDef [] = [def]
        listDef xs = xs
    toList          = NonEmpty.toList

foo :: (
    [x]          ~ a,
    NonEmpty [x] ~ b,
    Ordered  [x] ~ c,
    NonEmpty (Ordered [x]) ~ d,
    Ordered (NonEmpty [x]) ~ d,

    -- a ~ b,
    -- a ~ c,
    -- a ~ d,
    -- b ~ c,
    -- b ~ d,
    -- c ~ d,
    
    Double -- dummy, no comma here
    ) 
    => ()
foo = undefined


x  :: Ord a => Ordered (NonEmpty [a])
y  :: Ord a => NonEmpty (Ordered [a])
xy :: Ord a => Ordered (NonEmpty [a])
xy = x `merge` y


[x,y] = undefined


{-
    > return 1 :: [Int]
    [1]

    > return 1 :: Ordered [Int]
    O {getO = [1]}

    > return 1 :: NonEmpty [Int]
    1 :| []

    > return 1 :: NonEmpty (Ordered [Int])
-}