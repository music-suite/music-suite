
{-# LANGUAGE OverlappingInstances, FlexibleInstances, TypeFamilies #-}

module RemoveRests where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Arrow (second)
import Control.Monad.Plus
import Control.Monad.SearchTree
    
class Rests a where
    type RemoveRests a
    removeRests :: (Functor m, MonadPlus m) => m a -> m (RemoveRests a)

type Score = []

instance Rests a => Rests (Score a) where
    type RemoveRests (Score a) = Score (RemoveRests a)
    removeRests = fmap removeRests

instance Rests a => Rests (SearchTree a) where
    type RemoveRests (SearchTree a) = SearchTree (RemoveRests a)
    removeRests = fmap removeRests

instance Rests (Maybe a) where
    type RemoveRests (Maybe a) = a
    removeRests = mcatMaybes

-- Everything else is just identities

instance Rests Double where
    type RemoveRests Double = Double
    removeRests = id
instance Rests Int where
    type RemoveRests Int = Int
    removeRests = id
instance Rests a => Rests (b, a) where
    type RemoveRests (b,a) = (b,a)
    removeRests = id

-- etc

main = defaultMain $ testGroup "" $ fmap (testCase "")
    [ 
        removeRests [Nothing, Just 1, Just 2]
            @?=     [1,2],

        removeRests [[Nothing, Just 1, Just 2], [Just 3]]
            @?=     [[1,2],[3]],

        removeRests [[("h",1),("h",2::Int)]] 
            @?=     [[("h",1),("h",2)]],

        removeRests [Just [1, 2, 3], Just [], Nothing] 
            @?=     [[1,2,3],[]]
    ]
