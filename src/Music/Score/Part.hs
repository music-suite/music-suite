                              
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
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
-- Provides a musical score represenation.
--
-------------------------------------------------------------------------------------

module Music.Score.Part (
        Part(..)
  ) where

import Prelude hiding (foldr, concat, foldl, mapM, concatMap, maximum, sum, minimum)

import Data.Semigroup
import Control.Applicative
import Control.Monad (ap, join, MonadPlus(..))
import Data.Foldable
import Data.Traversable
import Data.Maybe
import Data.Either
import Data.Function (on)
import Data.Ord (comparing)
import Data.Ratio
import Data.VectorSpace
import Data.AffineSpace

import Music.Score.Time
import Music.Score.Duration

-------------------------------------------------------------------------------------
-- Part type
-------------------------------------------------------------------------------------

-- |
-- A part is a sorted list of relative-time notes and rests.
--
-- Part is a 'Monoid' under sequential compisiton. 'mempty' is the empty part and 'mappend'
-- appends parts.
--
-- Part has an 'Applicative' instance derived from the 'Monad' instance.
--
-- Part is a 'Monad'. 'return' creates a part containing a single value of duration
-- one, and '>>=' transforms the values of a part, allowing the addition and
-- removal of values under relative duration. Perhaps more intuitively, 'join' scales 
-- each inner part to the duration of the outer part, then removes the 
-- intermediate structure. 
--
-- > let p = Part [(1, Just 0), (2, Just 1)] :: Part Int
-- >
-- > p >>= \x -> Part [ (1, Just $ toEnum $ x+65), 
-- >                    (3, Just $ toEnum $ x+97) ] :: Part Char
-- >
-- >     ===> Part {getPart = [ (1 % 1,Just 'A'),
-- >                            (3 % 1,Just 'a'),
-- >                            (2 % 1,Just 'B'),
-- >                            (6 % 1,Just 'b') ]}
--
-- Part is a 'VectorSpace' using sequential composition as addition, and time scaling
-- as scalar multiplication.
--
newtype Part a = Part { getPart :: [(Duration, a)] }
    deriving (Eq, Ord, Show, Functor, Foldable, Monoid)

instance Semigroup (Part a) where
    (<>) = mappend

instance Applicative Part where
    pure  = return
    (<*>) = ap

instance Monad Part where
    return a = Part [(1, a)]
    a >>= k = join' $ fmap k a
        where
            join' (Part ps) = foldMap (uncurry (*^)) ps

instance AdditiveGroup (Part a) where
    zeroV   = mempty
    (^+^)   = mappend
    negateV = id

instance VectorSpace (Part a) where
    type Scalar (Part a) = Duration
    n *^ Part as = Part (fmap (first (n*^)) as)

instance HasDuration (Part a) where
    duration (Part []) = 0
    duration (Part as) = sum (fmap fst as)
                        





list z f [] = z
list z f xs = f xs

first f (x,y)  = (f x, y)
second f (x,y) = (x, f y)
