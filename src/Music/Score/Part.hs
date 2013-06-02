                              
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleInstances,
    OverloadedStrings,
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
        HasPart(..),
        -- PartName(..),
        PartT(..),
        extract,
        mapPart,
        mapParts,
        getParts,
        setParts,
        modifyParts,
        
        -- ** Part composition
        (</>),
        moveParts,
        moveToPart,
  ) where

import Control.Monad (ap, mfilter, join, liftM, MonadPlus(..))
import Data.Semigroup
import Data.String
import Data.Foldable
import Data.Ord (comparing)
import Data.Traversable
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace
import Data.Ratio

import Music.Score.Voice
import Music.Score.Score (Score, note, rest, perform)
import Music.Time.Relative
import Music.Time.Absolute
import Music.Score.Ties


class HasPart a where
    -- | 
    -- Associated voice type. Should implement 'Ord' and 'Show' to be usable.
    -- 
    type Part a :: *

    -- |
    -- Get the voice of the given note.
    -- 
    getPart :: a -> Part a

    -- |
    -- Set the voice of the given note.
    -- 
    setPart :: Part a -> a -> a

    -- |
    -- Modify the voice of the given note.
    -- 
    modifyPart :: (Part a -> Part a) -> a -> a
   
    setPart n = modifyPart (const n)
    modifyPart f x = x

-- newtype PartName = PartName { getPartName :: String }
    -- deriving (Eq, Ord, IsString)
-- instance Show PartName where show = getPartName

newtype PartT n a = PartT { getPartT :: (n, a) }
    deriving (Eq, Ord, Show, Functor)

instance HasPart ()                            where   { type Part ()         = Integer ; getPart _ = 0 }
instance HasPart Double                        where   { type Part Double     = Integer ; getPart _ = 0 }
instance HasPart Float                         where   { type Part Float      = Integer ; getPart _ = 0 }
instance HasPart Int                           where   { type Part Int        = Integer ; getPart _ = 0 }
instance HasPart Integer                       where   { type Part Integer    = Integer ; getPart _ = 0 }
instance Integral a => HasPart (Ratio a)       where   { type Part (Ratio a)  = Integer ; getPart _ = 0 }



-- | 
-- Extract parts from the given score. 
--
-- The parts are returned in the order given by the 'Ord' instance for the given part type.
-- You can recompose the score with 'mconcat', i.e.
-- 
-- > mconcat . extract = id
--
-- Simple type
-- 
-- > Score a -> [Score a]
--
extract :: (HasPart a, Ord v, v ~ Part a, MonadPlus s, Foldable s) => s a -> [s a]
extract sc = fmap (flip extract' sc) (getParts sc) 
    where                    
        extract' v = mfilter ((== v) . getPart)

-- |
-- Map over a single voice in the given score.
--
-- > Part -> (Score a -> Score a) -> Score a -> Score a
--
mapPart :: (Ord v, v ~ Part a, HasPart a, MonadPlus s, Foldable s, Enum b) => b -> (s a -> s a) -> s a -> s a
mapPart n f = mapParts (zipWith ($) (replicate (fromEnum n) id ++ [f] ++ repeat id))

-- |
-- Map over all voices in the given score.
--
-- > ([Score a] -> [Score a]) -> Score a -> Score a
--
mapParts :: (HasPart a, Ord v, v ~ Part a, MonadPlus s, Foldable s) => ([s a] -> [s b]) -> s a -> s b
mapParts f = msum . f . extract

-- |
-- Get all voices in the given score. Returns a list of voices.
--
-- > Score a -> [Part]
--
getParts :: (HasPart a, Ord v, v ~ Part a, Foldable s) => s a -> [Part a]
getParts = List.sort . List.nub . fmap getPart . toList

-- |
-- Set all voices in the given score.
--
-- > Part -> Score a -> Score a
--
setParts :: (HasPart a, Functor s) => Part a -> s a -> s a
setParts n = fmap (setPart n)

-- |
-- Modify all voices in the given score.
--
-- > (Part -> Part) -> Score a -> Score a
--
modifyParts :: (HasPart a, Functor s) => (Part a -> Part a) -> s a -> s a
modifyParts n = fmap (modifyPart n)



--------------------------------------------------------------------------------
-- Part composition
--------------------------------------------------------------------------------

infixr 6 </>

-- |
-- Similar to '<>', but increases voices in the second part to prevent voice collision.
--
(</>) :: (Enum v, Ord v, v ~ Part a, Functor s, MonadPlus s, Foldable s, HasPart a) => s a -> s a -> s a
a </> b = a `mplus` moveParts offset b
    where               
        -- max voice in a + 1
        offset = succ $ maximum' 0 $ fmap fromEnum $ getParts a


-- |
-- Move down one voice (all parts).
--
moveParts :: (Enum v, v ~ Part a, Integral b, Functor s, HasPart a) => b -> s a -> s a
moveParts x = modifyParts (successor x)

-- |
-- Move top-part to the specific voice (other parts follow).
--
moveToPart :: (Enum v, v ~ Part a, Functor s, HasPart a) => v -> s a -> s a
moveToPart v = moveParts (fromEnum v)









successor :: (Integral b, Enum a) => b -> a -> a
successor n | n <  0 = (!! fromIntegral (abs n)) . iterate pred
            | n >= 0 = (!! fromIntegral n)       . iterate succ

maximum' :: (Ord a, Foldable t) => a -> t a -> a
maximum' z = option z getMax . foldMap (Option . Just . Max)

minimum' :: (Ord a, Foldable t) => a -> t a -> a
minimum' z = option z getMin . foldMap (Option . Just . Min)
