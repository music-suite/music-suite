                              
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleInstances,
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


module Music.Score.Voice (
        HasVoice(..),
        getVoices,
        setVoices,
        extractVoices,
        mapVoices,
  ) where

import Control.Monad (ap, mfilter, join, liftM, MonadPlus(..))
import Data.Foldable
import Data.Traversable
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace
import Data.Ratio

import Music.Score.Part
import Music.Score.Score
import Music.Score.Duration
import Music.Score.Time



class HasVoice a where
    -- | Associated voice type.
    --   This should implement 'Eq' and 'Show' to be usable with 'toXml' and friends.
    type Voice a :: *

    getVoice :: a -> Voice a
    setVoice :: Voice a -> a -> a
    modifyVoice :: (Voice a -> Voice a) -> a -> a
    setVoice n = modifyVoice (const n)
    modifyVoice f x = x

instance HasVoice ()                            where   { type Voice () = String ; getVoice _ = "" }
instance HasVoice Double                        where   { type Voice Double = String ; getVoice _ = "" }
instance HasVoice Int                           where   { type Voice Int = String ; getVoice _ = ""     }
instance HasVoice Integer                       where   { type Voice Integer = String ; getVoice _ = ""     }

-- instance Integral a => HasVoice (Ratio a)       where   { type Voice (Ratio a) = String ; getVoice _ = "" }

instance HasVoice (String, a)                   where   
    type Voice (String, a) = String
    getVoice (v,_) = v
    modifyVoice f (v,x) = (f v, x)
instance HasVoice a => HasVoice (Bool, a, Bool) where   
    type Voice (Bool, a, Bool) = Voice a
    getVoice (_,x,_) = getVoice x

getVoices :: (HasVoice a, Eq v, v ~ Voice a, Foldable s) => s a -> [Voice a]
getVoices = List.nub . fmap getVoice . toList

setVoices :: (HasVoice a, Functor s) => Voice a -> s a -> s a
setVoices n = fmap (setVoice n)

-- | 
-- Extract the voice components of the given score.
--
-- > mconcat . extractVoices = id
--
extractVoices :: (HasVoice a, Eq v, v ~ Voice a, MonadPlus s, Foldable s) => s a -> [s a]
extractVoices sc = fmap (flip extract $ sc) (getVoices sc) 
    where                    
        extract v = mfilter ((== v) . getVoice)

mapVoices :: (HasVoice a, Eq v, v ~ Voice a, MonadPlus s, Foldable s) 
    => ([s a] -> [s b]) -> s a -> s b
mapVoices f = msum . f . extractVoices


