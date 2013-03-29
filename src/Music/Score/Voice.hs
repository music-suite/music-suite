                              
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


module Music.Score.Voice (
        HasVoice(..),
        VoiceName(..),
        VoiceT(..),
        voices,
        mapVoices,
        getVoices,
        setVoices,
        modifyVoices,
  ) where

import Control.Monad (ap, mfilter, join, liftM, MonadPlus(..))
import Data.String
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
import Music.Score.Ties


class HasVoice a where
    -- | 
    -- Associated voice type. Should implement 'Ord' and 'Show' to be usable.
    -- 
    type Voice a :: *

    -- |
    -- Get the voice of the given note.
    -- 
    getVoice :: a -> Voice a

    -- |
    -- Set the voice of the given note.
    -- 
    setVoice :: Voice a -> a -> a

    -- |
    -- Modify the voice of the given note.
    -- 
    modifyVoice :: (Voice a -> Voice a) -> a -> a
   
    setVoice n = modifyVoice (const n)
    modifyVoice f x = x

newtype VoiceName = VoiceName { getVoiceName :: String }
    deriving (Eq, Ord, IsString)
instance Show VoiceName where show = getVoiceName

newtype VoiceT n a = VoiceT { getVoiceT :: (n, a) }
    deriving (Eq, Ord, Show, Functor)

instance HasVoice ()                            where   { type Voice ()         = VoiceName ; getVoice _ = "" }
instance HasVoice Double                        where   { type Voice Double     = VoiceName ; getVoice _ = "" }
instance HasVoice Float                         where   { type Voice Float      = VoiceName ; getVoice _ = "" }
instance HasVoice Int                           where   { type Voice Int        = VoiceName ; getVoice _ = "" }
instance HasVoice Integer                       where   { type Voice Integer    = VoiceName ; getVoice _ = "" }
instance Integral a => HasVoice (Ratio a)       where   { type Voice (Ratio a)  = VoiceName ; getVoice _ = "" }



-- | 
-- Extract parts from the given score. Returns a list of single-part score. A dual of @pcat@.
--
-- > Score a -> [Score a]
--
voices :: (HasVoice a, Ord v, v ~ Voice a, MonadPlus s, Foldable s) => s a -> [s a]
voices sc = fmap (flip extract $ sc) (getVoices sc) 
    where                    
        extract v = mfilter ((== v) . getVoice)

-- |
-- Map over the voices in a given score.
--
-- > ([Score a] -> [Score a]) -> Score a -> Score a
--
mapVoices :: (HasVoice a, Ord v, v ~ Voice a, MonadPlus s, Foldable s) => ([s a] -> [s b]) -> s a -> s b
mapVoices f = msum . f . voices

-- |
-- Get all voices in the given score. Returns a list of voices.
--
-- > Score a -> [Voice]
--
getVoices :: (HasVoice a, Ord v, v ~ Voice a, Foldable s) => s a -> [Voice a]
getVoices = List.sort . List.nub . fmap getVoice . toList

-- |
-- Set all voices in the given score.
--
-- > Voice -> Score a -> Score a
--
setVoices :: (HasVoice a, Functor s) => Voice a -> s a -> s a
setVoices n = fmap (setVoice n)

-- |
-- Modify all voices in the given score.
--
-- > (Voice -> Voice) -> Score a -> Score a
--
modifyVoices :: (HasVoice a, Functor s) => (Voice a -> Voice a) -> s a -> s a
modifyVoices n = fmap (modifyVoice n)



