
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

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
-- Provides functions for manipulating ornaments (and miscellaneous stuff to be
-- given its own module soon...).
--
-------------------------------------------------------------------------------------


module Music.Score.Tremolo (
        -- * Tremolo
        HasTremolo(..),
        TremoloT(..),
        tremolo,
  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens hiding (transform)
import           Data.Functor.Couple
import           Data.Foldable
import           Data.Foldable
import           Data.Ratio
import           Data.Word
import           Data.Semigroup
import           Data.Typeable

-- import           Music.Score.Combinators
import           Music.Score.Part
import           Music.Time
import           Music.Pitch.Literal
import           Music.Dynamics.Literal
import           Music.Pitch.Alterable
import           Music.Pitch.Augmentable
import           Music.Score.Phrases

class HasTremolo a where
    setTrem :: Int -> a -> a

instance HasTremolo a => HasTremolo (b, a) where
    setTrem n = fmap (setTrem n)

instance HasTremolo a => HasTremolo (Couple b a) where
    setTrem n = fmap (setTrem n)

instance HasTremolo a => HasTremolo [a] where
    setTrem n = fmap (setTrem n)

instance HasTremolo a => HasTremolo (Score a) where
    setTrem n = fmap (setTrem n)



newtype TremoloT a = TremoloT { getTremoloT :: Couple (Max Word) a }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad, Comonad)
-- 
-- We use Word instead of Int to get (mempty = Max 0), as (Max.mempty = Max minBound)
-- Preferably we would use Natural but unfortunately this is not an instance of Bounded
-- 

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (TremoloT a) where
  type Unwrapped (TremoloT a) = Couple (Max Word) a
  _Wrapped' = iso getTremoloT TremoloT

instance Rewrapped (TremoloT a) (TremoloT b)

instance HasTremolo (TremoloT a) where
    setTrem n (TremoloT (Couple (_,x))) = TremoloT (Couple (Max $ fromIntegral n,x))

-- Lifted instances
deriving instance Num a => Num (TremoloT a)
deriving instance Fractional a => Fractional (TremoloT a)
deriving instance Floating a => Floating (TremoloT a)
deriving instance Enum a => Enum (TremoloT a)
deriving instance Bounded a => Bounded (TremoloT a)
deriving instance (Num a, Ord a, Real a) => Real (TremoloT a)
deriving instance (Real a, Enum a, Integral a) => Integral (TremoloT a)

-- |
-- Set the number of tremolo divisions for all notes in the score.
--
tremolo :: HasTremolo a => Int -> a -> a
tremolo = setTrem

