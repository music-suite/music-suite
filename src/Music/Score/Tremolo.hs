
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides a representation for tremolo, i.e. rapid iterations of a note.
--
-------------------------------------------------------------------------------------


module Music.Score.Tremolo (

        -- * Tremolo
        HasTremolo(..),
        TremoloT,
        getTremolo,
        tremolo,

  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens            hiding (transform)
import           Data.Foldable
import           Data.Foldable
import           Data.Functor.Couple
import           Data.Ratio
import           Data.Semigroup
import           Data.Typeable
import           Data.Word
import           Data.Functor.Adjunction  (unzipR)

import           Music.Dynamics.Literal
import           Music.Pitch.Alterable
import           Music.Pitch.Augmentable
import           Music.Pitch.Literal
import           Music.Score.Part
import           Music.Score.Phrases
import           Music.Dynamics.Literal
import           Music.Pitch.Alterable
import           Music.Pitch.Augmentable
import           Music.Pitch.Literal
import           Music.Score.Articulation
-- import           Music.Score.Color
import           Music.Score.Dynamics
import           Music.Score.Harmonics
import           Music.Score.Meta
import           Music.Score.Part
import           Music.Score.Pitch
import           Music.Score.Slide
import           Music.Score.Text
import           Music.Score.Ties
import           Music.Time

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
  setTrem n = set (_Wrapped . _Wrapped . _1) (Max $ fromIntegral n)

-- TODO these must be moved down:
deriving instance (Monoid b, IsPitch a) => IsPitch (Couple b a)
deriving instance (Monoid b, IsDynamics a) => IsDynamics (Couple b a)
deriving instance (Monoid b, Transformable a) => Transformable (Couple b a)
deriving instance (Monoid b, Reversible a) => Reversible (Couple b a)
deriving instance (Monoid b, Alterable a) => Alterable (Couple b a)
deriving instance (Monoid b, Augmentable a) => Augmentable (Couple b a)


-- Lifted instances
deriving instance Num a => Num (TremoloT a)
deriving instance Fractional a => Fractional (TremoloT a)
deriving instance Floating a => Floating (TremoloT a)
deriving instance Enum a => Enum (TremoloT a)
deriving instance Bounded a => Bounded (TremoloT a)
deriving instance (Num a, Ord a, Real a) => Real (TremoloT a)
deriving instance (Real a, Enum a, Integral a) => Integral (TremoloT a)

deriving instance IsPitch a => IsPitch (TremoloT a)
deriving instance IsDynamics a => IsDynamics (TremoloT a)
instance Semigroup a => Semigroup (TremoloT a) where
    (<>) = liftA2 (<>)
instance Tiable a => Tiable (TremoloT a) where
    toTied = unzipR . fmap toTied
type instance Pitch (TremoloT a)        = Pitch a
type instance SetPitch g (TremoloT a)   = TremoloT (SetPitch g a)
instance (HasPitches a b) => HasPitches (TremoloT a) (TremoloT b) where
  pitches = _Wrapped . pitches
instance (HasPitch a b) => HasPitch (TremoloT a) (TremoloT b) where
  pitch = _Wrapped . pitch

type instance Dynamic (TremoloT a)        = Dynamic a
type instance SetDynamic g (TremoloT a)   = TremoloT (SetDynamic g a)
instance (HasDynamics a b) => HasDynamics (TremoloT a) (TremoloT b) where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b) => HasDynamic (TremoloT a) (TremoloT b) where
  dynamic = _Wrapped . dynamic

type instance Articulation (TremoloT a)        = Articulation a
type instance SetArticulation g (TremoloT a)   = TremoloT (SetArticulation g a)
instance (HasArticulations a b) => HasArticulations (TremoloT a) (TremoloT b) where
  articulations = _Wrapped . articulations
instance (HasArticulation a b) => HasArticulation (TremoloT a) (TremoloT b) where
  articulation = _Wrapped . articulation


type instance Part (TremoloT a) = Part a
deriving instance HasHarmonic a => HasHarmonic (TremoloT a)
deriving instance HasSlide a => HasSlide (TremoloT a)
deriving instance HasText a => HasText (TremoloT a)

deriving instance Transformable a => Transformable (TremoloT a)
deriving instance Reversible a => Reversible (TremoloT a)
deriving instance Alterable a => Alterable (TremoloT a)
deriving instance Augmentable a => Augmentable (TremoloT a)

-- |
-- Extract tremolo. Useful for backends.
--
getTremolo :: TremoloT a -> (Int, a)
getTremolo (TremoloT (Couple (Max n, a))) = (fromIntegral n, a)

-- |
-- Set the number of tremolo divisions for all notes in the score.
--
tremolo :: HasTremolo a => Int -> a -> a
tremolo = setTrem

