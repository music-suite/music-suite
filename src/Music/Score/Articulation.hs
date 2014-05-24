
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

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
-- Provides functions for manipulating articulation.
--
-------------------------------------------------------------------------------------


module Music.Score.Articulation (
        -- ** Articulation type functions
        Articulation,
        SetArticulation,
        Accentuation,
        Separation,
        -- ** Accessing articulation
        HasArticulations(..),
        HasArticulation(..),
        articulation',
        articulations',
        -- * Manipulating articulation
        Articulated,
        accent,
        marcato,
        accentLast,
        marcatoLast,
        accentAll,
        marcatoAll,

        tenuto,
        separated,
        staccato,
        portato,
        legato,
        spiccato,

        ArticulationT(..),
  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens                  hiding (above, below, transform)
import           Data.AffineSpace
import           Data.Foldable
import           Data.Functor.Couple
import           Data.Semigroup
import           Data.Typeable
import           Data.VectorSpace              hiding (Sum)

import           Music.Score.Part
import           Music.Time
import           Music.Time.Internal.Transform

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Score.Harmonics
import           Music.Score.Part
import           Music.Score.Phrases
import           Music.Score.Slide
import           Music.Score.Text
import           Music.Score.Ties
import           Music.Score.Tremolo




-- |
-- Articulations type.
--
type family Articulation (s :: *) :: *

-- |
-- Articulation type.
--
type family SetArticulation (b :: *) (s :: *) :: *

-- |
-- Class of types that provide a single articulation.
--
class (HasArticulations s t) => HasArticulation s t where

  -- | Articulation type.
  articulation :: Lens s t (Articulation s) (Articulation t)

-- |
-- Class of types that provide a articulation traversal.
--
class (Transformable (Articulation s),
       Transformable (Articulation t),
       SetArticulation (Articulation t) s ~ t) => HasArticulations s t where

  -- | Articulation type.
  articulations :: Traversal s t (Articulation s) (Articulation t)

-- |
-- Articulation type.
--
articulation' :: (HasArticulation s t, s ~ t) => Lens' s (Articulation s)
articulation' = articulation

-- |
-- Articulation type.
--
articulations' :: (HasArticulations s t, s ~ t) => Traversal' s (Articulation s)
articulations' = articulations

#define PRIM_ARTICULATION_INSTANCE(TYPE)       \
                                               \
type instance Articulation TYPE = TYPE;        \
type instance SetArticulation a TYPE = a;      \
                                               \
instance (Transformable a, a ~ Articulation a) \
  => HasArticulation TYPE a where {            \
  articulation = ($)              } ;          \
                                               \
instance (Transformable a, a ~ Articulation a) \
  => HasArticulations TYPE a where {           \
  articulations = ($)               } ;        \

PRIM_ARTICULATION_INSTANCE(())
PRIM_ARTICULATION_INSTANCE(Bool)
PRIM_ARTICULATION_INSTANCE(Ordering)
PRIM_ARTICULATION_INSTANCE(Char)
PRIM_ARTICULATION_INSTANCE(Int)
PRIM_ARTICULATION_INSTANCE(Integer)
PRIM_ARTICULATION_INSTANCE(Float)
PRIM_ARTICULATION_INSTANCE(Double)


type instance Articulation (c,a) = Articulation a
type instance SetArticulation b (c,a) = (c,SetArticulation b a)

instance HasArticulation a b => HasArticulation (c, a) (c, b) where
  articulation = _2 . articulation

instance HasArticulations a b => HasArticulations (c, a) (c, b) where
  articulations = traverse . articulations


type instance Articulation [a] = Articulation a
type instance SetArticulation b [a] = [SetArticulation b a]

instance HasArticulations a b => HasArticulations [a] [b] where
  articulations = traverse . articulations


type instance Articulation (Note a) = Articulation a
type instance SetArticulation g (Note a) = Note (SetArticulation g a)

instance (HasArticulation a b) => HasArticulation (Note a) (Note b) where
  articulation = _Wrapped . whilstL articulation

instance (HasArticulations a b) => HasArticulations (Note a) (Note b) where
  articulations = _Wrapped . whilstL articulations

type instance Articulation (Score a) = Articulation a
type instance SetArticulation b (Score a) = Score (SetArticulation b a)
instance HasArticulations a b => HasArticulations (Score a) (Score b) where
  articulations =
    _Wrapped . _2   -- into NScore
    . _Wrapped
    . traverse
    . _Wrapped      -- this needed?
    . whilstL articulations

type instance Articulation (Couple c a)        = Articulation a
type instance SetArticulation g (Couple c a)   = Couple c (SetArticulation g a)
type instance Articulation (TremoloT a)        = Articulation a
type instance SetArticulation g (TremoloT a)   = TremoloT (SetArticulation g a)
type instance Articulation (TextT a)           = Articulation a
type instance SetArticulation g (TextT a)      = TextT (SetArticulation g a)
type instance Articulation (HarmonicT a)       = Articulation a
type instance SetArticulation g (HarmonicT a)  = HarmonicT (SetArticulation g a)
type instance Articulation (TieT a)            = Articulation a
type instance SetArticulation g (TieT a)       = TieT (SetArticulation g a)
type instance Articulation (SlideT a)          = Articulation a
type instance SetArticulation g (SlideT a)     = SlideT (SetArticulation g a)

instance (HasArticulations a b) => HasArticulations (Couple c a) (Couple c b) where
  articulations = _Wrapped . articulations
instance (HasArticulation a b) => HasArticulation (Couple c a) (Couple c b) where
  articulation = _Wrapped . articulation

instance (HasArticulations a b) => HasArticulations (TremoloT a) (TremoloT b) where
  articulations = _Wrapped . articulations
instance (HasArticulation a b) => HasArticulation (TremoloT a) (TremoloT b) where
  articulation = _Wrapped . articulation

instance (HasArticulations a b) => HasArticulations (TextT a) (TextT b) where
  articulations = _Wrapped . articulations
instance (HasArticulation a b) => HasArticulation (TextT a) (TextT b) where
  articulation = _Wrapped . articulation

instance (HasArticulations a b) => HasArticulations (HarmonicT a) (HarmonicT b) where
  articulations = _Wrapped . articulations
instance (HasArticulation a b) => HasArticulation (HarmonicT a) (HarmonicT b) where
  articulation = _Wrapped . articulation

instance (HasArticulations a b) => HasArticulations (TieT a) (TieT b) where
  articulations = _Wrapped . articulations
instance (HasArticulation a b) => HasArticulation (TieT a) (TieT b) where
  articulation = _Wrapped . articulation

instance (HasArticulations a b) => HasArticulations (SlideT a) (SlideT b) where
  articulations = _Wrapped . articulations
instance (HasArticulation a b) => HasArticulation (SlideT a) (SlideT b) where
  articulation = _Wrapped . articulation


type family Accentuation (a :: *) :: *

type family Separation (a :: *) :: *

type instance Accentuation () = ()
type instance Separation   () = ()
type instance Accentuation (Double, Double) = Double
type instance Separation   (Double, Double) = Double

-- TODO move
instance VectorSpace () where
  type Scalar () = ()
  _ *^ _ = ()
instance AffineSpace () where
  type Diff () = ()
  _ .-. _ = ()
  _ .+^ _ = ()

-- |
-- Class of types that can be transposed, inverted and so on.
--
type Articulated a
  = (HasArticulations a a,
     AffineSpace (Accentuation (Articulation a)),
     AffineSpace (Separation (Articulation a)))


-- accent = error "Not implemented: accent"
-- marcato = error "Not implemented: marcato"
-- accentLast = error "Not implemented: accentLast"
-- marcatoLast = error "Not implemented: marcatoLast"
-- accentAll = error "Not implemented: accentAll"
-- marcatoAll = error "Not implemented: marcatoAll"
--
-- tenuto = error "Not implemented: tenuto"
-- separated = error "Not implemented: separated"
-- staccato = error "Not implemented: staccato"
-- portato = error "Not implemented: portato"
-- legato = error "Not implemented: legato"
-- spiccato = error "Not implemented: spiccato"

--
-- TODO use phrase-wise traversal here
-- The constraint becomes (HasVoices a b, Articulated b) ... or similar
--

accent :: Articulated a => a -> a
accent = id

marcato :: Articulated a => a -> a
marcato = id

accentLast :: Articulated a => a -> a
accentLast = id

marcatoLast :: Articulated a => a -> a
marcatoLast = id

accentAll :: Articulated a => a -> a
accentAll = id

marcatoAll :: Articulated a => a -> a
marcatoAll = id


tenuto :: Articulated a => a -> a
tenuto = id

separated :: Articulated a => a -> a
separated = id

staccato :: Articulated a => a -> a
staccato = id

portato :: Articulated a => a -> a
portato = id

legato :: Articulated a => a -> a
legato = id

spiccato :: Articulated a => a -> a
spiccato = id


newtype ArticulationT n a = ArticulationT { getArticulationT :: (n, a) }
  deriving (
    Eq, Ord, Show, Typeable, Functor, Applicative, Monad, 
    Comonad, Transformable, Monoid, Semigroup
    )

instance (Monoid n, Num a) => Num (ArticulationT n a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Monoid n, Fractional a) => Fractional (ArticulationT n a) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance (Monoid n, Floating a) => Floating (ArticulationT n a) where
  pi    = pure pi
  sqrt  = fmap sqrt
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acos

instance (Monoid n, Enum a) => Enum (ArticulationT n a) where
  toEnum = pure . toEnum
  fromEnum = fromEnum . extract

instance (Monoid n, Bounded a) => Bounded (ArticulationT n a) where
  minBound = pure minBound
  maxBound = pure maxBound

-- instance (Monoid n, Num a, Ord a, Real a) => Real (ArticulationT n a) where
--     toRational = toRational . extract
--
-- instance (Monoid n, Real a, Enum a, Integral a) => Integral (ArticulationT n a) where
--     quot = liftA2 quot
--     rem = liftA2 rem
--     toInteger = toInteger . extract

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (ArticulationT p a) where
  type Unwrapped (ArticulationT p a) = (p, a)
  _Wrapped' = iso getArticulationT ArticulationT

instance Rewrapped (ArticulationT p a) (ArticulationT p' b)

type instance Articulation (ArticulationT p a) = p
type instance SetArticulation p' (ArticulationT p a) = ArticulationT p' a

instance (Transformable p, Transformable p') 
    => HasArticulation (ArticulationT p a) (ArticulationT p' a) where
  articulation = _Wrapped . _1

instance (Transformable p, Transformable p') 
    => HasArticulations (ArticulationT p a) (ArticulationT p' a) where
  articulations = _Wrapped . _1

deriving instance (IsPitch a, Monoid n) => IsPitch (ArticulationT n a)
deriving instance (IsInterval a, Monoid n) => IsInterval (ArticulationT n a)
deriving instance Reversible a => Reversible (ArticulationT p a)

instance (Tiable n, Tiable a) => Tiable (ArticulationT n a) where
  toTied (ArticulationT (d,a)) = (ArticulationT (d1,a1), ArticulationT (d2,a2))
    where
      (a1,a2) = toTied a
      (d1,d2) = toTied d


