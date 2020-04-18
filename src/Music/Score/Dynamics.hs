{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports #-}

-- |  Provides functions for manipulating dynamics.
module Music.Score.Dynamics
  ( -- * Dynamic type functions
    Dynamic,
    Level,
    SetDynamic,
    DynamicLensLaws',
    DynamicLensLaws,

    -- * Accessing dynamics
    HasDynamics (..),
    HasDynamic (..),
    HasDynamics',
    HasDynamic',
    dynamic',
    dynamics',

    -- * Attenuable class
    Attenuable,

    -- * Manipulating dynamics
    level,
    louder,
    softer,
    cresc,
    dim,
    volume,
    compressor,
    compressUp,
    compressDown,
    fadeIn,
    fadeOut,
    DynamicT (..),

    -- * Context
    vdynamic,
    addDynCon,
  )
where

import BasePrelude hiding ((<>), Dynamic, first, second)
import Control.Comonad
import Control.Lens hiding ((&), Level, transform)
import Data.AffineSpace
import Data.AffineSpace.Point (relative)
import Data.Functor.Context
import Data.Functor.Couple
import Data.Kind
import qualified Data.List as List
import Data.Semigroup
import Data.VectorSpace hiding (Sum)
import Music.Dynamics.Literal
import Music.Pitch.Literal
import Music.Score.Harmonics
import Music.Score.Internal.Util (through)
import Music.Score.Part
import Music.Score.Phrases
import Music.Score.Slide
import Music.Score.Text
import Music.Score.Ties
import Music.Time
import Music.Time.Internal.Transform

-- |
-- Dynamics type.
type family Dynamic (s :: Type) :: Type

-- |
-- Dynamic type.
type family SetDynamic (b :: Type) (s :: Type) :: Type

-- |
-- Class of types that provide a single dynamic.
class (HasDynamics s t) => HasDynamic s t where
  -- | Access a single dynamic.
  dynamic :: Lens s t (Dynamic s) (Dynamic t)

type DynamicLensLaws' s t a b =
  ( Dynamic (SetDynamic a s) ~ a,
    SetDynamic (Dynamic t) s ~ t,
    SetDynamic a (SetDynamic b s) ~ SetDynamic a s
  )

type DynamicLensLaws s t = DynamicLensLaws' s t (Dynamic s) (Dynamic t)

-- |
-- Class of types that provide a dynamic traversal.
class
  ( Transformable (Dynamic s),
    Transformable (Dynamic t),
    -- SetDynamic (Dynamic t) s ~ t
    DynamicLensLaws s t
  ) =>
  HasDynamics s t where
  -- | Access all dynamics.
  dynamics :: Traversal s t (Dynamic s) (Dynamic t)

type HasDynamic' a = HasDynamic a a

type HasDynamics' a = HasDynamics a a

-- | Access a single dynamic.
dynamic' :: (HasDynamic s t, s ~ t) => Lens' s (Dynamic s)
dynamic' = dynamic

-- | Access all dynamics.
dynamics' :: (HasDynamics s t, s ~ t) => Traversal' s (Dynamic s)
dynamics' = dynamics

type instance Dynamic (c, a) = Dynamic a

type instance SetDynamic b (c, a) = (c, SetDynamic b a)

type instance Dynamic [a] = Dynamic a

type instance SetDynamic b [a] = [SetDynamic b a]

type instance Dynamic (Maybe a) = Dynamic a

type instance SetDynamic b (Maybe a) = Maybe (SetDynamic b a)

type instance Dynamic (Either c a) = Dynamic a

type instance SetDynamic b (Either c a) = Either c (SetDynamic b a)

type instance Dynamic (Event a) = Dynamic a

type instance SetDynamic b (Event a) = Event (SetDynamic b a)

type instance Dynamic (Placed a) = Dynamic a

type instance SetDynamic b (Placed a) = Placed (SetDynamic b a)

type instance Dynamic (Note a) = Dynamic a

type instance SetDynamic b (Note a) = Note (SetDynamic b a)

type instance Dynamic (Voice a) = Dynamic a

type instance SetDynamic b (Voice a) = Voice (SetDynamic b a)

type instance Dynamic (Track a) = Dynamic a

type instance SetDynamic b (Track a) = Track (SetDynamic b a)

type instance Dynamic (Score a) = Dynamic a

type instance SetDynamic b (Score a) = Score (SetDynamic b a)

type instance Dynamic (Aligned a) = Dynamic a

type instance SetDynamic b (Aligned a) = Aligned (SetDynamic b a)

instance HasDynamics a b => HasDynamics (Aligned a) (Aligned b) where
  dynamics = _Wrapped . dynamics

instance HasDynamic a b => HasDynamic (c, a) (c, b) where
  dynamic = _2 . dynamic

instance HasDynamics a b => HasDynamics (c, a) (c, b) where
  dynamics = traverse . dynamics

instance HasDynamics a b => HasDynamics [a] [b] where
  dynamics = traverse . dynamics

instance HasDynamics a b => HasDynamics (Maybe a) (Maybe b) where
  dynamics = traverse . dynamics

instance HasDynamics a b => HasDynamics (Either c a) (Either c b) where
  dynamics = traverse . dynamics

instance (HasDynamics a b) => HasDynamics (Event a) (Event b) where
  dynamics = from event . whilstL dynamics

instance (HasDynamic a b) => HasDynamic (Event a) (Event b) where
  dynamic = from event . whilstL dynamic

instance (HasDynamics a b) => HasDynamics (Placed a) (Placed b) where
  dynamics = _Wrapped . whilstLT dynamics

instance (HasDynamic a b) => HasDynamic (Placed a) (Placed b) where
  dynamic = _Wrapped . whilstLT dynamic

instance (HasDynamics a b) => HasDynamics (Note a) (Note b) where
  dynamics = _Wrapped . whilstLD dynamics

instance (HasDynamic a b) => HasDynamic (Note a) (Note b) where
  dynamic = _Wrapped . whilstLD dynamic

instance HasDynamics a b => HasDynamics (Voice a) (Voice b) where
  dynamics = traverse . dynamics

instance HasDynamics a b => HasDynamics (Track a) (Track b) where
  dynamics = traverse . dynamics

{-
type instance Dynamic (Chord a)       = Dynamic a
type instance SetDynamic b (Chord a)  = Chord (SetDynamic b a)
instance HasDynamics a b => HasDynamics (Chord a) (Chord b) where
  dynamics = traverse . dynamics
-}

instance (HasDynamics a b) => HasDynamics (Score a) (Score b) where
  dynamics = traverse . dynamics

type instance Dynamic (Behavior a) = Behavior a

type instance SetDynamic b (Behavior a) = b

instance
  ( Transformable b,
    b ~ Dynamic b,
    SetDynamic (Behavior a) b ~ Behavior a
  ) =>
  HasDynamics (Behavior a) b
  where
  dynamics = ($)

instance
  ( Transformable b,
    b ~ Dynamic b,
    SetDynamic (Behavior a) b ~ Behavior a
  ) =>
  HasDynamic (Behavior a) b
  where
  dynamic = ($)

type instance Dynamic (Couple c a) = Dynamic a

type instance SetDynamic g (Couple c a) = Couple c (SetDynamic g a)

type instance Dynamic (TextT a) = Dynamic a

type instance SetDynamic g (TextT a) = TextT (SetDynamic g a)

type instance Dynamic (HarmonicT a) = Dynamic a

type instance SetDynamic g (HarmonicT a) = HarmonicT (SetDynamic g a)

type instance Dynamic (TieT a) = Dynamic a

type instance SetDynamic g (TieT a) = TieT (SetDynamic g a)

type instance Dynamic (SlideT a) = Dynamic a

type instance SetDynamic g (SlideT a) = SlideT (SetDynamic g a)

instance (HasDynamics a b) => HasDynamics (Couple c a) (Couple c b) where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b) => HasDynamic (Couple c a) (Couple c b) where
  dynamic = _Wrapped . dynamic

instance (HasDynamics a b) => HasDynamics (TextT a) (TextT b) where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b) => HasDynamic (TextT a) (TextT b) where
  dynamic = _Wrapped . dynamic

instance (HasDynamics a b) => HasDynamics (HarmonicT a) (HarmonicT b) where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b) => HasDynamic (HarmonicT a) (HarmonicT b) where
  dynamic = _Wrapped . dynamic

instance (HasDynamics a b) => HasDynamics (TieT a) (TieT b) where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b) => HasDynamic (TieT a) (TieT b) where
  dynamic = _Wrapped . dynamic

instance (HasDynamics a b) => HasDynamics (SlideT a) (SlideT b) where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b) => HasDynamic (SlideT a) (SlideT b) where
  dynamic = _Wrapped . dynamic

-- |
-- The 'Level' type represents difference in dynamics.
type Level a = Diff (Dynamic a)

-- | Set dynamic level, overwriting previous value.
level :: Attenuable a => Dynamic a -> a -> a
level a = dynamics .~ a

-- |
-- Class of types that can be modified dynamically.
type Attenuable a =
  ( HasDynamics a a,
    VectorSpace (Level a),
    AffineSpace (Dynamic a)
  )

-- |
-- Increase dynamics (linear).
-- For standard notation, switches dynamic marks the given number of step upwards.
--
-- >>> louder 1 (mp :: Dynamics)
-- _p
--
-- >>> louder 1 (ff :: Dynamics)
-- fff
--
-- >>> louder 5 [ppp,mf :: Dynamics]
-- [_f, fffff]
louder :: Attenuable a => Level a -> a -> a
louder a = dynamics %~ (.+^ a)

-- >>> louder 1 (level ff cs :: DynamicT Dynamics (Sum Pitch))
-- DynamicT {getDynamicT = (fff,Sum {getSum = cs})}
--
-- >>> louder 1 (level ff cs :: Note (DynamicT Dynamics (Sum Pitch)))
-- (1,DynamicT {getDynamicT = (fff,Sum {getSum = cs})})^.note
--

-- | Decrease dynamic (linear).
--   For standard notation, switches dynamic marks the given number of step downwards.
softer :: Attenuable a => Level a -> a -> a
softer a = dynamics %~ (.-^ a)

-- | Scale dynamic values.
volume :: (Num (Dynamic t), HasDynamics s t, Dynamic s ~ Dynamic t) => Dynamic t -> s -> t
volume a = dynamics *~ a

-- | Compress dynamics upwards.
--
-- >>> compressUp mp 2 [ppp,pp,_p,mp,mf,_f,ff,fff::Dynamics]
-- [ppp,pp,_p,mp,mp,fff,fffff,fffffff]
--
-- >>> compressUp 0 (1/2) (0.2 :: Amplitude)
-- Amplitude {getAmplitude = 0.1}
compressUp ::
  (Attenuable a, Ord (Level a), Num (Level a)) =>
  -- | Threshold
  Dynamic a ->
  -- | Ratio
  Scalar (Level a) ->
  -- | Value to compress
  a ->
  a
compressUp th r = over dynamics (relative th $ \x -> if x < 0 then x else x ^* r)

-- | Compress dynamics downwards.
--
-- >>> compressUp mp 2 [ppp,pp,_p,mp,mf,_f,ff,fff::Dynamics]
-- [ppp,pp,_p,mp,mp,fff,fffff,fffffff]
--
-- >>> compressDown 0 1.5 (-0.2 :: Amplitude)
-- Amplitude {getAmplitude = -0.30000000000000004}
compressDown ::
  (Attenuable a, Ord (Level a), Num (Level a)) =>
  -- | Threshold
  Dynamic a ->
  -- | Ratio
  Scalar (Level a) ->
  -- | Value to compress
  a ->
  a
compressDown th r = over dynamics (relative th $ \x -> if x > 0 then x else x ^* r)

compressor ::
  (Attenuable a, Ord (Level a), Num (Level a)) =>
  -- | Threshold
  Dynamic a ->
  -- | Ratio
  Scalar (Level a) ->
  -- | Value to compress
  a ->
  a
compressor = compressUp
{-# DEPRECATED compressor "Use compressUp (or compressDown)" #-}

--
-- TODO non-linear fades etc
--

cresc :: (Attenuable a, Fractional (Scalar (Level a))) => Dynamic a -> Dynamic a -> Voice a -> Voice a
cresc a b x = stretchToD (_duration x) $ cresc' a b (stretchToD 1 x)

cresc' :: (Attenuable a, Fractional (Scalar (Level a))) => Dynamic a -> Dynamic a -> Voice a -> Voice a
cresc' a b = setLevelWithAlignment (\t -> alerp a b (realToFrac t))

setLevelWithAlignment :: (Attenuable a) => (Duration -> Dynamic a) -> Voice a -> Voice a
setLevelWithAlignment f = mapWithOnsetRelative 0 (\t x -> level (f (t .-. 0)) x)

dim :: (Attenuable a, Fractional (Scalar (Level a))) => Dynamic a -> Dynamic a -> Voice a -> Voice a
dim = cresc

-- |
-- Fade in.
fadeIn :: (HasPosition1 a, Transformable a, HasDynamics' a, Dynamic a ~ Behavior c, Fractional c) => Duration -> a -> a
fadeIn d x = x & dynamics *~ ((x ^. onset >-> d) `transform` unit)

-- |
-- Fade in.
fadeOut :: (HasPosition1 a, Transformable a, HasDynamics' a, Dynamic a ~ Behavior c, Fractional c) => Duration -> a -> a
fadeOut d x = x & dynamics *~ ((d <-< (x ^. offset)) `transform` revB unit)

revB :: Behavior a -> Behavior a
revB = stretch (-1)

newtype DynamicT n a = DynamicT {getDynamicT :: (n, a)}
  deriving
    ( Eq,
      Ord,
      Show,
      Typeable,
      Functor,
      Applicative,
      Monad,
      Comonad,
      Transformable,
      Monoid,
      Semigroup
    )

instance (Monoid n, Num a) => Num (DynamicT n a) where

  (+) = liftA2 (+)

  (*) = liftA2 (*)

  (-) = liftA2 (-)

  abs = fmap abs

  signum = fmap signum

  fromInteger = pure . fromInteger

instance (Monoid n, Fractional a) => Fractional (DynamicT n a) where

  recip = fmap recip

  fromRational = pure . fromRational

instance (Monoid n, Floating a) => Floating (DynamicT n a) where

  pi = pure pi

  sqrt = fmap sqrt

  exp = fmap exp

  log = fmap log

  sin = fmap sin

  cos = fmap cos

  asin = fmap asin

  atan = fmap atan

  acos = fmap acos

  sinh = fmap sinh

  cosh = fmap cosh

  asinh = fmap asinh

  atanh = fmap atanh

  acosh = fmap acos

instance (Monoid n, Enum a) => Enum (DynamicT n a) where

  toEnum = pure . toEnum

  fromEnum = fromEnum . extract

instance (Monoid n, Bounded a) => Bounded (DynamicT n a) where

  minBound = pure minBound

  maxBound = pure maxBound

-- instance (Monoid n, Num a, Ord a, Real a) => Real (DynamicT n a) where
--     toRational = toRational . extract
--
-- instance (Monoid n, Real a, Enum a, Integral a) => Integral (DynamicT n a) where
--     quot = liftA2 quot
--     rem = liftA2 rem
--     toInteger = toInteger . extract

instance Wrapped (DynamicT p a) where

  type Unwrapped (DynamicT p a) = (p, a)

  _Wrapped' = iso getDynamicT DynamicT

instance Rewrapped (DynamicT p a) (DynamicT p' b)

type instance Dynamic (DynamicT p a) = p

type instance SetDynamic p' (DynamicT p a) = DynamicT p' a

instance
  (Transformable p, Transformable p') =>
  HasDynamic (DynamicT p a) (DynamicT p' a)
  where
  dynamic = _Wrapped . _1

instance
  (Transformable p, Transformable p') =>
  HasDynamics (DynamicT p a) (DynamicT p' a)
  where
  dynamics = _Wrapped . _1

deriving instance (IsPitch a, Monoid n) => IsPitch (DynamicT n a)

deriving instance (IsInterval a, Monoid n) => IsInterval (DynamicT n a)

instance (IsDynamics n, Monoid a) => IsDynamics (DynamicT n a) where
  fromDynamics l = DynamicT (fromDynamics l, mempty)

instance (Tiable n, Tiable a) => Tiable (DynamicT n a) where
  toTied (DynamicT (d, a)) = (DynamicT (d1, a1), DynamicT (d2, a2))
    where
      (a1, a2) = toTied a
      (d1, d2) = toTied d

-- JUNK

-- |
-- View just the dynamices in a voice.
vdynamic ::
  ({-SetDynamic (Dynamic t) s ~ t,-} HasDynamic a a, HasDynamic a b) =>
  Lens (Voice a) (Voice b) (Voice (Dynamic a)) (Voice (Dynamic b))
vdynamic = lens (fmap $ view dynamic) (flip $ zipVoiceWithNoScale (set dynamic))

-- vdynamic = through dynamic dynamic

addDynCon ::
  ( HasPhrases s t a b,
    HasDynamic a a,
    HasDynamic a b,
    Dynamic a ~ d,
    Dynamic b ~ Ctxt d
  ) =>
  s ->
  t
addDynCon = over (phrases . vdynamic) withContext
{-
-- | Specialized verion of 'phrases' for
phrasesVoice :: Traversal (MVoice a) (MVoice b) (Phrase a) (Phrase b)
phrasesVoice  =
  mVoicePVoice . each . _Right
-}
