{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports #-}

-- |  Provides functions for manipulating articulation.
module Music.Score.Articulation
  ( -- ** Articulation type functions
    Articulation,
    SetArticulation,
    Accentuation,
    Separation,
    Articulated (..),

    -- ** Accessing articulation
    HasArticulations (..),
    HasArticulation (..),
    HasArticulations',
    HasArticulation',
    articulation',
    articulations',

    -- * Manipulating articulation

    -- ** Accents
    accent,
    marcato,
    accentLast,
    marcatoLast,
    accentAll,
    marcatoAll,

    -- ** Phrasing and separation
    staccatissimo,
    staccato,
    separated,
    portato,
    legato,
    legatissimo,
    tenuto,
    spiccato,

    -- * Articulation transformer
    ArticulationT (..),

    -- * Context
    varticulation,
    addArtCon,
  )
where

import BasePrelude hiding ((<>), Dynamic, first, second)
import Control.Comonad
import Control.Lens hiding ((&), below, transform)
import Data.AffineSpace
import Data.Functor.Context
import Data.Functor.Couple
import Data.Kind
import Data.Semigroup
import Data.VectorSpace hiding (Sum)
import Music.Dynamics.Literal
import Music.Pitch.Literal
import Music.Score.Harmonics
import Music.Score.Part
import Music.Score.Part
import Music.Score.Phrases
import Music.Score.Slide
import Music.Score.Text
import Music.Score.Ties
import Music.Time
import Music.Time.Internal.Transform

-- |
-- Articulations type.
type family Articulation (s :: Type) :: Type

-- |
-- Articulation type.
type family SetArticulation (b :: Type) (s :: Type) :: Type

type ArticulationLensLaws' s t a b =
  ( Articulation (SetArticulation a s) ~ a,
    SetArticulation (Articulation t) s ~ t,
    SetArticulation a (SetArticulation b s) ~ SetArticulation a s
  )

type ArticulationLensLaws s t = ArticulationLensLaws' s t (Articulation s) (Articulation t)

-- |
-- Class of types that provide a single articulation.
class (HasArticulations s t) => HasArticulation s t where
  -- | Articulation type.
  articulation :: Lens s t (Articulation s) (Articulation t)

-- |
-- Class of types that provide a articulation traversal.
class
  ( Transformable (Articulation s),
    Transformable (Articulation t),
    ArticulationLensLaws s t
  ) =>
  HasArticulations s t where
  -- | Articulation type.
  articulations :: Traversal s t (Articulation s) (Articulation t)

type HasArticulation' a = HasArticulation a a

type HasArticulations' a = HasArticulations a a

-- |
-- Articulation type.
articulation' :: (HasArticulation s t, s ~ t) => Lens' s (Articulation s)
articulation' = articulation

-- |
-- Articulation type.
articulations' :: (HasArticulations s t, s ~ t) => Traversal' s (Articulation s)
articulations' = articulations

type instance Articulation (c, a) = Articulation a

type instance SetArticulation b (c, a) = (c, SetArticulation b a)

type instance Articulation [a] = Articulation a

type instance SetArticulation b [a] = [SetArticulation b a]

type instance Articulation (Maybe a) = Articulation a

type instance SetArticulation b (Maybe a) = Maybe (SetArticulation b a)

type instance Articulation (Either c a) = Articulation a

type instance SetArticulation b (Either c a) = Either c (SetArticulation b a)

type instance Articulation (Event a) = Articulation a

type instance SetArticulation g (Event a) = Event (SetArticulation g a)

type instance Articulation (Placed a) = Articulation a

type instance SetArticulation g (Placed a) = Placed (SetArticulation g a)

type instance Articulation (Note a) = Articulation a

type instance SetArticulation g (Note a) = Note (SetArticulation g a)

type instance Articulation (Voice a) = Articulation a

type instance SetArticulation b (Voice a) = Voice (SetArticulation b a)

type instance Articulation (Track a) = Articulation a

type instance SetArticulation b (Track a) = Track (SetArticulation b a)

type instance Articulation (Score a) = Articulation a

type instance SetArticulation b (Score a) = Score (SetArticulation b a)

instance HasArticulation a b => HasArticulation (c, a) (c, b) where
  articulation = _2 . articulation

instance HasArticulations a b => HasArticulations (c, a) (c, b) where
  articulations = traverse . articulations

instance HasArticulations a b => HasArticulations [a] [b] where
  articulations = traverse . articulations

instance HasArticulations a b => HasArticulations (Maybe a) (Maybe b) where
  articulations = traverse . articulations

instance HasArticulations a b => HasArticulations (Either c a) (Either c b) where
  articulations = traverse . articulations

instance (HasArticulations a b) => HasArticulations (Event a) (Event b) where
  articulations = from event . whilstL articulations

instance (HasArticulation a b) => HasArticulation (Event a) (Event b) where
  articulation = from event . whilstL articulation

instance (HasArticulations a b) => HasArticulations (Placed a) (Placed b) where
  articulations = _Wrapped . whilstLT articulations

instance (HasArticulation a b) => HasArticulation (Placed a) (Placed b) where
  articulation = _Wrapped . whilstLT articulation

instance (HasArticulations a b) => HasArticulations (Note a) (Note b) where
  articulations = _Wrapped . whilstLD articulations

instance (HasArticulation a b) => HasArticulation (Note a) (Note b) where
  articulation = _Wrapped . whilstLD articulation

instance HasArticulations a b => HasArticulations (Voice a) (Voice b) where
  articulations = traverse . articulations

{-
type instance Articulation (Chord a) = Articulation a
type instance SetArticulation b (Chord a) = Chord (SetArticulation b a)
instance HasArticulations a b => HasArticulations (Chord a) (Chord b) where
  articulations = traverse . articulations
-}

instance HasArticulations a b => HasArticulations (Track a) (Track b) where
  articulations = traverse . articulations

instance HasArticulations a b => HasArticulations (Score a) (Score b) where
  articulations = traverse . articulations

type instance Articulation (Couple c a) = Articulation a

type instance SetArticulation g (Couple c a) = Couple c (SetArticulation g a)

type instance Articulation (TextT a) = Articulation a

type instance SetArticulation g (TextT a) = TextT (SetArticulation g a)

type instance Articulation (HarmonicT a) = Articulation a

type instance SetArticulation g (HarmonicT a) = HarmonicT (SetArticulation g a)

type instance Articulation (TieT a) = Articulation a

type instance SetArticulation g (TieT a) = TieT (SetArticulation g a)

type instance Articulation (SlideT a) = Articulation a

type instance SetArticulation g (SlideT a) = SlideT (SetArticulation g a)

instance (HasArticulations a b) => HasArticulations (Couple c a) (Couple c b) where
  articulations = _Wrapped . articulations

instance (HasArticulation a b) => HasArticulation (Couple c a) (Couple c b) where
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

type family Accentuation (a :: Type) :: Type

type family Separation (a :: Type) :: Type

type instance Accentuation () = ()

type instance Separation () = ()

type instance Accentuation (a, b) = a

type instance Separation (a, b) = b

-- |
-- Class of types that can be transposed, inverted and so on.
class
  ( Fractional (Accentuation a),
    Fractional (Separation a),
    AffineSpace (Accentuation a),
    AffineSpace (Separation a)
  ) =>
  Articulated a where

  accentuation :: Lens' a (Accentuation a)

  separation :: Lens' a (Separation a)

instance (AffineSpace a, AffineSpace b, Fractional a, Fractional b) => Articulated (a, b) where

  accentuation = _1

  separation = _2

accent :: (HasPhrases' s b, HasArticulations' b, Articulation b ~ a, Articulated a) => s -> s
accent = set (phrases . _head . articulations . accentuation) 1

marcato :: (HasPhrases' s b, HasArticulations' b, Articulation b ~ a, Articulated a) => s -> s
marcato = set (phrases . _head . articulations . accentuation) 2

accentLast :: (HasPhrases' s b, HasArticulations' b, Articulation b ~ a, Articulated a) => s -> s
accentLast = set (phrases . _last . articulations . accentuation) 1

marcatoLast :: (HasPhrases' s b, HasArticulations' b, Articulation b ~ a, Articulated a) => s -> s
marcatoLast = set (phrases . _last . articulations . accentuation) 2

accentAll :: (HasArticulations' s, Articulation s ~ a, Articulated a) => s -> s
accentAll = set (articulations . accentuation) 1

marcatoAll :: (HasArticulations' s, Articulation s ~ a, Articulated a) => s -> s
marcatoAll = set (articulations . accentuation) 2

tenuto :: (HasArticulations' s, Articulation s ~ a, Articulated a) => s -> s
tenuto = legato -- TODO

spiccato :: (HasArticulations' s, Articulation s ~ a, Articulated a) => s -> s
spiccato = legato -- TODO

legatissimo :: (HasArticulations' s, Articulation s ~ a, Articulated a) => s -> s
legatissimo = set (articulations . separation) (-2)

legato :: (HasArticulations' s, Articulation s ~ a, Articulated a) => s -> s
legato = set (articulations . separation) (-1)

separated :: (HasArticulations' s, Articulation s ~ a, Articulated a) => s -> s
separated = set (articulations . separation) 0

portato :: (HasArticulations' s, Articulation s ~ a, Articulated a) => s -> s
portato = set (articulations . separation) 0.5

staccato :: (HasArticulations' s, Articulation s ~ a, Articulated a) => s -> s
staccato = set (articulations . separation) 1

staccatissimo :: (HasArticulations' s, Articulation s ~ a, Articulated a) => s -> s
staccatissimo = set (articulations . separation) 2

newtype ArticulationT n a = ArticulationT {getArticulationT :: (n, a)}
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

instance (Monoid n, Num a) => Num (ArticulationT n a) where

  (+) = liftA2 (+)

  (*) = liftA2 (*)

  (-) = liftA2 (-)

  abs = fmap abs

  signum = fmap signum

  fromInteger = pure . fromInteger

instance (Monoid n, Fractional a) => Fractional (ArticulationT n a) where

  recip = fmap recip

  fromRational = pure . fromRational

instance (Monoid n, Floating a) => Floating (ArticulationT n a) where

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

instance Wrapped (ArticulationT p a) where

  type Unwrapped (ArticulationT p a) = (p, a)

  _Wrapped' = iso getArticulationT ArticulationT

instance Rewrapped (ArticulationT p a) (ArticulationT p' b)

type instance Articulation (ArticulationT p a) = p

type instance SetArticulation p' (ArticulationT p a) = ArticulationT p' a

instance
  (Transformable p, Transformable p') =>
  HasArticulation (ArticulationT p a) (ArticulationT p' a)
  where
  articulation = _Wrapped . _1

instance
  (Transformable p, Transformable p') =>
  HasArticulations (ArticulationT p a) (ArticulationT p' a)
  where
  articulations = _Wrapped . _1

deriving instance (IsPitch a, Monoid n) => IsPitch (ArticulationT n a)

deriving instance (IsInterval a, Monoid n) => IsInterval (ArticulationT n a)

instance (Tiable n, Tiable a) => Tiable (ArticulationT n a) where
  toTied (ArticulationT (d, a)) = (ArticulationT (d1, a1), ArticulationT (d2, a2))
    where
      (a1, a2) = toTied a
      (d1, d2) = toTied d

-- TODO move
addArtCon ::
  ( HasPhrases s t a b,
    HasArticulation' a,
    HasArticulation a b,
    Articulation a ~ d,
    Articulation b ~ Ctxt d
  ) =>
  s ->
  t
addArtCon = over (phrases . varticulation) withContext

varticulation ::
  (HasArticulation s s, HasArticulation s t) =>
  Lens (Voice s) (Voice t) (Voice (Articulation s)) (Voice (Articulation t))
varticulation = lens (fmap $ view articulation) (flip $ zipVoiceWithNoScale (set articulation))
