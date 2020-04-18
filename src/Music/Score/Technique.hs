{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports #-}

-- |  Provides functions for manipulating techniques.
module Music.Score.Technique
  ( -- * Technique type functions
    Technique,
    SetTechnique,
    TechniqueLensLaws',
    TechniqueLensLaws,

    -- * Accessing techniques
    HasTechniques (..),
    HasTechnique (..),
    HasTechniques',
    HasTechnique',
    technique',
    techniques',
    TechniqueT (..),

    -- * Dynamically typed playing techniques
    SomeTechnique,
    PizzArco (..),
    Legno (..),
    StringPos (..),
    StringMute (..),
    pizzicato,
    legno,
    stringPos,
    stringMute,

    -- ** High level combinators
    pizz,
    arco,
    moltoSulPont,
    sulPont,
    posNat,
    sulTasto,
    moltoSulTasto,
    naturale,
    colLegno,
    colLegnoBatt,
    senzaLegno,
    muted,
    unmuted,
    conSord,
    senzaSord,

    -- * Context
    vtechnique,
    addTechniqueCon,
  )
where

import BasePrelude
import Control.Comonad
import Control.Lens hiding ((&), Level, transform)
import Control.Lens.TH (makeLenses)
import Data.AffineSpace
import Data.AffineSpace.Point (relative)
import Data.Functor.Context
import Data.Functor.Couple
import Data.Kind
import qualified Data.List as List
import Data.Semigroup
import Data.VectorSpace hiding (Sum)
import Music.Pitch.Literal
import Music.Score.Color (ColorT)
import Music.Score.Harmonics (HarmonicT)
import Music.Score.Internal.Util (through)
import Music.Score.Part
import Music.Score.Phrases
import Music.Score.Slide (SlideT)
import Music.Score.StaffNumber (StaffNumberT)
import Music.Score.Text (TextT)
import Music.Score.Ties (Tiable (..), TieT)
import Music.Score.Tremolo (TremoloT)
import Music.Time
import Music.Time.Internal.Transform

-- |
-- Techniques type.
type family Technique (s :: Type) :: Type

-- |
-- Technique type.
type family SetTechnique (b :: Type) (s :: Type) :: Type

-- |
-- Class of types that provide a single technique.
class (HasTechniques s t) => HasTechnique s t where
  -- | Access a single technique.
  technique :: Lens s t (Technique s) (Technique t)

type TechniqueLensLaws' s t a b =
  ( Technique (SetTechnique a s) ~ a,
    SetTechnique (Technique t) s ~ t,
    SetTechnique a (SetTechnique b s) ~ SetTechnique a s
  )

type TechniqueLensLaws s t = TechniqueLensLaws' s t (Technique s) (Technique t)

-- |
-- Class of types that provide a technique traversal.
class
  ( -- Transformable (Technique s),
    -- Transformable (Technique t),
    -- SetTechnique (Technique t) s ~ t
    TechniqueLensLaws s t
  ) =>
  HasTechniques s t where
  -- | Access all techniques.
  techniques :: Traversal s t (Technique s) (Technique t)

type HasTechnique' a = HasTechnique a a

type HasTechniques' a = HasTechniques a a

-- | Access a single technique.
technique' :: (HasTechnique s t, s ~ t) => Lens' s (Technique s)
technique' = technique

-- | Access all techniques.
techniques' :: (HasTechniques s t, s ~ t) => Traversal' s (Technique s)
techniques' = techniques

type instance Technique (c, a) = Technique a

type instance SetTechnique b (c, a) = (c, SetTechnique b a)

type instance Technique [a] = Technique a

type instance SetTechnique b [a] = [SetTechnique b a]

type instance Technique (Maybe a) = Technique a

type instance SetTechnique b (Maybe a) = Maybe (SetTechnique b a)

type instance Technique (PartT r a) = Technique a

type instance SetTechnique b (PartT r a) = PartT r (SetTechnique b a)

type instance Technique (StaffNumberT a) = Technique a

type instance SetTechnique b (StaffNumberT a) = StaffNumberT (SetTechnique b a)

instance HasTechniques a b => HasTechniques (StaffNumberT a) (StaffNumberT b) where
  techniques = _Wrapped . techniques

instance HasTechnique a b => HasTechnique (StaffNumberT a) (StaffNumberT b) where
  technique = _Wrapped . technique

type instance Technique (TremoloT a) = Technique a

type instance SetTechnique b (TremoloT a) = TremoloT (SetTechnique b a)

instance HasTechniques a b => HasTechniques (TremoloT a) (TremoloT b) where
  techniques = _Wrapped . techniques

instance HasTechnique a b => HasTechnique (TremoloT a) (TremoloT b) where
  technique = _Wrapped . technique

type instance Technique (ColorT a) = Technique a

type instance SetTechnique b (ColorT a) = ColorT (SetTechnique b a)

instance HasTechniques a b => HasTechniques (ColorT a) (ColorT b) where
  techniques = _Wrapped . techniques

instance HasTechnique a b => HasTechnique (ColorT a) (ColorT b) where
  technique = _Wrapped . technique

type instance Technique (TextT a) = Technique a

type instance SetTechnique b (TextT a) = TextT (SetTechnique b a)

type instance Technique (HarmonicT a) = Technique a

type instance SetTechnique b (HarmonicT a) = HarmonicT (SetTechnique b a)

type instance Technique (SlideT a) = Technique a

type instance SetTechnique b (SlideT a) = SlideT (SetTechnique b a)

type instance Technique (Either c a) = Technique a

type instance SetTechnique b (Either c a) = Either c (SetTechnique b a)

type instance Technique (Event a) = Technique a

type instance SetTechnique b (Event a) = Event (SetTechnique b a)

type instance Technique (Placed a) = Technique a

type instance SetTechnique b (Placed a) = Placed (SetTechnique b a)

type instance Technique (Note a) = Technique a

type instance SetTechnique b (Note a) = Note (SetTechnique b a)

type instance Technique (Voice a) = Technique a

type instance SetTechnique b (Voice a) = Voice (SetTechnique b a)

type instance Technique (Track a) = Technique a

type instance SetTechnique b (Track a) = Track (SetTechnique b a)

type instance Technique (Score a) = Technique a

type instance SetTechnique b (Score a) = Score (SetTechnique b a)

type instance Technique (Aligned a) = Technique a

type instance SetTechnique b (Aligned a) = Aligned (SetTechnique b a)

type instance Technique () = ()

type instance SetTechnique a () = a

type instance Technique SomeTechnique = SomeTechnique

type instance SetTechnique a SomeTechnique = a

instance HasTechniques a b => HasTechniques (Aligned a) (Aligned b) where
  techniques = _Wrapped . techniques

instance HasTechnique a b => HasTechnique (c, a) (c, b) where
  technique = _2 . technique

instance HasTechniques a b => HasTechniques (c, a) (c, b) where
  techniques = traverse . techniques

instance HasTechniques a b => HasTechniques [a] [b] where
  techniques = traverse . techniques

instance HasTechniques a b => HasTechniques (Maybe a) (Maybe b) where
  techniques = traverse . techniques

instance HasTechniques a b => HasTechniques (Either c a) (Either c b) where
  techniques = traverse . techniques

instance (HasTechniques a b) => HasTechniques (Event a) (Event b) where
  techniques = from event . techniques

instance (HasTechnique a b) => HasTechnique (Event a) (Event b) where
  technique = from event . technique

instance (HasTechniques a b) => HasTechniques (Placed a) (Placed b) where
  techniques = _Wrapped . techniques

instance (HasTechnique a b) => HasTechnique (Placed a) (Placed b) where
  technique = _Wrapped . technique

instance (HasTechniques a b) => HasTechniques (Note a) (Note b) where
  techniques = _Wrapped . techniques

instance (HasTechnique a b) => HasTechnique (Note a) (Note b) where
  technique = _Wrapped . technique

instance HasTechniques a b => HasTechniques (Voice a) (Voice b) where
  techniques = traverse . techniques

instance HasTechniques a b => HasTechniques (Track a) (Track b) where
  techniques = traverse . techniques

instance HasTechnique a b => HasTechnique (PartT p a) (PartT p b) where
  technique = _Wrapped . _2 . technique

instance HasTechniques a b => HasTechniques (PartT p a) (PartT p b) where
  techniques = _Wrapped . _2 . techniques

{-
type instance Technique (Chord a)       = Technique a
type instance SetTechnique b (Chord a)  = Chord (SetTechnique b a)
instance HasTechniques a b => HasTechniques (Chord a) (Chord b) where
  techniques = traverse . techniques
-}

instance (HasTechniques a b) => HasTechniques (Score a) (Score b) where
  techniques = traverse . techniques

type instance Technique (Behavior a) = Behavior a

type instance SetTechnique b (Behavior a) = b

instance
  ( b ~ Technique b,
    SetTechnique (Behavior a) b ~ Behavior a
  ) =>
  HasTechniques (Behavior a) b
  where
  techniques = ($)

instance
  ( b ~ Technique b,
    SetTechnique (Behavior a) b ~ Behavior a
  ) =>
  HasTechnique (Behavior a) b
  where
  technique = ($)

type instance Technique (Couple c a) = Technique a

type instance SetTechnique g (Couple c a) = Couple c (SetTechnique g a)

type instance Technique (TextT a) = Technique a

type instance SetTechnique g (TextT a) = TextT (SetTechnique g a)

type instance Technique (HarmonicT a) = Technique a

type instance SetTechnique g (HarmonicT a) = HarmonicT (SetTechnique g a)

type instance Technique (TieT a) = Technique a

type instance SetTechnique g (TieT a) = TieT (SetTechnique g a)

type instance Technique (SlideT a) = Technique a

type instance SetTechnique g (SlideT a) = SlideT (SetTechnique g a)

instance (HasTechniques a b) => HasTechniques (Couple c a) (Couple c b) where
  techniques = _Wrapped . techniques

instance (HasTechnique a b) => HasTechnique (Couple c a) (Couple c b) where
  technique = _Wrapped . technique

instance (HasTechniques a b) => HasTechniques (TextT a) (TextT b) where
  techniques = _Wrapped . techniques

instance (HasTechnique a b) => HasTechnique (TextT a) (TextT b) where
  technique = _Wrapped . technique

instance (HasTechniques a b) => HasTechniques (HarmonicT a) (HarmonicT b) where
  techniques = _Wrapped . techniques

instance (HasTechnique a b) => HasTechnique (HarmonicT a) (HarmonicT b) where
  technique = _Wrapped . technique

instance (HasTechniques a b) => HasTechniques (TieT a) (TieT b) where
  techniques = _Wrapped . techniques

instance (HasTechnique a b) => HasTechnique (TieT a) (TieT b) where
  technique = _Wrapped . technique

instance (HasTechniques a b) => HasTechniques (SlideT a) (SlideT b) where
  techniques = _Wrapped . techniques

instance (HasTechnique a b) => HasTechnique (SlideT a) (SlideT b) where
  technique = _Wrapped . technique

newtype TechniqueT n a = TechniqueT {getTechniqueT :: Couple n a}
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

instance (Monoid n, Num a) => Num (TechniqueT n a) where

  (+) = liftA2 (+)

  (*) = liftA2 (*)

  (-) = liftA2 (-)

  abs = fmap abs

  signum = fmap signum

  fromInteger = pure . fromInteger

instance (Monoid n, Fractional a) => Fractional (TechniqueT n a) where

  recip = fmap recip

  fromRational = pure . fromRational

instance (Monoid n, Floating a) => Floating (TechniqueT n a) where

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

instance (Monoid n, Enum a) => Enum (TechniqueT n a) where

  toEnum = pure . toEnum

  fromEnum = fromEnum . extract

instance (Monoid n, Bounded a) => Bounded (TechniqueT n a) where

  minBound = pure minBound

  maxBound = pure maxBound

-- instance (Monoid n, Num a, Ord a, Real a) => Real (TechniqueT n a) where
--     toRational = toRational . extract
--
-- instance (Monoid n, Real a, Enum a, Integral a) => Integral (TechniqueT n a) where
--     quot = liftA2 quot
--     rem = liftA2 rem
--     toInteger = toInteger . extract

instance Wrapped (TechniqueT p a) where

  type Unwrapped (TechniqueT p a) = Couple p a

  _Wrapped' = iso getTechniqueT TechniqueT

instance Rewrapped (TechniqueT p a) (TechniqueT p' b)

type instance Technique (TechniqueT p a) = p

type instance SetTechnique p' (TechniqueT p a) = TechniqueT p' a

instance HasTechnique (TechniqueT p a) (TechniqueT p' a) where
  technique f (TechniqueT (Couple (t, x))) = fmap (TechniqueT . Couple . (,x)) (f t)

instance HasTechniques (TechniqueT p a) (TechniqueT p' a) where
  techniques = technique

deriving instance (IsPitch a, Monoid n) => IsPitch (TechniqueT n a)

deriving instance (IsInterval a, Monoid n) => IsInterval (TechniqueT n a)

deriving instance (Tiable a) => Tiable (TechniqueT n a)

{-
-- TODO move?
data InstrType = Winds | Strings | Brass | Perc | Keyboard | Vocal

data Technique :: '[InstrType] -> Type where
  Pizz :: Bool -> Technique Strings

data SomeTechnique where
  SomeTechnique :: forall ts . Technique ts -> SomeTechnique
-}

data PizzArco = Arco | Pizz
  deriving (Show, Enum, Bounded, Eq, Ord)

data Legno = NonLegno | ColLegnoTratto | ColLegnoBatt
  deriving (Show, Enum, Bounded, Eq, Ord)

data StringPos = MoltoSulPont | SulPont | PosNat | SulTasto | MoltoSulTasto
  deriving (Show, Enum, Bounded, Eq, Ord)

data StringMute = NoStringMute | StringMute
  deriving (Show, Enum, Bounded, Eq, Ord)

instance Monoid PizzArco where mempty = Arco

instance Monoid Legno where mempty = NonLegno

instance Monoid StringPos where mempty = PosNat

instance Monoid StringMute where mempty = NoStringMute

instance Semigroup PizzArco where
  x <> y
    | x == mempty = y
    | otherwise = x

instance Semigroup Legno where
  x <> y
    | x == mempty = y
    | otherwise = x

instance Semigroup StringPos where
  x <> y
    | x == mempty = y
    | otherwise = x

instance Semigroup StringMute where
  x <> y
    | x == mempty = y
    | otherwise = x

data SomeTechnique
  = SomeTechnique
      { _pizzicato :: PizzArco,
        _legno :: Legno,
        _stringPos :: StringPos,
        _stringMute :: StringMute
        -- TODO etc
      }
  deriving (Show, Eq)

makeLenses ''SomeTechnique

instance Monoid SomeTechnique where
  mempty = SomeTechnique
    { _pizzicato = mempty,
      _legno = mempty,
      _stringPos = mempty,
      _stringMute = mempty
    }

instance Semigroup SomeTechnique where
  SomeTechnique p1 l1 o1 m1
    <> SomeTechnique p2 l2 o2 m2 =
      SomeTechnique (p1 <> p2) (l1 <> l2) (o1 <> o2) (m1 <> m2)

instance Tiable SomeTechnique where
  toTied x = (x, x)

pizz, arco :: (HasTechniques' a, Technique a ~ SomeTechnique) => a -> a
pizz = set (techniques . pizzicato) Pizz
arco = set (techniques . pizzicato) Arco

moltoSulPont, sulPont, posNat, sulTasto, moltoSulTasto :: (HasTechniques' a, Technique a ~ SomeTechnique) => a -> a
moltoSulPont = set (techniques . stringPos) MoltoSulPont
sulPont = set (techniques . stringPos) SulPont
posNat = set (techniques . stringPos) PosNat
sulTasto = set (techniques . stringPos) SulTasto
moltoSulTasto = set (techniques . stringPos) MoltoSulTasto

naturale :: (HasTechniques' a, Technique a ~ SomeTechnique) => a -> a
naturale = senzaLegno . posNat . unmuted

colLegno, colLegnoBatt, senzaLegno :: (HasTechniques' a, Technique a ~ SomeTechnique) => a -> a
colLegno = set (techniques . legno) ColLegnoTratto
colLegnoBatt = set (techniques . legno) ColLegnoBatt
senzaLegno = set (techniques . legno) NonLegno

muted, unmuted, conSord, senzaSord :: (HasTechniques' a, Technique a ~ SomeTechnique) => a -> a
muted = set (techniques . stringMute) StringMute
unmuted = set (techniques . stringMute) NoStringMute
conSord = set (techniques . stringMute) StringMute
senzaSord = set (techniques . stringMute) NoStringMute

-- |
-- View just the techniquees in a voice.
vtechnique ::
  ({-SetTechnique (Technique t) s ~ t,-} HasTechnique a a, HasTechnique a b) =>
  Lens (Voice a) (Voice b) (Voice (Technique a)) (Voice (Technique b))
vtechnique = lens (fmap $ view technique) (flip $ zipVoiceWithNoScale (set technique))

-- vtechnique = through technique technique

addTechniqueCon ::
  ( HasPhrases s t a b,
    HasTechnique a a,
    HasTechnique a b,
    Technique a ~ d,
    Technique b ~ Ctxt d
  ) =>
  s ->
  t
addTechniqueCon = over (phrases . vtechnique) withContext
