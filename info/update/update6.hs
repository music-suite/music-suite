
{-# LANGUAGE
    MultiParamTypeClasses,
    TypeFamilies,
    TupleSections,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    NoMonomorphismRestriction,
    UndecidableInstances,
    RankNTypes,
    DefaultSignatures,
    GeneralizedNewtypeDeriving #-}

module Update6 where
    
-- Like Update5, this design is based on the Overloaded record fields proposal,
-- but simplified to capture a single class for each type (we are only interested
-- in four anyway)
--
--  See <https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Plan>
--

import Control.Applicative
import Control.Arrow
import Control.Lens
import Data.Semigroup
import Data.Foldable
import Data.Traversable
import Data.AffineSpace
import Data.AffineSpace.Point
import qualified Music.Pitch as M

type family Pitch             (s :: *) :: * -- Pitch s   = a
type family SetPitch (b :: *) (s :: *) :: * -- Pitch b s = t

-- class Has s t a b | 
--     s -> a, 
--     -- t -> b, 
--     s b -> t, 
--     -- t a -> s

-- type Lens      s t a b = forall f. Functor f     => (a -> f b) -> s -> f t
-- type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

class (SetPitch (Pitch t) s ~ t) => HasPitch s t where
    pitch :: Lens s t (Pitch s) (Pitch t)

    pitch' :: (s ~ t) => Lens' s (Pitch s)
    pitch' = pitch

class (SetPitch (Pitch t) s ~ t) => HasPitches s t where
    pitches :: Traversal s t (Pitch s) (Pitch t)

    pitches' :: (s ~ t) => Traversal' s (Pitch s)
    pitches' = pitches

{-
    lens id (flip const)
        = (\sa sbt afb s -> sbt s <$> afb (sa s)) id (flip const)
        = (\afb s -> (flip const) s <$> afb (id s))
        = (\afb s -> (flip const) s <$> afb s)
        = (\afb s -> id <$> afb s)
        = (\afb s -> afb s)
        = ($)
-}

type instance Pitch Bool = Bool
type instance SetPitch a Bool = a
instance (a ~ Pitch a) => HasPitch Bool a where
    pitch = ($)
instance (a ~ Pitch a) => HasPitches Bool a where
    pitches = ($)
    
type instance Pitch Int = Int
type instance SetPitch a Int = a
instance (a ~ Pitch a) => HasPitch Int a where
    pitch = ($)
instance (a ~ Pitch a) => HasPitches Int a where
    pitches = ($)

type instance Pitch Float = Float
type instance SetPitch a Float = a
instance (a ~ Pitch a) => HasPitch Float a where
    pitch = ($)
instance (a ~ Pitch a) => HasPitches Float a where
    pitches = ($)

type instance Pitch (c,a) = Pitch a
type instance SetPitch b (c,a) = (c,SetPitch b a)
instance HasPitch a b => HasPitch (c, a) (c, b) where
    pitch = _2 . pitch
instance HasPitches a b => HasPitches (c, a) (c, b) where
    pitches = traverse . pitches

type instance Pitch [a] = Pitch a
type instance SetPitch b [a] = [SetPitch b a]
instance HasPitches a b => HasPitches [a] [b] where
    pitches = traverse . pitches


data PitchT f a = PitchT f a
    deriving (Show, Functor, Foldable, Traversable)

type instance Pitch (PitchT p a) = p
type instance SetPitch p' (PitchT p a) = PitchT p' a
instance HasPitch a b => HasPitch (PitchT a c) (PitchT b c) where
    pitch = lens (\(PitchT p a) -> p) (\(PitchT p a) p' -> PitchT p' a)
instance HasPitches a b => HasPitches (PitchT a c) (PitchT b c) where
    pitches = lens (\(PitchT p a) -> p) (\(PitchT p a) p' -> PitchT p' a)


type Interval a = Diff (Pitch a)


x :: PitchT Int (Bool, Int)
x = PitchT 3 (True, 0)

y :: PitchT Float (Bool, Int)
y = x & pitch %~ fromIntegral

z :: PitchT Bool (Bool, Int)
z = x & pitch .~ True

-- up :: (HasPitches a a, AffineSpace (Pitch a)) => Interval a -> a -> a
up a = pitches %~ (.+^ a)

-- down :: (HasPitches a a, AffineSpace (Pitch a)) => Interval a -> a -> a
down a = pitches %~ (.-^ a)


type HasInterval a = (Floating (Interval a), M.InnerSpace (Interval a), M.Scalar (Interval a) ~ (Interval a))

interval :: (HasPitch a a, AffineSpace (Pitch a), HasInterval a) => a -> a -> Interval a
interval x y = (x^.pitch) `distance` (y^.pitch)

invert :: (HasPitch a a, AffineSpace (Pitch a)) => Pitch a -> a -> a
invert p = pitch %~ reflectThrough p      


--                         

type Span = ()
class Transformable a where
sunder :: (Transformable a, Transformable b) => Span -> (a -> b) -> a -> b
sunder = undefined
sapp :: Transformable a => Span -> a -> a
sapp = undefined

newtype Score a = Score { getScore :: [(Span, a)] }
instance Wrapped (Score a) where
    type Unwrapped (Score a) = [(Span, a)]
    _Wrapped' = iso getScore Score
instance Rewrapped (Score a) (Score b)
instance Functor Score where
instance Foldable Score where
instance Traversable Score where
mapWithSpan :: (Span -> a -> b) -> Score a -> Score b
mapWithSpan f (Score x) = Score $ fmap (\(s,x) -> (s, f s x)) x


type instance Pitch (Score a) = Pitch a
type instance SetPitch g (Score a) = Score (SetPitch g a)
type instance Pitch (Score a) = Pitch a
instance (HasPitches a b, Transformable (Pitch a), Transformable (Pitch b)) => HasPitches (Score a) (Score b) where         
    pitches = _Wrapped . traverse . pl


pl :: (HasPitches a b, Transformable (Pitch a), Transformable (Pitch b)) => 
    Traversal (Span, a) (Span, b) (Pitch a) (Pitch b)
pl f (s,a) = (s,) <$> (pitches $ fmap (sapp s) . f . sapp s) a

pl2 :: (HasPitch a b, Transformable (Pitch a), Transformable (Pitch b)) => 
    Lens (Span, a) (Span, b) (Pitch a) (Pitch b)
-- pl2 f (s,a) = (s,) <$> (pitch $ fmap (sapp s) . f . sapp s) a

pl2 = pl3 (sapp . fst) (sapp . fst) pitch

pl3 :: Functor f => (s -> b -> b') -> (s -> a' -> a)
    -> ((a' -> f b') -> s -> t)
    -> (a -> f b) -> s -> t
pl3 sbb saa sl f s = (sl $ fmap (sbb s) . f . saa s) s




-- pl3
--   :: 
--     -- (Functor f, Functor g) =>
--         (x -> a' -> a)
--      -> (x -> b -> b')
--      -> forall f g . (Functor f) =>
--        ((a' -> f b') -> s -> f t)
--      -> (a -> f b) -> (x, s) -> f (x, t)
-- pl3 bef aft subL f (s,a) = (s,) <$> (subL $ fmap (aft s) . f . bef s) a
-- 




