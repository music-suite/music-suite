
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ViewPatterns               #-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}


import Data.Functor.Context
import Data.Maybe (fromJust, isNothing)
import           Data.Clipped
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ord (comparing)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Semigroup
import           Control.Comonad
import           Control.Applicative
import           Control.Lens               hiding (Indexable, Level, above,
                                             below, index, inside, parts,
                                             reversed, transform, (<|), (|>))
import           Control.Lens               hiding (Indexable, Level, above,
                                             below, index, inside, parts,
                                             reversed, transform, (<|), (|>))
import           Control.Monad
import           Control.Monad.Compose
import           Control.Monad.Plus
import           Data.Aeson                 (ToJSON (..))
import qualified Data.Aeson                 as JSON
import           Data.AffineSpace
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.AffineSpace.Point
import           Data.Bifunctor
import           Data.Foldable              (Foldable)
import qualified Data.Foldable              as Foldable
import           Data.List                  (mapAccumL, mapAccumR)
import qualified Data.List
import           Data.Map                   (Map)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import qualified Data.Map                   as Map
import           Data.Ratio
import           Data.Ratio
import           Data.Semigroup
import           Data.Semigroup
import           Data.Set                   (Set)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Set                   as Set
import           Data.String
import           Data.Traversable           (Traversable)
import qualified Data.Traversable           as T
import           Data.Typeable
import           Data.Typeable

import           Control.Applicative
import           Control.Lens               hiding (Indexable, Level, above,
                                             below, index, inside, parts,
                                             reversed, transform, (<|), (|>))
import           Control.Lens               hiding (Indexable, Level, above,
                                             below, index, inside, parts,
                                             reversed, transform, (<|), (|>))
import           Data.AffineSpace
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.AffineSpace.Point
import           Data.Bifunctor
import           Data.Foldable              (Foldable)
import qualified Data.Foldable              as Foldable
import           Data.Functor.Adjunction    (unzipR)
import           Data.Functor.Contravariant
import           Data.Functor.Couple
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.NumInstances          ()
import           Data.Ratio
import           Data.Semigroup             hiding ()
import           Data.Semigroup
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.String
import           Data.String
import           Data.Typeable
import           Data.VectorSpace           hiding (Sum (..))
import           Music.Dynamics.Literal
import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Pitch.Literal
import           Prelude

import           Control.Applicative
import           Control.Lens               hiding (Indexable, Level, above,
                                             below, index, inside, parts,
                                             reversed, transform, (<|), (|>))

import           Data.Distributive
import           Data.Functor.Rep           as R
import           Data.Functor.Rep.Lens
import           Data.Typeable
-- import           Music.Time.Bound
-- import           Music.Time.Internal.Transform
-- import           Music.Time.Note
-- import           Music.Time.Reverse
-- import           Music.Time.Score
-- import           Music.Time.Split

import           Music.Dynamics.Literal
import           Music.Pitch.Alterable
import           Music.Pitch.Augmentable
import           Music.Pitch.Literal



newtype Behavior a  = Behavior { getBehavior :: Time -> a }
  deriving (Functor, Applicative, Monad, Typeable)

instance Show (Behavior a) where
  show _ = "<<Behavior>>"

instance Distributive Behavior where
  distribute = Behavior . distribute . fmap getBehavior

instance Representable Behavior where
  type Rep Behavior = Time
  tabulate = Behavior
  index (Behavior x) = x

instance Transformable (Behavior a) where
  transform s (Behavior a) = Behavior (a `whilst` s)
    where
      f `whilst` s = f . transform (negateV s)

instance Reversible (Behavior a) where
  rev = stretch (-1)

deriving instance Semigroup a => Semigroup (Behavior a)
deriving instance Monoid a => Monoid (Behavior a)
deriving instance Num a => Num (Behavior a)
deriving instance Fractional a => Fractional (Behavior a)
deriving instance Floating a => Floating (Behavior a)
deriving instance AdditiveGroup a => AdditiveGroup (Behavior a)

-- TODO bad
instance Real a => Real (Behavior a) where
  toRational = toRational . (! 0)

instance IsString a => IsString (Behavior a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Behavior a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Behavior a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Behavior a) where
  fromDynamics = pure . fromDynamics

instance Alterable a => Alterable (Behavior a) where
  sharpen = fmap sharpen
  flatten = fmap flatten

instance Augmentable a => Augmentable (Behavior a) where
  augment = fmap augment
  diminish = fmap diminish

instance Eq a => Eq (Behavior a) where
  (==) = error "No overloading for behavior: (<=)"

instance Ord a => Ord (Behavior a) where
  (<=) = error "No overloading for behavior: (<=)"
  (>=) = error "No overloading for behavior: (<=)"
  (<)  = error "No overloading for behavior: (<=)"
  (>)  = error "No overloading for behavior: (<=)"
  max  = liftA2 max
  min  = liftA2 min

instance Enum a => Enum (Behavior a) where
  toEnum = pure . toEnum
  fromEnum = fromEnum . (! 0)

instance VectorSpace a => VectorSpace (Behavior a) where
  type Scalar (Behavior a) = Behavior (Scalar a)
  (*^) = liftA2 (*^)

instance AffineSpace a => AffineSpace (Behavior a) where
  type Diff (Behavior a) = Behavior (Diff a)
  (.-.) = liftA2 (.-.)
  (.+^) = liftA2 (.+^)

behavior :: Iso (Time -> a) (Time -> b) (Behavior a) (Behavior b)
behavior = R.tabulated

unbehavior :: Iso (Behavior a) (Behavior b) (Time -> a) (Time -> b)
unbehavior = from behavior

line :: Fractional a => Behavior a
line = realToFrac ^. R.tabulated

unit :: Fractional a => Behavior a
unit = switch 0 0 (switch 1 line 1)
-- > f t | t < 0     = 0
-- >     | t > 1     = 1
-- >     | otherwise = t

-- |
-- A behavior that
--
interval :: (Fractional a, Transformable a) => Time -> Time -> Note (Behavior a)
interval t u = (t <-> u, line) ^. note

-- |
-- A behavior that
--
sine :: Floating a => Behavior a
sine = sin (line*tau)

-- |
-- A behavior that
--
cosine :: Floating a => Behavior a
cosine = cos (line*tau)

-- |
-- A behavior that goes from 0 to 1 repeatedly with a period of 1.
--
-- sawtooth :: RealFrac a => Behavior a
-- sawtooth = line - fmap (fromIntegral . floor) line

-- |
-- A behavior that is 1 at time 0, and 0 at all other times.
--
impulse :: Num a => Behavior a
impulse = switch' 0 0 1 0
-- > f t | t == 0    = 1
-- >     | otherwise = 0

-- |
-- A behavior that goes from 0 to 1 at time 0.
--
turnOn  = switch 0 0 1

-- |
-- A behavior that goes from 1 to 0 at time 0.
--
turnOff = switch 0 1 0

--
-- Because the 'Time' type is fixed and unbounded in the current version, we can not
-- define a generix isomorphism from behaviors to segments. If we change the library to
-- provide multiple time representations (using TFs or similar), we should provide
-- these combinators:
--
-- > focusOnFullRange :: Bounded Time => Behavior a -> Segment a
-- > focusingOnFullRange :: Bounded Time => Iso' (Behavior a) (Segment a)
--
-- |
-- Instantly switch from one behavior to another.
--
switch :: Time -> Behavior a -> Behavior a -> Behavior a
switch t rx ry = switch' t rx ry ry

-- | Replace everthing before the given time by `mempty`.
trimBefore :: Monoid a => Time -> Behavior a -> Behavior a
trimBefore start = switch start mempty

-- | Replace everthing after the given time by `mempty`.
trimAfter :: Monoid a => Time -> Behavior a -> Behavior a
trimAfter stop x = switch stop x mempty

-- |
-- Instantly switch from one behavior to another with an optinal intermediate value.
--
switch' :: Time -> Behavior a -> Behavior a -> Behavior a -> Behavior a
switch' t rx ry rz = tabulate $ \u -> case u `compare` t of
  LT -> rx ! u
  EQ -> ry ! u
  GT -> rz ! u


-- Internal

tau :: Floating a => a
tau = 2 * pi

-- floor' :: RealFrac a => a -> a
-- floor' = fromIntegral . floor


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
-------------------------------------------------------------------------------------





-- |
-- 'Bound' restricts the start and stop time of a value, and prevents access to values
-- outside the bounds.
--
-- 'Bound' is especially useful to restrict the range of a 'Behavior'. If we have a
-- value with can only be reasonably defined for a particular time range, we can
-- represent it as 'Bound' 'Behavior'. This is isomorphic to a 'Note' 'Segment', and
-- 'bounded' whitnesses the isomorphism.
--
-- 'Bound' is not 'Foldable' or 'Traversable', as that would allow us to access values
-- outside the bounds. However, we can still access values of a 'Bound' 'Behavior' in a
-- safe manner using 'trim' or 'splice'.
--
newtype Bound a = Bound { getBound :: (Span, a) }
  deriving (Functor, Semigroup, Typeable, Eq, Show)

-- $semantics Bound
--
-- @
-- type Bound a = (Time, Time, a)
-- @
--

--
-- These are both unsafe, as they allow us to define 'unBound'
--
-- instance Foldable Bound where
--   foldr f z (Bound (_,x)) = f x z
--
-- instance Traversable Bound where
--   traverse f (Bound (s,x)) = (Bound . (s,)) <$> f x
--

--
-- TODO define Applicative/Monad
--
--
-- This is a Writer-style instance with interval arithmetic style union/empty as the Monoid
-- A possible problem with this is that there are multiple representations of the empty
-- set (namely [(t, t)^.from range | t <- {Time} ]).
--

instance Wrapped (Bound a) where
  type Unwrapped (Bound a) = (Span, a)
  _Wrapped' = iso getBound Bound

instance Rewrapped (Bound a) (Bound b)

instance Reversible a => Reversible (Bound a) where
  -- rev = over (_Wrapped . each) rev
  rev = over _Wrapped $ (bimap rev rev)

instance (HasPosition a, Splittable a) => Splittable (Bound a) where
  -- TODO

-- |
-- 'Bound' transform by transforming the bounded value as well as
-- the bounds.
--
instance Transformable a => Transformable (Bound a) where
  transform t = over _Wrapped (transform t `bimap` transform t)

instance (HasPosition a, HasDuration a) => HasDuration (Bound a) where
  _duration x = _offset x .-. _onset x

instance HasPosition a => HasPosition (Bound a) where
  -- TODO lawless
  -- _position (Bound (view range -> (t, u), x)) d = truncating t u (_position x d)
  _position (Bound (view range -> (t, u), x)) = alerp t u

truncating :: Ord a => a -> a -> a -> a
truncating t u x = (x `max` t) `min` u

-- |
-- Add bounds.
--
bounds :: Time -> Time -> a -> Bound a
bounds t u x = Bound (t <-> u, x)

-- |
-- Add bounds.
--
-- @
-- (s,x)^.note = (bounding s . transform s) x
-- @
--
bounding :: Span -> a -> Bound a
bounding (view range -> (t, u)) = bounds t u



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
-------------------------------------------------------------------------------------





-- |
-- A 'Chord' is a parallel composition of values.
--
-- @
-- type Chord a = [Delayed a]
-- @
--
newtype Chord a = Chord { getChord :: ChordList (ChordEv a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

-- Can use [] or Seq here
type ChordList = []

-- Can use any type as long as chordEv provides an Iso
type ChordEv a = Delayed a

chordEv :: Iso (Delayed a) (Delayed b) (ChordEv a) (ChordEv b)
chordEv = id

instance Applicative Chord where
  pure  = return
  (<*>) = ap

instance Monad Chord where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

instance Wrapped (Chord a) where
  type Unwrapped (Chord a) = (ChordList (ChordEv a))
  _Wrapped' = iso getChord Chord

instance Rewrapped (Chord a) (Chord b)

instance Transformable (Chord a) where
  transform s = over _Wrapped' (transform s)

instance HasDuration (Chord a) where
  _duration = Foldable.sum . fmap _duration . view _Wrapped'

instance Splittable a => Splittable (Chord a) where
  -- TODO

instance Reversible a => Reversible (Chord a) where
  rev = over _Wrapped' (fmap rev) -- TODO OK?

-- TODO
-- instance HasMeta (Chord a) where
  -- meta = error "Not implemented: meta"

chord :: Getter [Delayed a] (Chord a)
chord = from unsafeChord

unchord :: Lens (Chord a) (Chord b) [Delayed a] [Delayed b]
unchord = _Wrapped

-- TODO names are not consistent with other types
unsafeChord :: Iso (Chord a) (Chord b) [Delayed a] [Delayed b]
unsafeChord = _Wrapped

instance IsString a => IsString (Chord a) where
  fromString = pure . fromString

deriving instance IsPitch a => IsPitch (Chord a)
deriving instance IsInterval a => IsInterval (Chord a)
deriving instance IsDynamics a => IsDynamics (Chord a)


{-
-- |
-- Invert a chord, i.e. transpose its lowest pitch up one octave.
--
-- To access higher-numbered inversions, iterate this function, i.e.
--
-- @
-- 'iterate' 'invertC' ('triad' c) !! 2
-- @
--
invertC :: Transposable a => Chord a -> Chord a
invertC = over chord (rotlAnd $ up _P8)

-- TODO include transp
inversions :: Transposable a => Chord a -> [Chord a]
inversions = iterate invertC

chordToScore :: Chord a -> Score a
chordToScore = pcat . map pure . toListOf traverse

-- TODO
unchord =  toListOf traverse


arpUp3 :: Chord a -> Score a
arpUp3 x = scat $ map ((^/16) . pure) [a,b,c]
  where
    [a,b,c] = unchord x

arpDown3 :: Chord a -> Score a
arpDown3 x = scat $ map ((^/16) . pure) [c,b,a]
  where
    [a,b,c] = unchord x

arpUpDown3 x = arpUp3 x |> arpDown3 x
arpDownUp3 x = arpDown3 x |> arpUp3 x

alberti3 :: Chord a -> Score a
alberti3 x = scat $ map ((^/16) . pure) [a,c,b,c]
  where
    [a,b,c] = unchord x



triad :: Transposable a => a -> Chord a
triad x = mconcat $ map pure [x, up _M3 x, up _P5 x]

mtriad :: Transposable a => a -> Chord a
mtriad x = mconcat $ map pure [x, up m3 x, up _P5 x]

sixthChord       = down m3 . invertC . mtriad
sixthFourthChord = down _P5 . invertC . invertC . triad


-- TODO better parsing
fromBass :: Transposable a => String -> a -> Chord a
fromBass "" x = triad x
-}


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
-------------------------------------------------------------------------------------

-- |
-- 'Delayed' represents a value with an offset in time.
--
-- A delayed value has a known 'position', but no 'duration'.
--
-- Placing a value inside 'Delayed' does not make it invariant under 'stretch', as the
-- offset of a delayed value may be stretched with respect to the origin. However, in
-- contrast to a note the /duration/ is not stretched.
--
newtype Delayed a = Delayed   { _delayedValue :: (Time, a) }
  deriving (Eq,
            Ord,
            Functor,
            Applicative,
            Monad,
            -- Comonad,
            Foldable,
            Traversable,
            Typeable)

-- $semantics Delayed
--
-- @
-- type Delayed a = (Time, a)
-- @
--

instance (Show a, Transformable a) => Show (Delayed a) where
  show x = show (x^.from delayed) ++ "^.delayed"

instance Wrapped (Delayed a) where
  type Unwrapped (Delayed a) = (Time, a)
  _Wrapped' = iso _delayedValue Delayed

instance Rewrapped (Delayed a) (Delayed b)

instance Transformable (Delayed a) where
  transform t = over _Wrapped $ first (transform t)

instance HasDuration (Delayed a) where
  _duration x = _offset x .-. _onset x

instance HasPosition (Delayed a) where
  x `_position` p = fst (view _Wrapped x) `_position` p

instance Reversible (Delayed a) where
  rev = revDefault

instance Splittable a => Splittable (Delayed a) where
  -- TODO is this right?
  split t = unzipR . fmap (split t)

-- Lifted instances

instance IsString a => IsString (Delayed a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Delayed a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Delayed a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Delayed a) where
  fromDynamics = pure . fromDynamics

-- |
-- View a delayed value as a pair of a the original value and a delay time.
--
delayed :: Iso (Time, a) (Time, b) (Delayed a) (Delayed b)
delayed = _Unwrapped

-- |
-- View a delayed value as a pair of the original value and the transformation (and vice versa).
--
delayedValue :: (Transformable a, Transformable b) => Lens (Delayed a) (Delayed b) a b
delayedValue = lens runDelayed $ flip (mapDelayed . const)

runDelayed :: Transformable a => Delayed a -> a
runDelayed = uncurry delayTime . view _Wrapped

mapDelayed :: (Transformable a, Transformable b) => (a -> b) -> Delayed a -> Delayed b
mapDelayed f (Delayed (t,x)) = Delayed (t, over (transformed (t >-> 1)) f x)

-- delayTime t = transform (t >-> 1)



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
-------------------------------------------------------------------------------------



-- |
-- Class of values that have a duration.
--
-- Law Duration
--
-- @
-- '_duration' x = ('offset' x '.-.' 'onset' x)
-- @
--
class HasDuration a where
  _duration :: a -> Duration

instance HasDuration Time where
  _duration = 0

instance HasDuration Duration where
  _duration = id

instance HasDuration Span where
  _duration = snd . view delta

--
-- By convention, we treat pairs and triplets as having the form
-- (t,x), (d,x) and (t,d,x) where t has a position and d has a
-- duration. This makes it convenient to represent simple event
-- lists as [(Time, Duration, a)] without needing any special
-- structure.
--

instance HasDuration a => HasDuration (a, b) where
  _duration (d,_) = _duration d

instance HasDuration b => HasDuration (a, b, c) where
  _duration (_,d,_) = _duration d

instance HasDuration a => HasDuration (Product a) where
  _duration (Product x) = _duration x

instance HasDuration a => HasDuration (Sum a) where
  _duration (Sum x) = _duration x

instance HasDuration a => HasDuration (Min a) where
  _duration (Min x) = _duration x

instance HasDuration a => HasDuration (Max a) where
  _duration (Max x) = _duration x

-- For HasDuration [a] we assume parallel composition and
-- use the HasPosition instance, see Music.Time.Position.

instance (HasDuration a, HasDuration b) => HasDuration (Either a b) where
  _duration (Left x)  = _duration x
  _duration (Right x) = _duration x

-- |
-- Access the duration.
--
duration :: (Transformable a, HasDuration a) => Lens' a Duration
duration = lens _duration (flip stretchTo)
{-# INLINE duration #-}

-- |
-- Stretch a value to have the given duration.
--
stretchTo :: (Transformable a, HasDuration a) => Duration -> a -> a
stretchTo d x = (d ^/ _duration x) `stretch` x
{-# INLINE stretchTo #-}

{-

-- TODO more general pattern here
withDurationR :: (Functor f, HasDuration a) => f a -> f (Duration, a)
withDurationR = fmap $ \x -> (_duration x, x)

withDurationL :: (Contravariant f, HasDuration a) => f (Duration, a) -> f a
withDurationL = contramap $ \x -> (_duration x, x)

mapWithDuration :: HasDuration a => (Duration -> a -> b) -> a -> b
mapWithDuration = over dual withDurationL . uncurry
  where
    dual :: Iso (a -> b) (c -> d) (Op b a) (Op d c)
    dual = iso Op getOp

-}



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
-------------------------------------------------------------------------------------



-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012–2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

-- import           Control.Applicative
-- import           Control.Monad
-- import           Control.Monad.Compose
-- import           Control.Monad.Plus
-- import           Data.Foldable          (Foldable)
-- import qualified Data.Foldable          as Foldable
-- import           Data.Traversable       (Traversable)
-- import qualified Data.Traversable       as T
-- import           Data.Typeable

-- import Music.Time.Split
-- import Music.Time.Reverse
-- import Music.Time.Nominal
-- import Music.Time.Voice

newtype Graces f a = Graces { getGraces :: (Nominal f a, f a, Nominal f a) }
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- TODO move
instance Alternative f => Alternative (Nominal f) where
  empty = Nominal empty
  Nominal a <|> Nominal b = Nominal (a <|> b)

instance (Applicative f, Alternative f) => Applicative (Graces f) where
  pure x = Graces (empty, pure x, empty)

-- deriving instance Transformable a => Transformable (Graces a) where
-- deriving instance Splittable a => Splittable (Graces a) where
-- deriving instance Reversible a => Reversible (Graces a) where



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
-- Provides miscellaneous conversions.
--
-------------------------------------------------------------------------------------

-- import           Control.Applicative
-- import           Control.Lens hiding (transform, time)
-- import           Control.Monad
-- import           Control.Monad.Plus
-- import           Data.AffineSpace
-- import           Data.AffineSpace.Point
-- import           Data.Foldable          (Foldable (..))
-- import           Data.Ord
-- import           Data.Ratio
-- import           Data.Semigroup
-- import           Data.String
-- import           Data.Traversable
-- import           Data.VectorSpace
--
-- import           Music.Time
--
-- import qualified Data.Foldable          as Foldable
-- import qualified Data.List              as List


reactiveToVoice' :: Span -> Reactive a -> Voice a
reactiveToVoice' (view range -> (u,v)) r = (^. voice) $ fmap (^. stretched) $ durs `zip` (fmap (r `atTime`) times)
    where
        times = 0 : filter (\t -> u < t && t < v) (occs r)
        durs  = toRelativeTimeN' v times
{-# DEPRECATED reactiveToVoice' "" #-}

-- |
-- Convert a score to a voice. Fails if the score contain overlapping events.
--
scoreToVoice :: Transformable a => Score a -> Voice (Maybe a)
scoreToVoice = (^. voice) . fmap (^. stretched) . fmap throwTime . addRests . (^. events)
    where
       throwTime (t,d,x) = (d,x)
       addRests = concat . snd . mapAccumL g 0
           where
               g u (t, d, x)
                   | u == t    = (t .+^ d, [(t, d, Just x)])
                   | u <  t    = (t .+^ d, [(u, t .-. u, Nothing), (t, d, Just x)])
                   | otherwise = error "scoreToVoice: Strange prevTime"
{-# DEPRECATED scoreToVoice "" #-}

-- |
-- Convert a voice to a score.
--
voiceToScore :: Voice a -> Score a
voiceToScore = scat . fmap g . (^. stretcheds) where g = (^. stretchee) . fmap return
{-# DEPRECATED voiceToScore "" #-}

{-
-- | Join voices in a given part into a score.
voicesToScore :: HasPart a => [(Part a, Voice a)] -> Score a
voicesToScore = pcat . fmap (voiceToScore . uncurry (\n -> fmap (setPart n)))
-}

-- |
-- Convert a voice which may contain rests to a score.
--
voiceToScore' :: Voice (Maybe a) -> Score a
voiceToScore' = mcatMaybes . voiceToScore
{-# DEPRECATED voiceToScore' "" #-}


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
-- Rhythmical quantization.
--
-------------------------------------------------------------------------------------




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
-------------------------------------------------------------------------------------

--
-- import           Music.Time.Types
--
-- import           Data.Ratio
--
-- import           Control.Applicative
-- import           Control.Lens           hiding (Indexable, Level, above, below,
--                                          index, inside, parts, reversed,
--                                          transform, (<|), (|>))
-- import           Data.AffineSpace
-- import           Data.AffineSpace.Point
-- import           Data.Map               (Map)
-- import qualified Data.Map               as Map
-- import           Data.Semigroup
-- import           Data.Semigroup.Instances ()
-- import           Data.Sequence          (Seq)
-- import qualified Data.Sequence          as Seq
-- import           Data.Set               (Set)
-- import qualified Data.Set               as Set
-- import           Data.VectorSpace       hiding (Sum (..))
--
-- |
-- Class of values that can be transformed (i.e. scaled and moved) in time.
--
-- Law
--
-- @
-- transform mempty = id
-- transform (s \<> t) = transform s . transform t
-- transform (s \<> negateV s) = id
-- @
--
-- Law
--
-- @
-- onset (delay n a)       = n ^+. onset a
-- offset (delay n a)      = n ^+. offset a
-- duration (stretch n a)  = n * duration a
-- duration (compress n a) = duration a / n
-- @
--
-- @
-- delay n b ! t    = b ! (t .-^ n)
-- undelay n b ! t  = b ! (t .+^ n)
-- @
--
-- Lemma
--
-- @
-- duration a = duration (delay n a)
-- @
--
class Transformable a where
  transform :: Span -> a -> a

instance Transformable () where
  transform _ = id

instance Transformable Bool where
  transform _ = id

instance Transformable Ordering where
  transform _ = id

instance Transformable Char where
  transform _ = id

instance Transformable Int where
  transform _ = id

instance Transformable Integer where
  transform _ = id

instance Transformable a => Transformable (Ratio a) where
  transform _ = id

instance Transformable Float where
  transform _ = id

instance Transformable Double where
  transform _ = id

instance Transformable Duration where
  (view delta -> (_, d1)) `transform` d2 = d1 * d2

instance Transformable Time where
  (view delta -> (t1, d1)) `transform` t2 = t1 ^+^ d1 *^ t2

instance Transformable Span where
  transform = (<>)

instance Transformable a => Transformable (Option a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (Last a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (Sum a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (Product a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (b, a) where
  transform t = fmap (transform t)

instance Transformable a => Transformable [a] where
  transform t = fmap (transform t)

-- instance Transformable a => Transformable (Seq a) where
  -- transform t = fmap (transform t)

instance (Ord a, Transformable a) => Transformable (Set a) where
  transform t = Set.map (transform t)

instance (Ord k, Transformable a) => Transformable (Map k a) where
  transform t = Map.map (transform t)

-- |
-- Functions transform by conjugation, i.e. we reverse-transform the argument
-- and transform the result.
--
instance (Transformable a, Transformable b) => Transformable (a -> b) where
  transform t = (`whilst` negateV t)


-- |
-- Apply the inverse of the given transformation.
--
-- @
-- 'itransform' s = 'transform' ('negateV' s)
-- @
--
itransform :: Transformable a => Span -> a -> a
itransform s = transform (negateV s)

-- |
-- View the given value in the context of the given transformation.
--
transformed :: (Transformable a, Transformable b) => Span -> Iso a b a b
transformed s = iso (transform s) (itransform s)


-- |
-- Transforms a lens of to a 'Transformable' type to act inside a transformation.
--
itransformed :: (Transformable a, Transformable b) => Span -> Iso a b a b
itransformed s = transformed (negateV s)

spanned = itransformed
{-# DEPRECATED spanned "Use itransformed" #-}

-- |
-- Apply a function under transformation.
--
-- >>> stretch 2 `whilst` delaying 2 $ (1 <-> 2)
-- 4 <-> 6
--
whilst :: (Transformable a, Transformable b) => (a -> b) -> Span -> a -> b
-- f `whilst` t = transform (negateV t) . f . transform t
f `whilst` t = over (transformed t) f


{-
delayed :: (Transformable a, Transformable b) => Time -> Iso a b a b
delayed = transformed . delayingTime

stretched :: (Transformable a, Transformable b) => Duration -> Iso a b a b
stretched = transformed . stretching
-}

-- |
-- A transformation that moves a value forward in time.
--
delaying :: Duration -> Span
delaying x = (0 .+^ x) >-> 1
delayingTime x = x >-> 1

-- |
-- A transformation that stretches (augments) a value by the given factor.
--
stretching :: Duration -> Span
stretching x = 0 >-> x

-- |
-- A transformation that moves a value backward in time.
--
undelaying :: Duration -> Span
undelaying x = delaying (negate x)

-- |
-- A transformation that compresses (diminishes) a value by the given factor.
--
compressing :: Duration -> Span
compressing x = stretching (recip x)

-- |
-- Moves a value forward in time.
--
delay :: Transformable a => Duration -> a -> a
delay = transform . delaying

-- |
-- Moves a value backward in time. Equnitvalent to @'stretch' . 'negate'@.
--
undelay :: Transformable a => Duration -> a -> a
undelay = transform . undelaying

-- |
-- Stretches (augments) a value by the given factor.
--
stretch :: Transformable a => Duration -> a -> a
stretch = transform . stretching

-- |
-- Compresses (diminishes) a score. Equnitvalent to @'stretch' . 'recip'@.
--
compress :: Transformable a => Duration -> a -> a
compress = transform . compressing


-- |
-- Delay relative to 'origin'.
--
-- Provided for situations when we really want to use 'startAt', but the
-- type does not have an instance for 'HasPosition' and we can assume that
-- the value is starting at time zero.
--
delayTime :: Transformable a => Time -> a -> a
delayTime = transform . delayingTime


--
-- $musicTimeSpanConstruct
--
-- - To convert a span to a pair, use @s^.'delta'@.
-- - To construct a span from a pair, use @(t, d)^.'from' 'delta'@.
--

--
-- $musicTimeSpanLaws
--
-- > forall s . id `whilst` s = id
-- > forall s . return `whilstM` s = return
-- > forall s . extract `whilstW` s = extract


-- We really must flip all these functions. To do:
--
--    1) Come up with some other name for the infix version
--    2) Acknowledge that this is a valid Lens (when flipped)
--
-- Perhaps we should call the inline version `whilst`, as in @f `whilst` delaying 2@?

{-

-- flip whilstM is a lens
flip whilstM :: (Functor f, Transformable a, Transformable b) => (a -> f b) -> Span -> a -> f b
s `flip whilstM` f = fmap (transform (negateV t)) . f . transform t

-- is this the same as transformed?


From lens:
  iso sa bt = dimap sa (fmap bt)
From profunctor:
  dimap ab cd bc = cd . bc . ab
  dimap ab cd    = \f -> cd . f . ab


flip whilstM = transformed
flip whilstM = \s -> iso (transform s) (itransform s)
flip whilstM = \s -> dimap (transform s) (fmap $ itransform s)
flip whilstM = \s f -> (fmap $ itransform s) . f . transform s
flip (\f t -> fmap (transform (negateV t)) . f . transform t) = \s f -> (fmap $ itransform s) . f . transform s

\t f -> fmap (transform (negateV t)) . f . transform t
=
\t f -> (fmap $ itransform t) . f . transform t

\t f -> fmap (itransform t) . f . transform t
=
\t f -> fmap (itransform t) . f . transform t




Something similar to whilstL* is being used in Note/Delayed/Stretched
Are they the same?

whilstL l f (s,a)  = (s,) <$> (l $ transformed s f) a
whilstL id f (s,a) = (s,) <$> (transformed s f) a
whilstL id         = \f (s,a) -> (s,) <$> (transformed s f) a

whilstL id
  :: (Transformable a, Transformable b, Functor f) =>
     (a -> f b) -> (Span, a) -> f (Span, b)

-}

-- dofoo
  -- :: Functor f => (t -> t2) -> (a1 -> a) -> (t2 -> f a1) -> (t1, t) -> f (t1, a)
dofoo v w = \f (s,a) -> (s,) <$> w s <$> f ((v s) a)



dobar :: (Functor f)

  =>
  (sp -> (s -> f t) -> (s -> f t))
  -> ((s -> f t) -> a -> f b)
  -> (s -> f t)  -> ((sp, a) -> f (sp, b))

dobar q l = \f (s,a) -> (s,) <$> (l (q s f)) a

-- whilstL2 :: (Transformable a, Transformable b) => Lens (Span, a) (Span, b) a b
whilstL2 = dofoo (transform) (transform . negateV)

whilstL :: (Functor f, Transformable a, Transformable b)
  => LensLike f s t a b
  -> LensLike f (Span,s) (Span,t) a b
  -- whilstL l = whilstL2 . l
whilstL l = dobar transformed l

{-
If we could rewrite (whilstL l) as (whilstLXX . l)

-}
{-

whilstLT :: (Functor f, Transformable a, Transformable b)
  => LensLike f s t a b
  -> LensLike f (Time,s) (Time,t) a b
whilstLT = dobar delayed

whilstLD :: (Functor f, Transformable a, Transformable b)
  => LensLike f s t a b
  -> LensLike f (Duration,s) (Duration,t) a b
whilstLD = dobar stretched
-}


-- |
-- Apply a function under transformation.
--
whilstDelay :: (Transformable a, Transformable b) => (a -> b) -> Time -> a -> b
whilstDelay     = flip (flip whilst . delaying . (.-. 0))

-- |
-- Apply a function under transformation.
--
whilstStretch :: (Transformable a, Transformable b) => (a -> b) -> Duration -> a -> b
whilstStretch = flip (flip whilst . stretching)

-- |
-- The conjugate of two spans.
--
conjugateS :: Span -> Span -> Span
conjugateS t1 t2  = negateV t1 <> t2 <> t1


-- |
-- Transforms a lens of to a 'Transformable' type to act inside a transformation.
--
-- Designed to be used infix, as in
--
-- @
-- l `onSpan` (2 \<-> 3)
-- @
--
onSpan :: (Transformable s, Transformable t, Functor f)
  => LensLike f s t a b -> Span -> LensLike f s t a b
f `onSpan` s = spanned s . f
-- TODO name

-- deriving instance Functor Sum
-- deriving instance Functor Product

-- Empty so far




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
-------------------------------------------------------------------------------------



{-
    Rules:

        * Functions may depend on any  in the lastest Haskell Platform release
        * All functions but those in Prelude must be referred to with their full,
          qualified names (i.e. Data.List.unfoldr).
        * Each function must have a unique name (so the whole file is a loadable ).
        * Each function should have a synopisis, like:

            -- | Ordinary Haddock commentary ...
            -- > category: Categories (please use the common Hackage names)
            -- > depends : base (all packages in HP that the function depends on)

-}

-- import Control.Lens
-- import Control.Monad.Plus
-- import Control.Applicative
-- import qualified Data.Char
-- import qualified Data.Monoid
-- import qualified Data.List
-- import qualified Data.Ratio
-- import Data.Functor.Contravariant

-- | Divide a list into parts of maximum length n.
-- > category : List
-- > depends : base
divideList :: Int -> [a] -> [[a]]
divideList n xs
    | length xs <= n = [xs]
    | otherwise = [take n xs] ++ (divideList n $ drop n xs)

-- | Group a list into sublists whereever a predicate holds. The matched element
--   is the first in the sublist.
--
--   > splitWhile isSpace "foo bar baz"
--   >    ===> ["foo"," bar"," baz"]
--   >
--   > splitWhile (> 3) [1,5,4,7,0,1,2]
--   >    ===> [[1],[5],[4],[7,0,1,2]]
--
-- > category : List
-- > depends : base
splitWhile :: (a -> Bool) -> [a] -> [[a]]
splitWhile p xs = case splitWhile' p xs of
    []:xss -> xss
    xss    -> xss
    where
        splitWhile' p []     = [[]]
        splitWhile' p (x:xs) = case splitWhile' p xs of
            (xs:xss) -> if p x then []:(x:xs):xss else (x:xs):xss


-- | Break up a list into parts of maximum length n, inserting the given list as separator.
--   Useful for breaking up strings, as in @breakList 80 "\n" str@.
--
-- > category : List
-- > depends : base
breakList :: Int -> [a] -> [a] -> [a]
breakList n z = mconcat . Data.List.intersperse z . divideList n

-- | Map over the indices and elements of list.
-- > category : List
-- > depends : base
mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed f as = map (uncurry f) (zip is as)
    where
        n  = length as - 1
        is = [0..n]

-- test

-- | Duplicate an element.
-- > category: Combinator, Tuple
-- > depends: base
dup :: a -> (a,a)
dup x = (x,x)

-- | Unfold a partial function. This is a simpler version of 'Data.List.unfoldr'.
-- > category: Function, List
-- > depends: base
unf :: (a -> Maybe a) -> a -> [a]
unf f = Data.List.unfoldr (fmap dup . f)

-- |
-- Map over first elements of a list.
-- Biased on first element for shorter lists.
-- > category: List
-- > depends: base
mapF f = mapFTL f id id

-- |
-- Map over all but the first and last elements of a list.
-- Biased on middle elements for shorter lists.
-- > category: List
-- > depends: base
mapT f = mapFTL id f id

-- |
-- Map over last elements of a list.
-- Biased on last element for shorter lists.
-- > category: List
-- > depends: base
mapL f = mapFTL id id f

-- |
-- Map over first, middle and last elements of list.
-- Biased on first, then on first and last for short lists.
--
-- > category: List
-- > depends: base
mapFTL :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapFTL f g h = go
    where
        go []    = []
        go [a]   = [f a]
        go [a,b] = [f a, h b]
        go xs    = [f $ head xs]          ++
                   map g (tail $ init xs) ++
                   [h $ last xs]

-- |
-- Extract the first consecutive sublist for which the predicate returns true, or
-- the empty list if no such sublist exists.
-- > category: List
-- > depends: base
filterOnce :: (a -> Bool) -> [a] -> [a]
filterOnce p = Data.List.takeWhile p . Data.List.dropWhile (not . p)


-- | Returns all rotations of the given list. Given an infinite list, returns an infinite
-- list of rotated infinite lists.
-- > category: List
-- > depends: base
rots :: [a] -> [[a]]
rots xs = init (zipWith (++) (Data.List.tails xs) (Data.List.inits xs))

-- |
-- > category: List
-- > depends: base
rotl :: [a] -> [a]
rotl []     = []
rotl (x:xs) = xs ++ [x]

-- |
-- > category: List
-- > depends: base
rotr :: [a] -> [a]
rotr [] = []
rotr xs = last xs : init xs

-- |
-- > category: List
-- > depends: base
rotated :: Int -> [a] -> [a]
rotated = go
    where
        go n as
            | n >= 0 = iterate rotr as !! n
            | n <  0 = iterate rotl as !! abs n


curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 = curry . curry . (. tripl)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 = (. untripl) . uncurry . uncurry

untripl :: (a,b,c) -> ((a,b),c)
untripl (a,b,c) = ((a,b),c)

tripl :: ((a,b),c) -> (a,b,c)
tripl ((a,b),c) = (a,b,c)

tripr :: (a,(b,c)) -> (a,b,c)
tripr (a,(b,c)) = (a,b,c)


-- TODO mo
partial2 :: (a -> b      -> Bool) -> a -> b      -> Maybe b
partial3 :: (a -> b -> c -> Bool) -> a -> b -> c -> Maybe c
partial2 f = curry  (fmap snd  . partial (uncurry f))
partial3 f = curry3 (fmap (view _3) . partial (uncurry3 f))

-- | Case matching on lists.
-- > category: List
-- > depends: base
list :: r -> ([a] -> r) -> [a] -> r
list z f [] = z
list z f xs = f xs

-- | Merge lists.
-- > category: List
-- > depends: base
merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

-- | Merge lists.
-- > category: List
-- > depends: base
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f = mergeBy' $ (fmap.fmap) orderingToBool f
    where
        orderingToBool LT = True
        orderingToBool EQ = True
        orderingToBool GT = False

mergeBy' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeBy' pred xs []         = xs
mergeBy' pred [] ys         = ys
mergeBy' pred (x:xs) (y:ys) =
    case pred x y of
        True  -> x: mergeBy' pred xs (y:ys)
        False -> y: mergeBy' pred (x:xs) ys

-- | Compose all functions.
-- > category: Function
-- > depends: base
composed :: [b -> b] -> b -> b
composed = Prelude.foldr (.) id

-- | Separate a ratio.
-- > category: Math
-- > depends: base
unRatio :: Integral a => Data.Ratio.Ratio a -> (a, a)
unRatio x = (Data.Ratio.numerator x, Data.Ratio.denominator x)

-- | Nicer printing of ratio as ordinary fractions.
-- > category: Math
-- > depends: base
showRatio :: (Integral a, Show a) => Data.Ratio.Ratio a -> String
showRatio (realToFrac -> (unRatio -> (x, 1))) = show x
showRatio (realToFrac -> (unRatio -> (x, y))) = "(" ++ show x ++ "/" ++ show y ++ ")"


-- Replace all contigous ranges of equal values with [Just x, Nothing, Nothing ...]
-- > category: List
-- > depends: base
retainUpdates :: Eq a => [a] -> [Maybe a]
retainUpdates = snd . Data.List.mapAccumL g Nothing where
    g Nothing  x = (Just x, Just x)
    g (Just p) x = (Just x, if p == x then Nothing else Just x)


-- Generic version of 'replicate'.
-- > category: List
-- > depends: base
replic :: Integral a => a -> b -> [b]
replic n = replicate (fromIntegral n)

-- Swap components.
-- > category: Tuple
-- > depends: base
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Interleave a list with the next consecutive element.
--
-- For any xs
--
-- > lenght xs == length (withNext xs)
--
-- If @xs@ is a finite list
--
-- > isNothing  $ snd $ last $ withNext xs == True
-- > all isJust $ snd $ init $ withNext xs == True
--
-- If @xs@ is an infinite list
--
-- > all isJust $ snd $ withNext xs == True
--
-- > category: List
-- > depends: base
withNext :: [a] -> [(a, Maybe a)]
withNext = fmap (\(p,c,n) -> (c,n)) . withPrevNext

withPrev :: [a] -> [(Maybe a, a)]
withPrev = fmap (\(p,c,n) -> (p,c)) . withPrevNext

-- withNext = go
--     where
--         go []       = []
--         go [x]      = [(x, Nothing)]
--         go (x:y:rs) = (x, Just y) : withNext (y : rs)

withPrevNext :: [a] -> [(Maybe a, a, Maybe a)]
withPrevNext xs = zip3 (pure Nothing ++ fmap Just xs) xs (fmap Just (tail xs) ++ repeat Nothing)

-- Map over a list with the next consecutive element.
--
-- > category: List
-- > depends: base
mapWithNext :: (a -> Maybe a -> b) -> [a] -> [b]
mapWithNext f = map (uncurry f) . withNext

mapWithPrev :: (Maybe a -> a -> b) -> [a] -> [b]
mapWithPrev f = map (uncurry f) . withPrev

mapWithPrevNext :: (Maybe a -> a -> Maybe a -> b) -> [a] -> [b]
mapWithPrevNext f = map (uncurry3 f) . withPrevNext

-- |
-- Rotate a list.
--
-- > rotate n xs == id iff (n `mod` length xs) == 0
-- > rotate (-n) . rotate n == id
rotate :: Int -> [a] -> [a]
rotate n xs = drop n' xs ++ take n' xs
  where
    n' = negate n `mod` length xs

--------







toDouble :: Real a => a -> Double
toDouble = realToFrac

through :: Applicative f =>
  Lens' s a
  -> Lens s t a b
  -> Lens (f s) (f t) (f a) (f b)
through lens1 lens2 = lens getter (flip setter)
  where
    getter = fmap (view lens1)
    setter = liftA2 (set lens2)
{-# INLINE through #-}

_zipList :: Iso [a] [b] (ZipList a) (ZipList b)
_zipList = iso ZipList getZipList


single :: Prism' [a] a
single = prism' return $ \xs -> case xs of
  [x] -> Just x
  _   -> Nothing
{-# INLINE single #-}

tripped :: Iso ((a, b), c) ((a', b'), c') (a, b, c) (a', b', c')
tripped = iso tripl untripl
{-# INLINE tripped #-}

-- floor' :: RealFrac a => a -> a
-- floor' = fromIntegral . floor

-- Like Data.Ord.comparing
-- (Are both variants of contramap?)
inspecting :: Eq a => (b -> a) -> b -> b -> Bool
inspecting f x y = f x == f y

inspectingBy :: (b -> a) -> (a -> a -> Bool) -> (b -> b -> Bool)
inspectingBy f e = getEquivalence $ contramap f $ Equivalence e
-- inspectingBy f p x y = f x `p` f y



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
-------------------------------------------------------------------------------------


--
-- import           Control.Lens hiding ((|>), (<|))
-- import           Data.AffineSpace
-- import           Data.AffineSpace.Point
-- -- import           Data.Monoid.WithSemigroup
-- import           Data.Semigroup
-- import           Data.VectorSpace
--
-- import           Music.Time.Reverse
-- import           Music.Time.Split
--

--
-- TODO names
-- Especially 'after' is counter-intuitive
--

-- |
-- Move a value so that
--
-- @
-- '_offset' (a `lead` b) = '_onset' b
-- @
--
--
lead   :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
a `lead` b   = placeAt 1 (b `_position` 0) a

-- |
-- Move a value so that
--
-- @
-- '_offset' a = '_onset' (a `follow` b)
-- @
--
follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
a `follow` b = placeAt 0 (a `_position` 1) b

-- |
-- Move a value so that
--
after :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `after` b =  a <> (a `follow` b)

-- |
-- Move a value so that
--
before :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `before` b =  (a `lead` b) <> b

-- |
-- A value followed by its reverse (retrograde).
--
palindrome :: (Semigroup a, Reversible a, HasPosition a) => a -> a
palindrome a = a `after` rev a

infixr 6 |>
infixr 6 <|

-- |
-- An infix alias for 'after'.
--
(|>) :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(|>) = after

-- |
-- An infix alias for 'before'.
--
(<|) :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(<|) = before

-- infixr 6 >|
-- infixr 6 |<

-- |
-- Compose a list of sequential objects, with onset and offset tangent to one another.
--
-- For non-positioned types, this is the often same as 'mconcat'
-- For positioned types, this is the same as 'afterAnother'
--
scat :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => [a] -> a
scat = Prelude.foldr (|>) mempty

-- |
-- Compose a list of parallel objects, so that their local origins align.
--
-- This not possible for non-positioned types, as they have no notion of an origin.
-- For positioned types this is the same as 'mconcat'.
--
pcat :: (Semigroup a, Monoid a) => [a] -> a
pcat = Prelude.foldr (<>) mempty

-- |
-- Move a value so that its era is equal to the era of another value.
--
-- @
-- 'Score' a -> 'Score' a -> 'Score' a
-- @
--
during :: (HasPosition a, HasPosition b, Transformable a, Transformable b) => a -> b -> a
y `during` x = set era (view era x) y

-- |
-- Like '<>', but scaling the second agument to the duration of the first.
--
-- @
-- 'Score' a -> 'Score' a -> 'Score' a
-- @
--
sustain :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
x `sustain` y = x <> y `during` x

-- |
-- Repeat exact amount of times.
--
-- @
-- 'Int' -> 'Score' a -> 'Score' a
-- @
--
times :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => Int -> a -> a
times n = scat . replicate n


-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012–2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides a way to annotate data-types with 'Transformable' meta-data.
-- Inspired by Clojure meta-data and Diagrams styles.
--
-------------------------------------------------------------------------------------



-- import           Control.Applicative
-- import           Control.Comonad
-- import           Control.Lens              hiding (transform)
-- import           Control.Monad.Plus
-- import           Data.Functor.Rep -- TODO experimental
-- import           Data.Foldable             (Foldable)
-- import qualified Data.Foldable             as F
-- import qualified Data.List                 as List
-- import           Data.Functor.Adjunction  (unzipR)
-- import           Data.Map                  (Map)
-- import qualified Data.Map                  as Map
-- import           Data.Maybe
-- import           Data.Semigroup
-- import           Data.Set                  (Set)
-- import qualified Data.Set                  as Set
-- import           Data.String
-- import           Data.Typeable
-- import           Data.Void
-- import           Data.Functor.Couple
--
-- import           Music.Time.Internal.Util
-- import           Music.Time.Reverse
-- import           Music.Time.Split
-- import           Music.Time.Transform
--
-- | Class of values that can be wrapped.
type AttributeClass a = (Typeable a, Monoid a, Semigroup a)

-- | Class of values that can be wrapped and transformed.
type TAttributeClass a = (Transformable a, AttributeClass a)

-- | An existential wrapper type to hold attributes.
data Attribute :: * where
  Attribute  :: AttributeClass a => a -> Attribute
  TAttribute :: TAttributeClass a  => a -> Attribute

-- | Wrap up an attribute.
wrapAttr :: AttributeClass a => a -> Attribute
wrapAttr = Attribute

-- | Wrap up a transformable attribute.
wrapTAttr :: TAttributeClass a => a -> Attribute
wrapTAttr = TAttribute

-- | Convert something from an attribute.
--   Also works with transformable attributes
unwrapAttr :: AttributeClass a => Attribute -> Maybe a
unwrapAttr (Attribute a)  = cast a
unwrapAttr (TAttribute a) = cast a

instance Semigroup Attribute where
  (Attribute a1) <> a2 = case unwrapAttr a2 of
    Nothing  -> error "Attribute.(<>) mismatch"
    Just a2' -> Attribute (a1 <> a2')
  (TAttribute a1) <> a2 = case unwrapAttr a2 of
    Nothing  -> error "Attribute.(<>) mismatch"
    Just a2' -> TAttribute (a1 <> a2')

instance Transformable Attribute where
  transform _ (Attribute a) = Attribute a
  transform s (TAttribute a) = TAttribute (transform s a)

instance Splittable Attribute where
  split _ x = (x,x)

instance Reversible Attribute where
  rev = id

-- Meta is Transformable because the contents of the map is transformable
newtype Meta = Meta (Map String Attribute)
  deriving (Transformable, Reversible, Splittable)

instance Semigroup Meta where
  Meta s1 <> Meta s2 = Meta $ Map.unionWith (<>) s1 s2

-- | The empty meta contains no attributes; composition of metas is
--   a union of attributes; if the two metas have attributes of the
--   same type they are combined according to their semigroup
--   structure.
instance Monoid Meta where
  mempty = Meta Map.empty
  mappend = (<>)

-- | Convert something to meta-data.
-- wrapTMeta :: forall a. TAttributeClass a => a -> Meta
-- wrapTMeta a = Meta $ Map.singleton key $ wrapTAttr a
--   where
--     key = show $ typeOf (undefined :: a)
--
-- -- | Convert something from meta-data.
-- unwrapMeta :: forall a. AttributeClass a => Meta -> Maybe a
-- unwrapMeta (Meta s) = (unwrapAttr =<<) $ Map.lookup key s
-- -- Note: unwrapAttr should never fail
--   where
--     key = show . typeOf $ (undefined :: a)
--
-- -- | Convert something from meta-data.
-- --   Also works with transformable attributes
-- wrapMeta :: forall a. AttributeClass a => a -> Meta
-- wrapMeta a = Meta $ Map.singleton key $ wrapAttr a
--   where
--     key = show $ typeOf (undefined :: a)


-- | Type class for things which have meta-data.
class HasMeta a where
  -- | Access the meta-data.
  meta :: Lens' a Meta

instance Show Meta where
  show _ = "{ meta }"

instance HasMeta Meta where
  meta = ($)

instance HasMeta a => HasMeta (Maybe a) where
  meta = lens viewM $ flip setM
    where
      viewM Nothing  = mempty
      viewM (Just x) = view meta x
      setM m = fmap (set meta m)

instance HasMeta a => HasMeta (b, a) where
  meta = _2 . meta

instance HasMeta a => HasMeta (Twain b a) where
  meta = _Wrapped . meta

-- | Extract meta-data.
getMeta :: HasMeta a => a -> Meta
getMeta = view meta

-- | Update meta-data.
setMeta :: HasMeta a => Meta -> a -> a
setMeta = set meta

-- | Map over meta-data.
mapMeta :: HasMeta a => (Meta -> Meta) -> a -> a
mapMeta = over meta

-- | Apply meta-information by combining it with existing meta-information.
applyMeta :: HasMeta a => Meta -> a -> a
applyMeta m = over meta (<> m)
{-

-- | Update a meta attribute.
setMetaAttr :: (AttributeClass b, HasMeta a) => b -> a -> a
setMetaAttr a = applyMeta (wrapMeta a)

-- | Update a meta attribute.
setMetaTAttr :: (TAttributeClass b, HasMeta a) => b -> a -> a
setMetaTAttr a = applyMeta (wrapTMeta a)

-- | Apply a function without affecting meta-data.
preserveMeta :: (HasMeta a, HasMeta b) => (a -> b) -> a -> b
preserveMeta f x = let m = view meta x in set meta m (f x)
-}

-- |
-- Annotate an arbitrary type with meta-data, preserving instances of
-- all common type classes. In particular 'Functor' and 'Applicative' is lifted and
-- @'Compose' 'AddMeta'@ is semantically equivalent to 'Identity'.

-- Meta-data is carried along with the annotated value. It defaults to 'mempty'
-- in 'pure'. When composing values using '<*>', 'liftA2' etc, meta-data is composed
-- using 'mappend'.
--
-- Similar to the approach taken in Clojure, meta-data does not contribute to ordering,
-- so both 'Eq' and 'Ord' ignore the meta-data.
--
-- You can access the meta-data using 'meta', and the annotated value using 'annotated'.
--
newtype AddMeta a = AddMeta { getAddMeta :: Twain Meta a }
  deriving (
    Show, Functor, Foldable, Typeable, Applicative, Monad, Comonad,
    Semigroup, Monoid, Num, Fractional, Floating, Enum, Bounded,
    Integral, Real, RealFrac,
    Eq, Ord
    )

instance Wrapped (AddMeta a) where
  type Unwrapped (AddMeta a) = Twain Meta a
  _Wrapped' = iso getAddMeta AddMeta

instance Rewrapped (AddMeta a) (AddMeta b)

instance HasMeta (AddMeta a) where
  -- twain, pair, element
  meta = _Wrapped . _Wrapped . _1


-- instance FunctorWithIndex i AddMeta where
  -- imap f = over annotated $ imap f
--
-- instance FoldableWithIndex Span Score where
--   ifoldMap f (Score (m,x)) = ifoldMap f x
--
-- instance TraversableWithIndex Span Score where
--   itraverse f (Score (m,x)) = fmap (\x -> Score (m,x)) $ itraverse f x

instance Transformable a => Transformable (AddMeta a) where
  transform t = over meta (transform t) . over annotated (transform t)

instance Reversible a => Reversible (AddMeta a) where
  rev = over meta rev . over annotated rev

instance Splittable a => Splittable (AddMeta a) where
  split t = unzipR . fmap (split t)

instance HasPosition a => HasPosition (AddMeta a) where
  _onset    = _onset . extract
  _offset   = _offset . extract
  _position = _position . extract

instance HasDuration a => HasDuration (AddMeta a) where
  _duration = _duration . extract

-- |
-- Access the annotated value.
--
-- @
-- over annotated = fmap
-- @
--
annotated :: Lens (AddMeta a) (AddMeta b) a b
annotated = unsafeAnnotated

-- |
-- Access the annotated value.
--
-- @
-- view fromAnnotated = pure
-- @
--
unannotated :: Getter a (AddMeta a)
unannotated = from unsafeAnnotated

-- |
-- Access the annotated value. This is only an isomorphism up to meta-data
-- equivalence. In particular @under unsafeAnnotated@ leads to meta-data being
-- thrown away. See 'annotated' and 'unannotated' for safe (but less general)
-- definitions.
--
-- @
-- over annotated = fmap
-- @
--
unsafeAnnotated :: Iso (AddMeta a) (AddMeta b) a b
unsafeAnnotated = _Wrapped . extracted


-- Nice generalizations
-- TODO move

extracted :: (Applicative m, Comonad m) => Iso (m a) (m b) a b
extracted = iso extract pure

extractedRep :: (Representable m, w ~ Rep m, Monoid w) => Iso (m a) (m b) a b
extractedRep = iso extractRep pureRep


-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012–2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

--
-- import           Control.Applicative
-- import           Control.Monad
-- import           Control.Monad.Compose
-- import           Control.Monad.Plus
-- import           Data.Foldable          (Foldable)
-- import qualified Data.Foldable          as Foldable
-- import           Data.Traversable       (Traversable)
-- import qualified Data.Traversable       as T
-- import           Data.Typeable
--
-- import Music.Time.Split
-- import Music.Time.Reverse
--
newtype Nominal f a = Nominal { getNominal :: f a }
  deriving (
  	Eq,
  	Ord,
  	Read,
  	Show,
  	Functor,
  	Foldable,
  	Traversable,
  	Monad
  )

instance Applicative f => Applicative (Nominal f) where
  pure = Nominal . pure
  Nominal f <*> Nominal x = Nominal (f <*> x)

instance Transformable (Nominal f a) where
  transform _ = id

instance Splittable (Nominal f a) where
  split _ x = (x,x)

instance Reversible (f a) => Reversible (Nominal f a) where
  rev (Nominal x) = Nominal (rev x)

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
-------------------------------------------------------------------------------------



-- import           Control.Applicative
-- import           Control.Comonad
-- import           Control.Lens             hiding (Indexable, Level, above,
--                                            below, index, inside, parts,
--                                            reversed, transform, (<|), (|>))
-- import           Data.PairMonad
-- import           Data.String
-- import           Data.VectorSpace
-- import           Data.Foldable            (Foldable)
-- import qualified Data.Foldable            as Foldable
-- import           Data.Typeable
--
-- import           Music.Dynamics.Literal
-- import           Music.Pitch.Literal
-- import           Music.Time.Internal.Util (through, tripped)
-- import           Music.Time.Reverse
-- import           Music.Time.Split


-- |
-- A 'Note' is a value with an 'onset' and and 'offset' in time. It is an instance
-- of 'Transformable'.
--
-- You can use 'value' to apply a function in the context of the transformation,
-- i.e.
--
-- @
-- over value (* line) (delay 2 $ return line)
-- @
--
-- @
-- ('view' 'value') . 'transform' s = 'transform' s . ('view' 'value')
-- @
--
newtype Note a = Note { _noteValue :: (Span, a) }
  deriving (Eq,
            Functor,
            Foldable,
            Traversable,
            Comonad,
            Typeable)

instance (Show a, Transformable a) => Show (Note a) where
  show x = show (x^.from note) ++ "^.note"

-- |
-- Note is a 'Monad' and 'Applicative' in the style of pair, with 'return' placing a value
-- at the default span 'mempty' and 'join' composing time transformations.
deriving instance Monad Note
deriving instance Applicative Note

instance Wrapped (Note a) where
  type Unwrapped (Note a) = (Span, a)
  _Wrapped' = iso _noteValue Note

instance Rewrapped (Note a) (Note b)

instance Transformable (Note a) where
  transform t = over (_Wrapped . _1) $ transform t

instance HasDuration (Note a) where
  _duration = _duration . fst . view _Wrapped

instance HasPosition (Note a) where
  x `_position` p = fst (view _Wrapped x) `_position` p

instance Splittable a => Splittable (Note a) where
  -- beginning d = over _Wrapped $ \(s, v) -> (beginning d s, beginning (transform (negateV s) d) v)
  beginning d = over _Wrapped $ \(s, v) -> (beginning d s, beginning (d / _duration s) v)
  ending    d = over _Wrapped $ \(s, v) -> (ending    d s, ending    (d / _duration s) v)

instance Reversible (Note a) where
  rev = revDefault

-- Lifted instances

instance IsString a => IsString (Note a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Note a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Note a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Note a) where
  fromDynamics = pure . fromDynamics

-- |
-- View a note as a pair of the original value and the transformation (and vice versa).
--
note :: ({-Transformable a, Transformable b-}) => Iso (Span, a) (Span, b) (Note a) (Note b)
note = _Unwrapped

-- |
-- View the value in the note.
--
noteValue :: (Transformable a, Transformable b) => Lens (Note a) (Note b) a b
noteValue = lens runNote (flip $ mapNote . const)
  where
    runNote = uncurry transform . view _Wrapped
    -- setNote f (view (from note) -> (s,x)) = view note (s, itransform s x)
    mapNote f (view (from note) -> (s,x)) = view note (s, f `whilst` negateV s $ x)
    f `whilst` t = over (transformed t) f
{-# INLINE noteValue #-}

-- |
-- View a note as an events, i.e. a time-duration-value triplet.
--
event :: Iso (Note a) (Note b) (Time, Duration, a) (Time, Duration, b)
event = from note . bimapping delta id . tripped





-- import Control.Lens -- DEBUG
-- import           Control.Applicative
-- import           Control.Comonad
-- import           Data.Functor.Couple
-- import           Data.Ord            (comparing)
-- import           Data.List           (takeWhile, sort, sortBy, group)
-- import           Data.List.Ordered
-- import           Data.Maybe
-- import           Data.Semigroup
-- import           Control.Monad
--
-- import           Music.Time.Behavior
-- import           Music.Time.Reverse
-- import           Music.Time.Segment
-- import           Music.Time.Split
--
-- |
-- 'Past' represents a value occuring /before and at/ some point in time.
--
-- It may be seen as a note whose era is a left-open, right-inclusive time interval.
--
newtype Past a = Past { getPast :: (Min Time, a) }
  deriving (Eq, Ord, Functor)

-- |
-- 'Future' represents a value occuring /at and after/ some point in time.
--
-- It may be seen as a note whose era is a left-open, right-inclusive time interval.
--
newtype Future a = Future { getFuture :: (Max Time, a) }
  deriving (Eq, Ord, Functor)

instance HasDuration (Past a) where
  _duration _ = 0

instance HasDuration (Future a) where
  _duration _ = 0

instance HasPosition (Past a) where
  _position (Past (extract -> t,_)) _ = t

instance HasPosition (Future a) where
  _position (Future (extract -> t,_)) _ = t

-- | Query a past value. Semantic function.
past :: Past a -> Time -> Maybe a
past (Past (extract -> t, x)) t'
  | t' <= t    = Just x
  | otherwise  = Nothing

-- | Query a future value. Semantic function.
future :: Future a -> Time -> Maybe a
future (Future (extract -> t, x)) t'
  | t' >= t    = Just x
  | otherwise  = Nothing

-- TODO more elegant
indexPast :: [Past a] -> Time -> Maybe a
indexPast ps t = firstTrue $ fmap (\p -> past p t) $ Data.List.sortBy (comparing _onset) ps

firstTrue :: [Maybe a] -> Maybe a
firstTrue = listToMaybe . join . fmap maybeToList
-- firstTrue = join . listToMaybe . dropWhile isNothing

-- | Project a segment (backwards) up to the given point.
pastSeg :: Past (Segment a) -> Behavior (Maybe a)
pastSeg = undefined

-- | Project a segment starting from the given point.
futureSeg :: Future (Segment a) -> Behavior (Maybe a)
futureSeg = undefined





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
-- This misleadingly named  provide a way to query a value for its
--  'duration', 'onset' and 'offset'.
--
-------------------------------------------------------------------------------------



-- import           Data.AffineSpace
-- import           Data.AffineSpace.Point
-- import           Data.Map               (Map)
-- import qualified Data.Map               as Map
-- import           Data.Semigroup
-- import           Data.Set               (Set)
-- import qualified Data.Set               as Set
-- import           Data.VectorSpace       hiding (Sum)
--
-- import           Music.Time.Duration
-- import           Music.Time.Internal.Util
--
-- import           Control.Lens           hiding (Indexable, Level, above, below,
--                                          index, inside, parts, reversed,
--                                          transform, (<|), (|>))

-- |
-- Class of values that have a position in time.
--
-- Many values such as notes, envelopes etc can in fact have many positions such as onset,
-- attack point, offset, decay point time etc. Rather than having separate methods for a
-- discrete set of cases, this class provides an interpolation from a /local/ position to
-- a /global/ position. While the local position goes from 0 to 1, the global position
-- goes from the 'onset' to the 'offset' of the value.
--
-- For instantaneous values, a suitable instance is:
--
-- @
-- '_position' x = 'const' t
-- @
--
-- For values with an onset and offset we can use 'alerp':
--
-- @
-- '_position' x = 'alerp' ('_onset' x) ('_offset' x)
-- @
--
class HasDuration a => HasPosition a where
  -- |
  -- Return the onset of the given value, or the value between the attack and decay phases.
  --
  _position :: a -> Duration -> Time
  _position x = alerp (_onset x) (_offset x)

  -- |
  -- Return the onset of the given value, or the value between the attack and decay phases.
  --
  _onset, _offset :: a -> Time
  _onset     = (`_position` 0)
  _offset    = (`_position` 1.0)

instance HasPosition Time where
  _position = const

instance HasPosition Span where
  -- Override as an optimization:
  _onset    (view range -> (t1, t2)) = t1
  _offset   (view range -> (t1, t2)) = t2
  _position (view range -> (t1, t2)) = alerp t1 t2

instance (HasPosition a, HasDuration a) => HasDuration [a] where
  _duration x = _offset x .-. _onset x

instance (HasPosition a, HasDuration a) => HasPosition [a] where
  _onset  = foldr min 0 . fmap _onset
  _offset = foldr max 0 . fmap _offset

_era :: HasPosition a => a -> Span
_era x = _onset x <-> _offset x
{-# INLINE _era #-}

-- |
-- Position of the given value.
--
position :: (HasPosition a, Transformable a) => Duration -> Lens' a Time
position d = lens (`_position` d) (flip $ placeAt d)
{-# INLINE position #-}

-- |
-- Onset of the given value.
--
onset :: (HasPosition a, Transformable a) => Lens' a Time
onset = position 0
{-# INLINE onset #-}

-- |
-- Onset of the given value.
--
offset :: (HasPosition a, Transformable a) => Lens' a Time
offset = position 1
{-# INLINE offset #-}

-- |
-- Pre-onset of the given value, or the value right before the attack phase.
--
preOnset :: (HasPosition a, Transformable a) => Lens' a Time
preOnset = position (-0.5)
{-# INLINE preOnset #-}

-- |
-- Midpoint of the given value, or the value between the decay and sustain phases.
--
midpoint :: (HasPosition a, Transformable a) => Lens' a Time
midpoint = position 0.5
{-# INLINE midpoint #-}

postOnset :: (HasPosition a, Transformable a) => Lens' a Time
postOnset = position 0.5
{-# DEPRECATED postOnset "Use midpoint" #-}

-- |
-- Post-offset of the given value, or the value right after the release phase.
--
postOffset :: (HasPosition a, Transformable a) => Lens' a Time
postOffset = position 1.5
{-# INLINE postOffset #-}



-- |
-- Move a value forward in time.
--
startAt :: (Transformable a, HasPosition a) => Time -> a -> a
startAt t x = (t .-. _onset x) `delay` x

-- |
-- Move a value forward in time.
--
stopAt  :: (Transformable a, HasPosition a) => Time -> a -> a
stopAt t x = (t .-. _offset x) `delay` x

-- |
-- Align a value to a given position.
--
-- @placeAt p t@ places the given thing so that its position p is at time t
--
-- @
-- 'placeAt' 0 = 'startAt'
-- 'placeAt' 1 = 'stopAt'
-- @
--
placeAt :: (Transformable a, HasPosition a) => Duration -> Time -> a -> a
placeAt p t x = (t .-. x `_position` p) `delay` x

-- |
-- Place a value over the given span.
--
-- @placeAt s t@ places the given thing so that @x^.place = s@
--
_setEra :: (HasPosition a, Transformable a) => Span -> a -> a
_setEra s x = transform (s ^-^ view era x) x

-- |
-- A lens to the position
--
era :: (HasPosition a, Transformable a) => Lens' a Span
era = lens _era (flip _setEra)
{-# INLINE era #-}

stretchRelative :: (HasPosition a, Transformable a) => Duration -> Duration -> a -> a
stretchRelative p n x = over (transformed $ undelaying (realToFrac $ x^.position p)) (stretch n) x

stretchRelativeOnset :: (HasPosition a, Transformable a) => Duration -> a -> a
stretchRelativeOnset = stretchRelative 0

stretchRelativeMidpoint :: (HasPosition a, Transformable a) => Duration -> a -> a
stretchRelativeMidpoint = stretchRelative 0.5

stretchRelativeOffset :: (HasPosition a, Transformable a) => Duration -> a -> a
stretchRelativeOffset = stretchRelative 1




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
-------------------------------------------------------------------------------------



-- Reactive values, or piecewise functions of time.
--
-- Similar to Conal's definition in <http://conal.net/blog/posts/reactive-normal-form>,
-- but defined in negative time as well. Its semantics function is either 'occs' @&&&@ '?'
-- /or/ 'initial' @&&&@ 'updates', where 'intial' is the value from negative infinity
-- to the first update.
--
-- TODO integrate better in the library
--

-- import           Music.Time.Behavior
-- import           Music.Time.Bound
-- import           Music.Time.Note
-- import           Music.Time.Reverse
-- import           Music.Time.Segment
-- import           Music.Time.Split
--
-- import           Music.Pitch.Alterable
-- import           Music.Pitch.Augmentable
-- import           Music.Pitch.Literal
--
--
-- import           Control.Applicative
-- import           Control.Lens            hiding (Indexable, Level, above, below,
--                                           index, inside, parts, reversed,
--                                           transform, (<|), (|>))
-- import           Control.Monad
-- import           Control.Monad.Plus
-- import           Data.Distributive
-- import           Data.Functor.Rep
-- import           Data.Functor.Rep.Lens
-- import qualified Data.List               as List
-- import           Data.Semigroup          hiding ()
-- import           Data.Typeable
-- import           Music.Dynamics.Literal
-- import           Music.Pitch.Literal
--
-- |
-- Forms an applicative as per 'Behavior', but only switches at discrete points.
--
newtype Reactive a = Reactive { getReactive :: ([Time], Behavior a) }
    deriving (Functor, Semigroup, Monoid, Typeable)

-- $semantics
--
-- type Reactive a = (a, Time, Voice a)
--

--
-- TODO Define a more compact representation and reimplement Behavior as (Reactive Segment).
--
-- Possible approach:
--
--  * Implement PosReactive (no negative values) and define Reactive = Delayed (PosReactive)
--
--  * Implement liftA2 for PosReactive (preferably with a single traversal)
--

instance Transformable (Reactive a) where
    transform s (Reactive (t,r)) = Reactive (transform s t, transform s r)

instance Reversible (Reactive a) where
  rev = stretch (-1)

instance Wrapped (Reactive a) where
    type Unwrapped (Reactive a) = ([Time], Behavior a)
    _Wrapped' = iso getReactive Reactive

instance Rewrapped (Reactive a) (Reactive b)
instance Applicative Reactive where
    pure  = pureDefault
      where
        pureDefault = view _Unwrapped . pure . pure

    (<*>) = apDefault
      where
        (view _Wrapped -> (tf, rf)) `apDefault` (view _Wrapped -> (tx, rx)) = view _Unwrapped (tf <> tx, rf <*> rx)

instance IsPitch a => IsPitch (Reactive a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Reactive a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Reactive a) where
  fromDynamics = pure . fromDynamics

instance Alterable a => Alterable (Reactive a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Augmentable a => Augmentable (Reactive a) where
    augment = fmap augment
    diminish = fmap diminish



-- |
-- Get the initial value.
--
initial :: Reactive a -> a
initial r = r `atTime` minB (occs r)
    where
        -- If there are no updates, just use value at time 0
        -- Otherwise pick an arbitrary time /before/ the first value
        -- It looks strange but it works
        minB []    = 0
        minB (x:_) = x - 1

-- | Get the time of all updates and the value switched to at this point.
updates :: Reactive a -> [(Time, a)]
updates r = (\t -> (t, r `atTime` t)) <$> (Data.List.sort . Data.List.nub) (occs r)

renderR :: Reactive a -> (a, [(Time, a)])
renderR x = (initial x, updates x)

occs :: Reactive a -> [Time]
occs = fst . (^. _Wrapped')

-- | Split a reactive into notes, as well as the values before and after the first/last update
splitReactive :: Reactive a -> Either a ((a, Time), [Note a], (Time, a))
splitReactive r = case updates r of
    []          -> Left  (initial r)
    (t,x):[]    -> Right ((initial r, t), [], (t, x))
    (t,x):xs    -> Right ((initial r, t), fmap mkNote $ mrights (res $ (t,x):xs), head $ mlefts (res $ (t,x):xs))

    where

        mkNote (t,u,x) = (t <-> u, x)^.note

        -- Always returns a 0 or more Right followed by one left
        res :: [(Time, a)] -> [Either (Time, a) (Time, Time, a)]
        res rs = let (ts,xs) = unzip rs in
            flip fmap (withNext ts `zip` xs) $
                \ ((t, mu), x) -> case mu of
                    Nothing -> Left (t, x)
                    Just u  -> Right (t, u, x)

        -- lenght xs == length (withNext xs)
        withNext :: [a] -> [(a, Maybe a)]
        withNext = go
            where
                go []       = []
                go [x]      = [(x, Nothing)]
                go (x:y:rs) = (x, Just y) : withNext (y : rs)

{-# DEPRECATED updates "" #-}
{-# DEPRECATED occs "" #-}
{-# DEPRECATED splitReactive "" #-}
{-# DEPRECATED atTime "" #-}

atTime :: Reactive a -> Time -> a
atTime = (!) . snd . (^. _Wrapped')

-- |
-- Get the final value.
--
final :: Reactive a -> a
final (renderR -> (i,[])) = i
final (renderR -> (i,xs)) = snd $ last xs

-- | @switch t a b@ behaves as @a@ before time @t@, then as @b@.
switchR :: Time -> Reactive a -> Reactive a -> Reactive a
switchR t (Reactive (tx, bx)) (Reactive (ty, by)) = Reactive $ (,)
    (filter (< t) tx <> [t] <> filter (> t) ty) (switch t bx by)

trimR :: Monoid a => Span -> Reactive a -> Reactive a
trimR (view range -> (t, u)) x = switchR t mempty (switchR u x mempty)

-- |
-- Get all intermediate values.
--
intermediate :: Transformable a => Reactive a -> [Note a]
intermediate (updates -> []) = []
intermediate (updates -> xs) = fmap (\((t1, x), (t2, _)) -> (t1 <-> t2, x)^.note) $ withNext $ xs
  where
    withNext xs = zip xs (tail xs)

-- |
-- Realize a 'Reactive' value as a discretely changing behavior.
--
discrete :: Reactive a -> Behavior a
discrete = continous . fmap pure

-- |
-- Realize a 'Reactive' value as an continous behavior.
--
-- See also 'concatSegment' and 'concatB'.
--
continous :: Reactive (Segment a) -> Behavior a

-- |
-- Realize a 'Reactive' value as an continous behavior.
--
-- See also 'concatSegment' and 'concatB'.
--
continousWith :: Segment (a -> b) -> Reactive a -> Behavior b
continousWith f x = continous $ liftA2 (<*>) (pure f) (fmap pure x)

-- |
-- Sample a 'Behavior' into a reactive.
--
sample   :: [Time] -> Behavior a -> Reactive a

-- TODO linear approximation
(continous, sample) = error "Not implemented: (continous, sample)"


window :: [Time] -> Behavior a -> Reactive (Segment a)
windowed :: Iso (Behavior a) (Behavior b) (Reactive (Segment a)) (Reactive (Segment b))
(window, windowed) = error "Not implemented: (window, windowed)"

{-

-- Fre monad of ?
{-
data Score s a
  = SOne a
  | SPlus s [Score a]
-}
newtype Trans s a = Trans (s, [a]) deriving (Functor)
instance Monoid s => Monad (Trans s) where
  return = Trans . return . return
  -- TODO the usual >>=

type Score s a = Free (Trans s) a

viewScore :: Monoid s => Score s a -> [(s, a)]
viewScore x = case retract x of
  Trans (s,as) -> zip (repeat s) as


-- Free monad of (a,a)
{-
data Tree a
  = One a
  | Plus (Tree a) (Tree a)
-}
data Pair a = Pair a a deriving (Functor)
newtype MaybePair a = MaybePair (Maybe (Pair a)) deriving (Functor) -- Use compose
type Tree a = Free MaybePair a

-- CPS-version of Tree
newtype Search a = Search { getSearch :: forall r . (a -> Tree r) -> Tree r }
   -}




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
-- Many time structures such as 'Score' allows for rests between notes. Generally rests
-- are simply treated as blank space, and thus have no duration. Sometimes it is useful
-- to represent rests explicitly, so this  provides an alias for 'pure' 'Nothing' that
-- can be used to that end.
--
-- To remove rests from a score, use 'mcatMaybes', for example:
--
-- > open $ mcatMaybes $ scat [c,d,rest^*2,e]^/8
--
-------------------------------------------------------------------------------------


-- import           Control.Applicative
-- import           Music.Time.Reverse
-- import           Music.Time.Split

rest :: Applicative f => f (Maybe a)
rest = pure Nothing

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
-- Provides reversible values.
--
-------------------------------------------------------------------------------------



-- import           Data.AffineSpace
-- import           Data.AffineSpace.Point
-- import           Data.Map               (Map)
-- import qualified Data.Map               as Map
-- import           Data.Ratio
-- import           Data.Semigroup
-- import           Data.Set               (Set)
-- import qualified Data.Set               as Set
-- import           Data.VectorSpace
--
-- import           Music.Time.Position
--
-- import           Control.Lens           hiding (Indexable, Level, above, below,
--                                          index, inside, parts, reversed,
--                                          transform, (<|), (|>))
-- import           Data.AffineSpace
-- import           Data.AffineSpace.Point
-- import           Data.Map               (Map)
-- import qualified Data.Map               as Map
-- import           Data.Semigroup         hiding ()
-- import           Data.Sequence          (Seq)
-- import qualified Data.Sequence          as Seq
-- import           Data.Typeable
-- import           Data.VectorSpace       hiding (Sum (..))

-- |
-- Class of values that can be reversed (retrograded).
--
-- For positioned values succh as 'Note', the value is reversed relative to its middle point, i.e.
-- the onset value becomes the offset value and vice versa.
--
-- For non-positioned values such as 'Stretched', the value is reversed in-place.
--
-- FIXME Second law is incompatible with 'revDefault' (and the 'Span' definition below)
--
-- Law
--
-- @
-- 'rev' ('rev' a) = a
-- @
--
-- @
-- 'abs' ('_duration' x) = _duration ('rev' x)
-- @
--
-- @
-- 'rev' s `transform` a = 'rev' (s `transform` a)
-- @
--
-- or equivalently,
--
-- @
-- 'transform' . 'rev' = 'fmap' 'rev' . 'transform'
-- @
--
-- For 'Span'
--
-- @
-- 'rev' = 'over' 'range' 'swap'
-- @
--
class Transformable a => Reversible a where

  -- | Reverse (retrograde) the given value.
  rev :: a -> a

--
-- XXX Counter-intuitive Behavior instances (just Behavior should reverse around origin,
-- while Bound (Behavior a) should reverse around the middle, like a note)
--

--
-- XXX Alternate formulation of second Reversiblee law
--
--     rev s `transform` a     = rev (s `transform` a)
-- ==> (rev s `transform`)     = rev . (s `transform`)
-- ==> transform (rev s)       = rev . (transform s)
-- ==> (transform . rev) s     = (rev .) (transform s)
-- ==> (transform . rev) s     = fmap rev (transform s)
-- ==> transform . rev         = fmap rev . transform
--

instance Reversible () where
  rev = id

instance Reversible Int where
  rev = id

instance Reversible Double where
  rev = id

instance Reversible Integer where
  rev = id

instance Reversible a => Reversible [a] where
  rev = reverse . map rev

-- instance Reversible a => Reversible (Seq a) where
  -- rev = Seq.reverse . fmap rev

instance (Ord k, Reversible a) => Reversible (Map k a) where
  rev = Map.map rev

instance Reversible Duration where
  rev = stretch (-1)

--
-- There is no instance for Reversible Time
-- as we can not satisfy the second Reversible law
--

instance Reversible Span where
  rev = revDefault

instance Reversible a => Reversible (b, a) where
  rev (s,a) = (s, rev a)

-- |
-- A default implementation of 'rev'
--
revDefault :: (HasPosition a, Transformable a) => a -> a
revDefault x = stretch (-1) x

-- Alternative:
-- revDefault x = (stretch (-1) `whilst` undelaying (_position x 0.5 .-. 0)) x
--   where f `whilst` t = over (transformed t) f

newtype NoReverse a = NoReverse { getNoReverse :: a }
  deriving (Typeable, Eq, Ord, Show)

instance Transformable (NoReverse a) where
  transform _ = id

instance Reversible (NoReverse a) where
  rev = id

-- |
-- View the reverse of a value.
--
-- >>> [1,2,3] & reversed %~ Data.List.sort
-- [3,2,1]
--
reversed :: Reversible a => Iso' a a
reversed = iso rev rev


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
-------------------------------------------------------------------------------------



-- import           Data.AffineSpace
-- import           Data.AffineSpace.Point
-- import qualified Data.List.NonEmpty     as NonEmpty
-- import           Data.Map               (Map)
-- import qualified Data.Map               as Map
-- import           Data.Ratio
-- import           Data.Semigroup
-- import           Data.Set               (Set)
-- import qualified Data.Set               as Set
-- import           Data.VectorSpace
-- import           Data.String
-- import           Data.Functor.Adjunction  (unzipR)
--
-- import           Music.Time.Juxtapose   (scat)
-- import           Music.Time.Meta
-- import           Music.Time.Note
-- import           Music.Time.Reverse
-- import           Music.Time.Split
-- import           Music.Time.Stretched
-- import           Music.Time.Voice
--
-- import           Control.Applicative
-- import           Control.Comonad
-- import           Control.Lens           hiding (Indexable, Level, above, below,
--                                          index, inside, parts, reversed,
--                                          transform, (<|), (|>))
-- import           Control.Monad
-- import           Control.Monad.Compose
-- import           Control.Monad.Plus
-- import           Data.Foldable          (Foldable)
-- import qualified Data.Foldable          as Foldable
-- import qualified Data.List              as List
-- import qualified Data.Ord               as Ord
-- import           Data.Semigroup         hiding ()
-- import           Data.Traversable       (Traversable)
-- import qualified Data.Traversable       as T
-- import           Data.Typeable
-- import           Data.VectorSpace       hiding (Sum (..))
-- import           Music.Dynamics.Literal
-- import           Music.Pitch.Literal
-- import           Music.Time.Internal.Util



type ScoreNote a = Note a

--   * 'empty' creates an empty score
--
--   * 'pure' creates a score containing a single note in the span @0 '<->' 1@
--
--   * '<|>' composes scores in parallel
--
--   * '|>' composes scores as a forward sequence
--
--   * '<|' composes scores as a backward sequence
--
-- You can also use '<>' and 'mempty' of course.
--

-- |
-- A 'Score' is a sequential or parallel composition of values, and allows overlapping events
--
-- You typically create a 'Score' using 'score', 'notes', 'voices', and 'phrases', or the 'Alternative' interface.
--
-- Score is an instance of 'Transformable', so you can use 'delay' and 'stretch'.
--
-- Score is an instance of 'HasPosition', so you can use 'duration', 'onset', 'offset', 'era'.
--
-- To inspect or deconstruct a score, see 'notes', 'voices', and 'phrases', as
-- well as 'singleNote', 'singleVoice', and 'singlePhrase'
--
newtype Score a = Score { getScore' :: (Meta, NScore a) }
    deriving (Functor, Semigroup, Monoid, Foldable, Traversable, Typeable{-, Show, Eq, Ord-})

--
-- $semantics Score
--
-- type Score a = [Note a]
--

instance Wrapped (Score a) where
  type Unwrapped (Score a) = (Meta, NScore a)
  _Wrapped' = iso getScore' Score

instance Rewrapped (Score a) (Score b) where

instance Applicative Score where
  pure = return
  (<*>) = ap

instance Monad Score where
  return = (^. _Unwrapped') . return . return
  xs >>= f = (^. _Unwrapped') $ mbind ((^. _Wrapped') . f) ((^. _Wrapped') xs)

instance Alternative Score where
  empty = mempty
  (<|>) = mappend

instance MonadPlus Score where
  mzero = mempty
  mplus = mappend

{-
instance FunctorWithIndex Span Score where
  imap f = over (_Wrapped._2) $ imap f

instance FoldableWithIndex Span Score where
  ifoldMap f (Score (m,x)) = ifoldMap f x

instance TraversableWithIndex Span Score where
  itraverse f (Score (m,x)) = fmap (\x -> Score (m,x)) $ itraverse f x
-}

instance Transformable (Score a) where
  transform t (Score (m,x)) = Score (transform t m, transform t x)

instance Reversible a => Reversible (Score a) where
  rev (Score (m,x)) = Score (rev m, rev x)

instance Splittable a => Splittable (Score a) where
  split t (Score (m,x)) = (Score (m1,x1), Score (m2,x2))
    where
      (m1, m2) = split t m
      (x1, x2) = split t x

-- TODO move these two "implementations" to NScore
instance HasPosition (Score a) where
  _position = _position . snd . view _Wrapped' {-. normalizeScore'-}
  -- TODO clean up in terms of AddMeta and optimize

instance HasDuration (Score a) where
  _duration x = _offset x .-. _onset x



-- Lifted instances

instance IsString a => IsString (Score a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Score a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Score a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Score a) where
  fromDynamics = pure . fromDynamics

-- Bogus instance, so we can use [c..g] expressions
instance Enum a => Enum (Score a) where
  toEnum = return . toEnum
  fromEnum = list 0 (fromEnum . head) . Foldable.toList

-- Bogus instance, so we can use numeric literals
instance Num a => Num (Score a) where
  fromInteger = return . fromInteger
  abs    = fmap abs
  signum = fmap signum
  (+)    = error "Not implemented"
  (-)    = error "Not implemented"
  (*)    = error "Not implemented"

-- Bogus instances, so we can use c^*2 etc.
instance AdditiveGroup (Score a) where
  zeroV   = error "Not implemented"
  (^+^)   = error "Not implemented"
  negateV = error "Not implemented"

instance VectorSpace (Score a) where
  type Scalar (Score a) = Duration
  d *^ s = d `stretch` s

instance HasMeta (Score a) where
  meta = _Wrapped . _1







newtype NScore a = NScore { getNScore :: [ScoreNote a] }
  deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

instance (Show a, Transformable a) => Show (Score a) where
  show x = show (x^.notes) ++ "^.score"

instance Wrapped (NScore a) where
  type Unwrapped (NScore a) = [ScoreNote a]
  _Wrapped' = iso getNScore NScore

instance Rewrapped (NScore a) (NScore b)

instance Applicative NScore where
  pure  = return
  (<*>) = ap

instance Monad NScore where
  return = (^. _Unwrapped) . pure . pure
  xs >>= f = (^. _Unwrapped) $ mbind ((^. _Wrapped') . f) ((^. _Wrapped') xs)

instance Alternative NScore where
  empty = mempty
  (<|>) = mappend

instance MonadPlus NScore where
  mzero = mempty
  mplus = mappend

instance Transformable (NScore a) where
  transform t (NScore xs) = NScore (fmap (transform t) xs)

instance Reversible a => Reversible (NScore a) where
  rev (NScore xs) = NScore (fmap rev xs)

instance HasPosition (NScore a) where
  _onset  = safeMinimum . fmap (_onset  . normalizeSpan) . toListOf (_Wrapped . each . era)
  _offset = safeMaximum . fmap (_offset . normalizeSpan) . toListOf (_Wrapped . each . era)

-- TODO move
safeMinimum xs = if null xs then 0 else minimum xs
safeMaximum xs = if null xs then 0 else maximum xs

instance HasDuration (NScore a) where
  _duration x = _offset x .-. _onset x

instance Splittable a => Splittable (NScore a) where
  split t (NScore notes) = over both (NScore . mfilter (not . isEmptyNote)) $ unzip $ map (\x -> splitAbs (0 .+^ t) x) notes
    where
      -- TODO move
      isEmptyNote :: Note a -> Bool
      isEmptyNote = isEmptySpan . view era

      isEmptySpan :: Span -> Bool
      isEmptySpan (view range -> (t, u)) = t == u


-- |
-- Create a score from a list of notes.
--
-- This is a getter (rather than a function) for consistency:
--
-- @
-- [ (0 '<->' 1, 10)^.'note',
--   (1 '<->' 2, 20)^.'note',
--   (3 '<->' 4, 30)^.'note' ]^.'score'
-- @
--
-- @
-- 'view' 'score' $ 'map' ('view' 'note') [(0 '<->' 1, 1)]
-- @
--
-- Se also 'notes'.
--
score :: Getter [Note a] (Score a)
score = from unsafeNotes
{-# INLINE score #-}

-- |
-- View a 'Score' as a list of 'Note' values.
--
-- @
-- 'view' 'notes'                        :: 'Score' a -> ['Note' a]
-- 'set'  'notes'                        :: ['Note' a] -> 'Score' a -> 'Score' a
-- 'over' 'notes'                        :: (['Note' a] -> ['Note' b]) -> 'Score' a -> 'Score' b
-- @
--
-- @
-- 'preview'  ('notes' . 'each')           :: 'Score' a -> 'Maybe' ('Note' a)
-- 'preview'  ('notes' . 'element' 1)      :: 'Score' a -> 'Maybe' ('Note' a)
-- 'preview'  ('notes' . 'elements' odd)   :: 'Score' a -> 'Maybe' ('Note' a)
-- @
--
-- @
-- 'set'      ('notes' . 'each')           :: 'Note' a -> 'Score' a -> 'Score' a
-- 'set'      ('notes' . 'element' 1)      :: 'Note' a -> 'Score' a -> 'Score' a
-- 'set'      ('notes' . 'elements' odd)   :: 'Note' a -> 'Score' a -> 'Score' a
-- @
--
-- @
-- 'over'     ('notes' . 'each')           :: ('Note' a -> 'Note' b) -> 'Score' a -> 'Score' b
-- 'over'     ('notes' . 'element' 1)      :: ('Note' a -> 'Note' a) -> 'Score' a -> 'Score' a
-- 'over'     ('notes' . 'elements' odd)   :: ('Note' a -> 'Note' a) -> 'Score' a -> 'Score' a
-- @
--
-- @
-- 'toListOf' ('notes' . 'each')                :: 'Score' a -> ['Note' a]
-- 'toListOf' ('notes' . 'elements' odd)        :: 'Score' a -> ['Note' a]
-- 'toListOf' ('notes' . 'each' . 'filtered'
--              (\\x -> '_duration' x \< 2))  :: 'Score' a -> ['Note' a]
-- @
--
-- This is not an 'Iso', as the note list representation does not contain meta-data.
-- To construct a score from a note list, use 'score' or @'flip' ('set' 'notes') 'empty'@.
--
notes :: Lens (Score a) (Score b) [Note a] [Note b]
notes = _Wrapped . _2 . _Wrapped . sorted
  where
    -- TODO should not have to sort...
    sorted = iso (Data.List.sortBy (Data.Ord.comparing _onset)) (Data.List.sortBy (Data.Ord.comparing _onset))
-- notes = unsafeNotes
{-# INLINE notes #-}

-- -- |
-- -- View a score as a list of voices.
-- --
-- -- @
-- -- 'view' 'voices'                        :: 'Score' a -> ['Voice' a]
-- -- 'set'  'voices'                        :: ['Voice' a] -> 'Score' a -> 'Score' a
-- -- 'over' 'voices'                        :: (['Voice' a] -> ['Voice' b]) -> 'Score' a -> 'Score' b
-- -- @
-- --
-- -- @
-- -- 'preview'  ('voices' . 'each')           :: 'Score' a -> 'Maybe' ('Voice' a)
-- -- 'preview'  ('voices' . 'element' 1)      :: 'Score' a -> 'Maybe' ('Voice' a)
-- -- 'preview'  ('voices' . 'elements' odd)   :: 'Score' a -> 'Maybe' ('Voice' a)
-- -- @
-- --
-- -- @
-- -- 'set'      ('voices' . 'each')           :: 'Voice' a -> 'Score' a -> 'Score' a
-- -- 'set'      ('voices' . 'element' 1)      :: 'Voice' a -> 'Score' a -> 'Score' a
-- -- 'set'      ('voices' . 'elements' odd)   :: 'Voice' a -> 'Score' a -> 'Score' a
-- -- @
-- --
-- -- @
-- -- 'over'     ('voices' . 'each')           :: ('Voice' a -> 'Voice' b) -> 'Score' a -> 'Score' b
-- -- 'over'     ('voices' . 'element' 1)      :: ('Voice' a -> 'Voice' a) -> 'Score' a -> 'Score' a
-- -- 'over'     ('voices' . 'elements' odd)   :: ('Voice' a -> 'Voice' a) -> 'Score' a -> 'Score' a
-- -- @
-- --
-- -- @
-- -- 'toListOf' ('voices' . 'each')           :: 'Score' a -> ['Voice' a]
-- -- 'toListOf' ('voices' . 'elements' odd)   :: 'Score' a -> ['Voice' a]
-- -- 'toListOf' ('voices' . 'each' . 'filtered' (\\x -> '_duration' x \< 2)) :: 'Score' a -> ['Voice' a]
-- -- @
-- --
-- -- This is not an 'Iso', as the voice list representation does not contain meta-data.
-- -- To construct a score from a voice list, use 'score' or @'flip' ('set' 'voices') 'empty'@.
-- --
-- voices :: Lens (Score a) (Score b) [Voice a] [Voice b]
-- voices = unsafeVoices
-- {-# INLINE voices #-}

-- |
-- View a score as a list of notes.
--
-- This only an isomorphism up to meta-data. See also the safe (but more restricted)
-- 'notes' and 'score'.
--
unsafeNotes :: Iso (Score a) (Score b) [Note a] [Note b]
unsafeNotes = _Wrapped . noMeta . _Wrapped . sorted
  where
    sorted = iso (Data.List.sortBy (Data.Ord.comparing _onset)) (Data.List.sortBy (Data.Ord.comparing _onset))
    noMeta = iso extract return
    -- noMeta = iso (\(_,x) -> x) (\x -> (mempty,x))

{-# INLINE unsafeNotes #-}

-- |
-- View a score as a list of events.
--
-- This only an isomorphism up to meta-data. See also the safe (but more restricted)
-- 'notes' and 'score'.
--
unsafeEvents :: Iso (Score a) (Score b) [(Time, Duration, a)] [(Time, Duration, b)]
unsafeEvents = iso _getScore _score
  where
    _score :: [(Time, Duration, a)] -> Score a
    _score = mconcat . fmap (uncurry3 event)
      where
        event t d x   = (delay (t .-. 0) . stretch d) (return x)

    _getScore :: {-Transformable a => -}Score a -> [(Time, Duration, a)]
    _getScore =
      fmap (\(view delta -> (t,d),x) -> (t,d,x)) .
      Data.List.sortBy (Data.Ord.comparing fst) .
      Foldable.toList .
      fmap (view $ from note) .
      reifyScore



-- |
-- View a score as a single note.
--
singleNote :: Prism' (Score a) (Note a)
singleNote = unsafeNotes . single
{-# INLINE singleNote #-}
{-# DEPRECATED singleNote "Use 'unsafeNotes . single'" #-}
-- TODO make prism fail if score contains meta-data
-- (or else second prism law is not satisfied)


-- | Map with the associated time span.
mapScore :: (Note a -> b) -> Score a -> Score b
mapScore f = over (_Wrapped._2) (mapNScore f)
  where
    mapNScore f = over (_Wrapped.traverse) (extend f)

reifyScore :: Score a -> Score (Note a)
reifyScore = over (_Wrapped . _2 . _Wrapped) $ fmap duplicate

-- |
-- View a score as a list of events, i.e. time-duration-value triplets.
--
-- This is a convenient combination of 'notes' and 'event'.
--
-- @
-- 'events' = 'notes' . 'through' 'event' 'event'
-- @
--
events :: {-Transformable a => -}Lens (Score a) (Score b) [(Time, Duration, a)] [(Time, Duration, b)]
events = notes . _zipList . through event event . from _zipList



-- | Map over the values in a score.
mapWithSpan :: (Span -> a -> b) -> Score a -> Score b
mapWithSpan f = mapScore (uncurry f . view (from note))

-- | Filter the values in a score.
filterWithSpan :: (Span -> a -> Bool) -> Score a -> Score a
filterWithSpan f = mapFilterWithSpan (partial2 f)

-- | Combination of 'mapEvents' and 'filterEvents'.
mapFilterWithSpan :: (Span -> a -> Maybe b) -> Score a -> Score b
mapFilterWithSpan f = mcatMaybes . mapWithSpan f

-- | Map over the values in a score.
mapEvents :: (Time -> Duration -> a -> b) -> Score a -> Score b
mapEvents f = mapWithSpan (uncurry f . view delta)

-- | Filter the values in a score.
filterEvents   :: (Time -> Duration -> a -> Bool) -> Score a -> Score a
filterEvents f = mapFilterEvents (partial3 f)

-- | Efficient combination of 'mapEvents' and 'filterEvents'.
mapFilterEvents :: (Time -> Duration -> a -> Maybe b) -> Score a -> Score b
mapFilterEvents f = mcatMaybes . mapEvents f

-- | Normalize a score, assuring its events spans are all forward (as by 'isForwardSpan'),
-- and that its onset is at least zero. Consequently, the onset and offset of each event
-- in the score is at least zero.
--
-- Many backends perform this operation implicitly.
--
normalizeScore :: Score a -> Score a
normalizeScore = reset . normalizeScoreDurations
  where
    reset x = set onset (view onset x `max` 0) x
    normalizeScoreDurations = over (notes . each . era) normalizeSpan

-- TODO version that reverses the values where appropriate
-- Use over (notes . each) normalizeNote or similar

-- |
-- Print the span of each event, as given by 'eras'.
--
printEras :: Score a -> IO ()
printEras = mapM_ print . toListOf eras

-- |
-- Print all eras of the given score.
--
-- >>> toListOf eras $ scat [c,d,e :: Score Integer]
-- [0 <-> 1,1 <-> 2,2 <-> 3]
--
eras :: Traversal' (Score a) Span
eras = notes . each . era

-- TODO rename and expose this
-- We have an (Iso (Score a) (TMap Span [a])), with [] as default value
chordEvents :: Transformable a => Span -> Score a -> [a]
chordEvents s = fmap extract . filter ((== s) . view era) . view notes

simultaneous' :: Transformable a => Score a -> Score [a]
simultaneous' sc = (^. from unsafeEvents) vs
  where
    -- es :: [Era]
    -- evs :: [[a]]
    -- vs :: [(Time, Duration, [a])]
    es  = Data.List.nub $ toListOf eras sc
    evs = fmap (`chordEvents` sc) es
    vs  = zipWith (\(view delta -> (t,d)) a -> (t,d,a)) es evs

-- overSimult :: Transformable a => (Score [a] -> Score [b]) -> Score a -> Score b
-- overSimult f = mscatter . f . simultaneous'

-- |
-- Merge all simultaneous events using their 'Semigroup' instance.
--
-- Two events /a/ and /b/ are considered simultaneous if and only if they have the same
-- era, that is if @`era` a == `era` b@
--
simultaneous :: (Transformable a, Semigroup a) => Score a -> Score a
simultaneous = fmap (sconcat . NonEmpty.fromList) . simultaneous'

simult :: Transformable a => Lens (Score a) (Score b) (Score [a]) (Score [b])
simult = iso simultaneous' mscatter
-- TODO identical to: lens simultaneous' (flip $ mapSimultaneous . const)
-- wrap in something to preserve meta


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
-------------------------------------------------------------------------------------



-- import           Data.AffineSpace
-- import           Data.AffineSpace.Point
-- import           Data.Map               (Map)
-- import qualified Data.Map               as Map
-- import           Data.Ratio
-- import           Data.Semigroup
-- import           Data.Set               (Set)
-- import qualified Data.Set               as Set
-- import           Data.VectorSpace
--
-- import           Music.Time.Behavior
-- import           Music.Time.Bound
-- import           Music.Time.Note
-- import           Music.Time.Reverse
-- import           Music.Time.Score
-- import           Music.Time.Split
-- import           Music.Time.Stretched
-- import           Music.Time.Voice
--
-- import           Control.Applicative
-- import           Control.Lens           hiding (Indexable, Level, above, below,
--                                          index, inside, parts, reversed,
--                                          transform, (<|), (|>))
-- import           Data.Distributive
-- import           Data.Functor.Rep as R
-- import           Data.Functor.Rep.Lens
-- import           Data.Maybe
-- import           Data.Typeable
-- import           Music.Dynamics.Literal
-- import           Music.Pitch.Literal


-- TODO Compare Diagram's Trail and Located (and see the conal blog post)

-- |
--
-- A 'Segment' is a value varying over some unknown time span.
-- Intuitively, a 'Segment' is to a 'Behavior' what a /ray/ is to a /line/.
--
-- To give a segment an explicit duration, use 'Stretched' 'Segment'.
--
-- To place a segment in a particular time span, use 'Note' 'Segment'.
--
newtype Segment a = Segment { getSegment :: Clipped Duration -> a }
  deriving (Functor, Applicative, Monad{-, Comonad-}, Typeable)

-- $semantics Segment
--
-- @
-- type Segment a = 'Duration' -> a
-- @
--

-- $musicTimeSegmentExamples
--
-- > foldr1 apSegments' $ map (view stretched) $ [(0.5,0::Segment Float), (1, timeS), (2,rev timeS), (3,-1)]
--
-- > openG $ draw $ (1, timeS :: Segment Float)^.stretched
--

instance Show (Segment a) where
  show _ = "<<Segment>>"

instance Distributive Segment where
  distribute = Segment . distribute . fmap getSegment

instance Representable Segment where
  type Rep Segment = Duration
  tabulate f = Segment (f . fromClipped)
  index    (Segment f) = f . unsafeToClipped

-- |
-- Segments are /invariant/ under transformation. To transform a timve varying value, use
-- 'fromSegment'.
--
instance Transformable (Segment a) where
  transform _ = id

instance Reversible (Segment a) where
  -- TODO in terms of Representable
  rev (Segment f) = Segment (f . unsafeToClipped . r . fromClipped)
    where
      r x = (x * (-1)) + 1

-- TODO
-- type instance Pitch                 (Segment a) = Segment (Pitch a)
-- type instance SetPitch (Segment g)  (Segment a) = Segment (SetPitch g a)
--
-- instance (HasPitch a a, HasPitch a b) => HasPitches (Segment a) (Segment b) where
--   pitches = through pitch pitch
-- instance (HasPitch a a, HasPitch a b) => HasPitch (Segment a) (Segment b) where
--   pitch = through pitch pitch
--
-- type instance Dynamic                 (Segment a) = Segment (Dynamic a)
-- type instance SetDynamic (Segment g) (Segment a) = Segment (SetDynamic g a)
--
-- instance (HasDynamic a a, HasDynamic a b) => HasDynamics (Segment a) (Segment b) where
--   dynamics = through dynamic dynamic
-- instance (HasDynamic a a, HasDynamic a b) => HasDynamic (Segment a) (Segment b) where
--   dynamic = through dynamic dynamic
--
--
-- type instance Articulation                 (Segment a) = Segment (Articulation a)
-- type instance SetArticulation (Segment g) (Segment a) = Segment (SetArticulation g a)
--
-- instance (HasArticulation a a, HasArticulation a b) => HasArticulations (Segment a) (Segment b) where
--   articulations = through articulation articulation
-- instance (HasArticulation a a, HasArticulation a b) => HasArticulation (Segment a) (Segment b) where
--   articulation = through articulation articulation
--
--
-- type instance Part                 (Segment a) = Segment (Part a)
-- type instance SetPart (Segment g) (Segment a) = Segment (SetPart g a)
--
-- instance (HasPart a a, HasPart a b) => HasParts (Segment a) (Segment b) where
--   parts = through part part
-- instance (HasPart a a, HasPart a b) => HasPart (Segment a) (Segment b) where
--   part = through part part

-- deriving instance Semigroup a => Semigroup (Segment a)
-- deriving instance Monoid a => Monoid (Segment a)
-- deriving instance Num a => Num (Segment a)
-- deriving instance Fractional a => Fractional (Segment a)
-- deriving instance Floating a => Floating (Segment a)
--
-- instance IsPitch a => IsPitch (Segment a) where
--   fromPitch = pure . fromPitch
--
-- instance IsInterval a => IsInterval (Segment a) where
--   fromInterval = pure . fromInterval
--
-- instance Alterable a => Alterable (Segment a) where
--     sharpen = fmap sharpen
--     flatten = fmap flatten
--
-- instance Augmentable a => Augmentable (Segment a) where
--     augment = fmap augment
--     diminish = fmap diminish
--
-- instance Eq a => Eq (Segment a) where
--   (==) = error "No fun"
--
-- instance Ord a => Ord (Segment a) where
--   (<) = error "No fun"
--   max = liftA2 max
--   min = liftA2 min

-- |
-- View a segment as a time function and vice versa.
--
segment :: Iso (Duration -> a) (Duration -> b) (Segment a) (Segment b)
segment = R.tabulated

apSegments' :: Stretched (Segment a) -> Stretched (Segment a) -> Stretched (Segment a)
apSegments' (view (from stretched) -> (d1,s1)) (view (from stretched) -> (d2,s2))
  = view stretched (d1+d2, slerp (d1/(d1+d2)) s1 s2)

-- |
-- Append a voice of segments to a single stretched segment.
--
apSegments :: Voice (Segment a) -> Stretched (Segment a)
apSegments = foldr1 apSegments' . toListOf (stretcheds . each)

-- t < i && 0 <= t <= 1   ==> 0 < (t/i) < 1
-- i     is the fraction of the slerped segment spent in a
-- (1-i) is the fraction of the slerped segment spent in b
slerp :: Duration -> Segment a -> Segment a -> Segment a
slerp i a b
  | i < 0 || i >= 1    = error "slerp: Bad value"
  | otherwise = tabulate $ \t -> if t < i then a ! (t/i) else b ! ((t-i)/(1-i))

slerp2 :: (a -> a -> a) -> Duration -> Segment a -> Segment a -> Segment a
slerp2 f i a b
  | i < 0 || i >= 1    = error "slerp: Bad value"
  | otherwise = tabulate $ \t -> case t `compare` i of
      LT -> a ! (t/i)
      EQ -> (a ! 1) `f` (b ! 1)
      GT -> b ! ((t-i)/(1-i))


-- |
-- View a 'Note' 'Segment' as a 'Bound' 'Behavior' and vice versa.
--
-- This can be used to safely turn a behavior into a segment and vice
-- versa. Often 'focusing' is more convenient to use.
--
bounded' :: Iso' (Note (Segment a)) (Bound (Behavior a))
bounded' = bounded

-- |
-- View a 'Note' 'Segment' as a 'Bound' 'Behavior' and vice versa.
--
-- This can be used to safely turn a behavior into a segment and vice
-- versa. Often 'focusing' is more convenient to use.
--
bounded :: Iso (Note (Segment a)) (Note (Segment b)) (Bound (Behavior a)) (Bound (Behavior b))
bounded = iso ns2bb bb2ns
  where
    bb2ns (Bound (s, x)) = view note (s, b2s $ transform (negateV s) $ x)
    ns2bb (view (from note) -> (s, x)) = Bound (s,       transform s           $ s2b $ x)
    s2b = under R.tabulated (. realToFrac)
    b2s = under R.tabulated (. realToFrac)

--
-- Note that the isomorhism only works because of 'Bound' being abstract.
-- A function @unBound :: Bound a -> a@ could break the isomorphism
-- as follows:
--
-- > (unBound . view (from bounded . bounded) . bounds 0 1) b ! 2
-- *** Exception: Outside 0-1
--

-- |
-- Extract a bounded behavior, replacing all values outside the bound with 'mempty'.
--
-- @
-- 'trim'   = 'splice' 'mempty'
-- 'trim' x = 'trimBefore' '_onset' x . 'trimAfter' '_offset' x
-- @
--
trim :: Monoid b => Bound (Behavior b) -> Behavior b
trim = trimG
  where
    trimG :: (Monoid b, Representable f, Rep f ~ Time) => Bound (f b) -> f b
    trimG (Bound (s, x)) = tabulate (trimOutside s) `apRep` x

trimOutside :: Monoid a => Span -> Time -> a -> a
trimOutside s t x = if t `inside` s then x else mempty

-- |
-- Inserts a bounded behavior on top of another behavior.
--
-- @
-- 'trim' = 'splice' 'mempty'
-- @
--
-- (Named after the analogous tape-editing technique.)
--
splice :: Behavior a -> Bound (Behavior a) -> Behavior a
splice constant insert = fmap fromLast $ fmap toLast constant <> trim (fmap (fmap toLast) insert)
  where
    toLast   = Option . Just . Last
    fromLast = getLast . fromJust . getOption
    -- fromJust is safe here, as toLast is used to create the Maybe wrapper


concatSegment :: Monoid a => Note (Segment a) -> Behavior a
concatSegment = trim . view bounded

-- |
-- Concatenate a score of (possibly overlapping) segments.
--
-- See also 'concatB' and 'continous'.
--
concatS :: Monoid a => Score (Segment a) -> Behavior a
concatS = mconcat . map concatSegment . view notes
-- Or: mconcat.fmap trim.toListOf (notes.each.bounded)

-- |
-- Concatenate a score of (possibly overlapping) segments.
--
-- See also 'concatSegment' and 'continous'.
--
concatB :: Monoid a => Score (Behavior a) -> Behavior a
concatB = concatS . fmap (view focusing)
-- Or (more generally): mconcat.toListOf (notes.each.noteValue)


-- |
-- View part of a 'Behavior' as a 'Segment'.
--
-- @
-- 'line' & 'focusing' `onSpan` (2 '<->' 3) '*~' 0
-- @
--
focusing :: Lens' (Behavior a) (Segment a)
focusing = lens get set
  where
    get = view (from bounded . noteValue) . {-pure-}bounding mempty
    set x = splice x . (view bounded) . pure



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
-------------------------------------------------------------------------------------

--
-- import           Music.Time.Position
-- import           Music.Time.Internal.Util
--
-- import           Control.Lens            hiding (Indexable, Level, above, below,
--                                           index, inside, parts, reversed,
--                                           transform, (<|), (|>))
-- import           Data.AffineSpace
-- import           Data.AffineSpace.Point
-- import           Data.Functor.Adjunction (unzipR)
-- import           Data.Functor.Rep
-- import           Data.Map                (Map)
-- import qualified Data.Map                as Map
-- import           Data.Semigroup          hiding ()
-- import           Data.Sequence           (Seq)
-- import qualified Data.Sequence           as Seq
-- import           Data.VectorSpace        hiding (Sum (..))

-- |
-- Class of values that can be split.
--
-- For non-positioned values such as 'Stretched', split cuts a value into pieces
-- of the given duration and the rest.
--
-- For positioned values succh as 'Note', split cuts a value relative to its onset.
-- To split at an absolute position, see 'splitAbs'.
--
--
-- Law
--
-- @
-- '_duration' ('beginning' t x) + '_duration' ('ending' t x) = '_duration' x
-- '_duration' ('beginning' t x) = t `min` '_duration' x                    iff t >= 0
-- '_duration' ('ending' t x)    = '_duration' x - (t `min` '_duration' x)    iff t >= 0
-- @
--
-- (Note that any of these three laws can be derived from the other two, so it is
-- sufficient to prove two!).
--
-- >>> (\x -> fmap (flip split x) [-2,-1,0,0.5,1,2]) $ (1::Duration)
-- [(0,1),(0,1),(0,1),((1/2),(1/2)),(1,0),(1,0)]
--
-- >>> (\x -> fmap (flip split x) [-2,-1,0,0.5,1,2]) $ (0<->1)
-- [(0 <-> 0,0 <-> 1),(0 <-> 0,0 <-> 1),(0 <-> 0,0 <-> 1),(0 <-> (1/2),(1/2) <-> 1),(0 <-> 1,1 <-> 1),(0 <-> 1,1 <-> 1)]
--
class Splittable a where
  split      :: Duration -> a -> (a, a)
  beginning  :: Duration -> a -> a
  ending     :: Duration -> a -> a
  split   d x = (beginning d x, ending d x)
  beginning d = fst . split d
  ending    d = snd . split d
-- TODO rename beginning/ending to fstSplit/sndSplit or similar

instance Splittable () where
  split _ x = (x, x)

instance Splittable Duration where
  -- Directly from the laws
  -- Guard against t < 0
  split t x = (t' `min` x, x ^-^ (t' `min` x))
    where t' = t `max` 0

instance Splittable Span where
  -- Splitting a span splits the duration
  split pos (view delta -> (t, d)) = (t >-> d1, (t .+^ d1) >-> d2)
    where (d1, d2) = split pos d

instance (Ord k, Splittable a) => Splittable (Map k a) where
  split d = unzipR . Map.map (split d)


-- takeMWhile :: (Monoid a, HasDuration a, Splittable a) => Duration -> (a -> Bool) -> a -> a
-- takeMWhile d p xs = if _duration xs <= 0 then mempty else takeMWhile' d p xs
--   where
--     takeMWhile' d p (split d -> (x, xs)) = if p x then x `mappend` takeMWhile d p xs else mempty

chunks :: (Splittable a, HasDuration a) => Duration -> a -> [a]
chunks d xs = if _duration xs <= 0 then [] else chunks' d xs
  where
    chunks' d (split d -> (x, xs)) = [x] ++ chunks d xs


splitAbs :: (HasPosition a, Splittable a) => Time -> a -> (a, a)
splitAbs t x = split (t .-. _onset x) x



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
-------------------------------------------------------------------------------------



-- import           Control.Applicative
-- import           Control.Lens           hiding (Indexable, Level, above, below,
--                                          index, inside, parts, reversed,
--                                          transform, (<|), (|>))
-- import           Data.Bifunctor
-- import           Data.Foldable          (Foldable)
-- import qualified Data.Foldable          as Foldable
-- import           Data.Functor.Couple
-- import           Data.String
-- import           Data.Typeable
-- import           Data.VectorSpace
--
-- import           Music.Dynamics.Literal
-- import           Music.Pitch.Literal
-- import           Music.Time.Reverse
-- import           Music.Time.Split


-- |
-- A value 'Stretched' value, representing a suspended stretch of some 'Transformable'
-- value. We can access the value in bothits original and stretched form using 'stretched'
-- and 'stretchee', respectively.
--
-- Placing a value inside 'Stretched' makes it invariant under 'delay', however the inner
-- value can still be delayed using @'fmap' 'delay'@.
--
newtype Stretched a = Stretched { _stretchee :: Couple Duration a }
  deriving (
    Eq,           Num,      Fractional,   Floating,
    Ord,          Real,     RealFrac,     Functor,
    Applicative,  Monad,    Foldable,     Traversable, Typeable
    )
            -- Comonad,

instance Wrapped (Stretched a) where
  type Unwrapped (Stretched a) = (Duration, a)
  _Wrapped' = iso (getCouple . _stretchee) (Stretched . Couple)

instance Rewrapped (Stretched a) (Stretched b)

-- $semantics Stretched
--
-- @
-- type Stretched = (Duration, a)
-- @
--

-- >>> stretch 2 $ (5,1)^.stretched
-- (10,1)^.stretched
--
-- >>> delay 2 $ (5,1)^.stretched
-- (5,1)^.stretched
--
instance Transformable (Stretched a) where
  transform t = over _Wrapped $ first (transform t)

instance Reversible (Stretched a) where
  rev = stretch (-1)

instance Splittable a => Splittable (Stretched a) where
  beginning d = over _Wrapped $ \(s, v) -> (beginning d s, beginning d v)
  ending    d = over _Wrapped $ \(s, v) -> (ending    d s, ending    d v)

instance HasDuration (Stretched a) where
  _duration = _duration . view _Wrapped

instance IsString a => IsString (Stretched a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Stretched a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Stretched a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Stretched a) where
  fromDynamics = pure . fromDynamics

instance (Show a, Transformable a) => Show (Stretched a) where
  show x = show (x^.from stretched) ++ "^.stretched"

-- | View a stretched value as a pair of the original value and a stretch factor.
stretched :: Iso (Duration, a) (Duration, b) (Stretched a) (Stretched b)
stretched = _Unwrapped

-- | Access the stretched value.
-- Taking a value out carries out the stretch (using the 'Transformable' instance),
-- while putting a value in carries out the reverse transformation.
--
-- >>> view stretchee $ (2,3::Duration)^.stretched
-- 6
--
-- >>> set stretchee 6 $ (2,1::Duration)^.stretched
-- (2,3)^.stretched
--
stretchee :: Transformable a => Lens (Stretched a) (Stretched a) a a
stretchee = _Wrapped `dep` (transformed . stretching)

-- | A stretched value as a duration carrying an associated value.
-- Whitness by picking a trivial value.
--
-- >>> 2^.durationStretched
-- (2,())^.stretched
--
durationStretched :: Iso' Duration (Stretched ())
durationStretched = iso (\d -> (d,())^.stretched) (^.duration)
-- >>> (pure ())^.from durationStretched
-- 1
-- >>> (pure () :: Stretched ())^.duration
-- 1

-- TODO could also be an iso...
stretchedComplement :: Stretched a -> Stretched a
stretchedComplement (Stretched (Couple (d,x))) = Stretched $ Couple (negateV d, x)
-- FIXME negateV is negate not recip
-- The negateV method should follow (^+^), which is (*) for durations (is this bad?)


-- TODO move
-- dep :: (a ~ b, d ~ c, s ~ t) => Lens s t (x,a) (x,b) -> (x -> Lens a b c d) -> Lens s t c d
dep :: Lens' s (x,a) -> (x -> Lens' a c) -> Lens' s c
dep l f = lens getter setter
  where
    getter s = let
      (x,a) = view l s
      l2    = f x
      in view l2 a
    setter s b = let
      (x,_) = view l s
      l2    = f x
      in set (l._2.l2) b s


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
-------------------------------------------------------------------------------------




-- import           Data.AffineSpace
-- import           Data.AffineSpace.Point
-- import           Data.Map               (Map)
-- import qualified Data.Map               as Map
-- import           Data.Ratio
-- import           Data.Semigroup
-- import           Data.Set               (Set)
-- import qualified Data.Set               as Set
-- import           Data.VectorSpace
--
-- import           Music.Time.Delayed
-- import           Music.Time.Reverse
-- import           Music.Time.Split
--
-- import           Control.Applicative
-- import           Control.Lens           hiding (Indexable, Level, above, below,
--                                          index, inside, parts, reversed,
--                                          transform, (<|), (|>))
-- import           Control.Monad
-- import           Control.Monad.Compose
-- import           Control.Monad.Plus
-- import           Data.Foldable          (Foldable)
-- import qualified Data.Foldable          as Foldable
-- import           Data.Traversable       (Traversable)
-- import qualified Data.Traversable       as T
-- import           Data.Typeable
-- import           Music.Time.Internal.Util

-- |
-- A 'Track' is a parallel composition of values.
--
-- @
-- type Track a = [Delayed a]
-- @
--
newtype Track a = Track { getTrack :: TrackList (TrackEv a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)
-- {-# DEPRECATED Track "Use 'Chord'" #-}

-- A track is a list of events with explicit onset.
--
-- Track is a 'Monoid' under parallel composition. 'mempty' is the empty track
-- and 'mappend' interleaves values.
--
-- Track is a 'Monad'. 'return' creates a track containing a single value at time
-- zero, and '>>=' transforms the values of a track, allowing the addition and
-- removal of values relative to the time of the value. Perhaps more intuitively,
-- 'join' delays each inner track to start at the offset of an outer track, then
-- removes the intermediate structure.

-- Can use [] or Seq here
type TrackList = []

-- Can use any type as long as trackEv provides an Iso
type TrackEv a = Delayed a

trackEv :: Iso (Delayed a) (Delayed b) (TrackEv a) (TrackEv b)
trackEv = id

instance Applicative Track where
  pure  = return
  (<*>) = ap

instance Alternative Track where
  (<|>) = (<>)
  empty = mempty

instance Monad Track where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

instance Wrapped (Track a) where
  type Unwrapped (Track a) = (TrackList (TrackEv a))
  _Wrapped' = iso getTrack Track

instance Rewrapped (Track a) (Track b)

instance Transformable (Track a) where
  transform s = over _Wrapped' (transform s)

instance HasPosition (Track a) where
  _onset  = safeMinimum . fmap _onset . view _Wrapped'
  _offset = safeMaximum . fmap _offset . view _Wrapped'

-- TODO move
-- safeMinimum xs = if null xs then 0 else minimum xs
-- safeMaximum xs = if null xs then 0 else maximum xs

instance HasDuration (Track a) where
  _duration x = _offset x .-. _onset x

-- instance Splittable a => Splittable (Track a) where
--   -- TODO

-- instance Reversible a => Reversible (Track a) where
--   -- TODO


-- |
-- Create a track from a list of notes.
--
-- Se also 'delayeds'.
--
track :: Getter [Delayed a] (Track a)
track = from unsafeTrack
{-# INLINE track #-}

delayeds :: Lens (Track a) (Track b) [Delayed a] [Delayed b]
delayeds = unsafeTrack
{-# INLINE delayeds #-}

singleDelayed :: Prism' (Track a) (Delayed a)
singleDelayed = unsafeTrack . single
{-# INLINE singleDelayed #-}

unsafeTrack :: Iso (Track a) (Track b) [Delayed a] [Delayed b]
unsafeTrack = _Wrapped
{-# INLINE unsafeTrack #-}


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
-------------------------------------------------------------------------------------



-- import           Music.Time.Internal.Transform
-- import           Music.Time.Types


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
-------------------------------------------------------------------------------------



-- import           Control.Lens           hiding (Indexable, Level, above, below,
--                                          index, inside, parts, reversed,
--                                          transform, (<|), (|>))
-- --
-- import           Control.Applicative.Backwards
-- import           Control.Monad.State.Lazy
-- --
--
-- import           Data.AffineSpace
-- import           Data.AffineSpace.Point
-- import           Data.Semigroup
-- import           Data.Typeable
-- import           Data.VectorSpace
-- import           Data.Ratio
--
-- import           Music.Time.Internal.Util (showRatio)
-- import           Data.Fixed

-- $convert
--
-- Note that you should use '.-.' and '.+^' to convert between time and
-- duration. To refer to time zero (the beginning of the music), use
-- 'origin'.
--

-- |
-- Internal time representation. Can be anything with instances
-- for 'Fractional' and 'RealFrac'.
--
type TimeBase = Rational
-- type TimeBase = Fixed E12
{-
type TimeBase = Fixed E12

instance HasResolution a => AdditiveGroup (Fixed a) where
  zeroV = 0
  negateV = negate
  (^+^) = (+)

-- Can be enabled for experimental time representation
instance Floating TimeBase where
deriving instance Floating Time
deriving instance Floating Duration
-}


-- |
-- Duration, corresponding to note values in standard notation.
-- The standard names can be used: @1\/2@ for half note @1\/4@ for a quarter note and so on.
--
newtype Duration = Duration { getDuration :: TimeBase }
  deriving (Eq, Ord, Num, Enum, Fractional, Real, RealFrac, Typeable)

-- Duration is a one-dimensional 'VectorSpace', and is the associated vector space of time points.
-- It is a also an 'AdditiveGroup' (and hence also 'Monoid' and 'Semigroup') under addition.
--
-- 'Duration' is invariant under translation so 'delay' has no effect on it.
--

-- $semantics Duration
--
-- type Duration = R
--

instance Show Duration where
  show = showRatio . realToFrac

instance ToJSON Duration where
  toJSON = JSON.Number . realToFrac

instance InnerSpace Duration where
  (<.>) = (*)

instance AdditiveGroup Duration where
  zeroV = 0
  (^+^) = (+)
  negateV = negate

instance VectorSpace Duration where
  type Scalar Duration = Duration
  (*^) = (*)

instance Semigroup Duration where
  (<>) = (*^)

instance Monoid Duration where
  mempty  = 1
  mappend = (*^)
 -- TODO use some notion of norm rather than 1

-- |
-- Convert a value to a duration.
--
toDuration :: Real a => a -> Duration
toDuration = realToFrac

-- |
-- Convert a value to a duration.
--
fromDuration :: Fractional a => Duration -> a
fromDuration = realToFrac


-- |
-- Time points, representing duration since some known reference time, typically the start
-- of the music. Note that time can be negative, representing values occuring before the
-- reference time.
--
newtype Time = Time { getTime :: TimeBase }
  deriving (Eq, Ord, Num, Enum, Fractional, Real, RealFrac, Typeable)

-- Time forms an affine space with durations as the underlying vector space, that is, we
-- can add a time to a duration to get a new time using '.+^', take the difference of two
-- times to get a duration using '.-.'. 'Time' forms an 'AffineSpace' with 'Duration' as
-- difference space.
--

-- $semantics Time
--
-- type Time = R
--

instance Show Time where
  show = showRatio . realToFrac

instance ToJSON Time where
  toJSON = JSON.Number . realToFrac

deriving instance AdditiveGroup Time

instance VectorSpace Time where
  type Scalar Time = Duration
  Duration x *^ Time y = Time (x * y)

instance AffineSpace Time where
  type Diff Time = Duration
  Time x .-. Time y   = Duration (x - y)
  Time x .+^ Duration y = Time   (x + y)

instance Semigroup Time where
  (<>) = (^+^)

instance Monoid Time where
  mempty  = zeroV
  mappend = (^+^)
  mconcat = sumV

-- |
-- Convert a value to a duration.
--
toTime :: Real a => a -> Time
toTime = realToFrac

-- |
-- Convert a value to a duration.
--
fromTime :: Fractional a => Time -> a
fromTime = realToFrac



-- TODO terminology
-- Return the "accumulative sum" of the given vecors

-- |
-- @length (offsetPoints x xs) = length xs + 1@
--
-- >>> offsetPoints (0 ::Double) [1,2,1]
-- [0.0,1.0,3.0,4.0]
--
-- @
-- offsetPoints :: 'AffineSpace' a => 'Time' -> ['Duration'] -> ['Time']
-- @
--
offsetPoints :: AffineSpace a => a -> [Diff a] -> [a]
offsetPoints = scanl (.+^)

-- | Convert to delta (time to wait before this note)
toRelativeTime :: [Time] -> [Duration]
toRelativeTime = snd . mapAccumL g 0 where g prev t = (t, t .-. prev)
-- toRelativeTime xs = fst $ mapAccumL2 g xs 0 where g t prev = (t .-. prev, t)

-- | Convert to delta (time to wait before next note)
toRelativeTimeN :: [Time] -> [Duration]
toRelativeTimeN [] = []
toRelativeTimeN xs = toRelativeTimeN' (last xs) xs

-- | Convert to delta (time to wait before next note)
toRelativeTimeN' :: Time -> [Time] -> [Duration]
toRelativeTimeN' end xs = snd $ mapAccumR g end xs where g prev t = (t, prev .-. t)

{-
TODO consolidate with this beat (used in Midi export)

toRelative = snd . List.mapAccumL g 0
    where
        g now (t,d,x) = (t, (0 .+^ (t .-. now),d,x))

-}
-- 0 x,1 x,1 x,1 x
  -- x 1,x 1,x 1,x 0

-- | Convert from delta (time to wait before this note)
toAbsoluteTime :: [Duration] -> [Time]
toAbsoluteTime = tail . offsetPoints 0


-- -- TODO use State instead
--
-- -- mapAccumL                 ::                   (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
-- -- \f -> mapM (runState . f) :: MonadState s m => (a -> s -> (b, s)) -> [a] -> s -> ([b], s)
--
-- -- mapAccumL :: (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
-- mapAccumL2   :: (a -> s -> (b, s)) -> [a] -> s -> ([b], s)
-- mapAccumL2 f = runState . mapM (state . f)







-- |
-- A 'Span' represents an onset and offset in time (or equivalently: an onset and a
-- duration, /or/ a duration and an offset, /or/ a duration and a middle point).
--
-- Pattern matching over span is possible (with @ViewPatterns@):
--
-- @
-- foo ('view' 'range'   -> (t1, t2)) = ...
-- foo ('view' 'delta'   -> (t1, d))  = ...
-- foo ('view' 'codelta' -> (d,  t2)) = ...
-- @
--
-- Another way of looking at 'Span' is that it represents a time transformation where
-- onset is translation and duration is scaling.
--
-- TODO How to use with 'transform', 'whilst' etc.
--
-- @
-- a '<->' b = (a, b)^.'from' 'range'
-- a '>->' b = (a, b)^.'from' 'delta'
-- a '<-<' b = (a, b)^.'from' 'codelta'
-- @
--
newtype Span = Delta { _delta :: (Time, Duration) }
  deriving (Eq, Ord, Typeable)

-- $semantics
--
-- type Span = Time x Time
--

-- You can create a span using the constructors '<->', '<-<' and '>->'. Note that:
--
-- > a >-> b = a         <-> (a .+^ b)
-- > a <-< b = (b .-^ a) <-> b
-- > a <-> b = a         >-> (b .-. a)
--
-- To create and destruct a span (in any of its incarnations), use the provided isomorphisms:
--
-- 'Span' is a 'Semigroup', 'Monoid' and 'AdditiveGroup':
--
-- - To convert a span to a pair, use @s^.'range'@.
--
-- - To construct a span from a pair, use @(t, u)^.'from' 'range'@.
--

--
-- $musicTimeSpanIsos
--
-- >>> (2 <-> 3)^.range
-- (2,3)
--
-- >>> (2 <-> 3)^.delta
-- (2,1)
--
-- >>> (10 >-> 5)^.range
-- (10,15)
--
-- >>> (10 >-> 5)^.delta
-- (10,5)
--

instance Show Span where
  -- show = showDelta
  show = showRange
  -- Which form should we use?

instance ToJSON Span where
  toJSON (view range -> (a,b)) = JSON.object [ ("onset", toJSON a), ("offset", toJSON b) ]


-- |
-- 'zeroV' or 'mempty' represents the /unit interval/ @0 \<-\> 1@, which also happens to
-- be the identity transformation.
--
instance Semigroup Span where
  (<>) = (^+^)

-- |
-- '<>' or '^+^' composes transformations, i.e. both time and duration is stretched,
-- and then time is added.
--
instance Monoid Span where
  mempty  = zeroV
  mappend = (^+^)

-- |
-- 'negateV' returns the inverse of a given transformation.
--
instance AdditiveGroup Span where
  zeroV   = 0 <-> 1
  Delta (t1, d1) ^+^ Delta (t2, d2) = Delta (t1 ^+^ d1 *^ t2, d1*d2)
  negateV (Delta (t, d)) = Delta (-t ^/ d, recip d)

--
-- a >-> b = a         <-> (a .+^ b)
-- a <-< b = (b .-^ a) <-> b
-- a <-> b = a         >-> (b .-. a)
-- (b .-^ a) <-> b = a <-< b
--

infixl 6 <->
infixl 6 >->
infixl 6 <-<

-- |
-- @t \<-\> u@ represents the span between @t@ and @u@.
--
(<->) :: Time -> Time -> Span
t <-> u = t >-> (u .-. t)

-- |
-- @t >-> d@ represents the span between @t@ and @t .+^ d@.
--
(>->) :: Time -> Duration -> Span
(>->) = curry Delta

-- |
-- @d \<-\> t@ represents the span between @t .-^ d@ and @t@.
--
(<-<) :: Duration -> Time -> Span
a <-< b = (b .-^ a) <-> b


-- |
-- View a span as pair of onset and offset.
--
range :: Iso' Span (Time, Time)
range = iso _range $ uncurry (<->)
  where
    _range x = let (t, d) = _delta x in (t, t .+^ d)

-- |
-- View a span as a pair of onset and duration.
--
delta :: Iso' Span (Time, Duration)
delta = iso _delta Delta

-- |
-- View a span as a pair of duration and offset.
--
codelta :: Iso' Span (Duration, Time)
codelta = iso _codelta $ uncurry (<-<)
  where
    _codelta x = let (t, d) = _delta x in (d, t .+^ d)

-- |
-- Show a span in range notation, i.e. @t1 \<-\> t2@.
--
showRange :: Span -> String
showRange (view range -> (t,u)) = show t ++ " <-> " ++ show u

-- |
-- Show a span in delta notation, i.e. @t >-> d@.
--
showDelta :: Span -> String
showDelta (view delta -> (t,d)) = show t ++ " >-> " ++ show d

-- |
-- Show a span in codelta notation, i.e. @t <-< d@.
--
showCodelta :: Span -> String
showCodelta (view codelta -> (d,u)) = show d ++ " <-< " ++ show u

-- |
-- Access the delay component in a span.
--
delayComponent :: Span -> Time
delayComponent x = x ^. delta . _1

-- |
-- Access the stretch component in a span.
--
stretchComponent :: Span -> Duration
stretchComponent x = x ^. delta . _2

-- |
-- A prism to the subset of 'Span' that performs a delay but no stretch.
--
fixedDurationSpan :: Prism' Span Time
fixedDurationSpan = prism' (\t -> view (from delta) (t, 1)) $ \x -> case view delta x of
  (t, 1) -> Just t
  _      -> Nothing

-- |
-- A prism to the subset of 'Span' that performs a stretch but no delay.
--
fixedOnsetSpan :: Prism' Span Duration
fixedOnsetSpan = prism' (\d -> view (from delta) (0, d)) $ \x -> case view delta x of
  (0, d) -> Just d
  _      -> Nothing

--
-- $forwardBackWardEmpty
--
-- A span is either /forward/, /backward/ or /empty/.
--
-- @any id [isForwardSpan x, isBackwardSpan x, isEmptySpan x] == True@
-- @all not [isForwardSpan x, isBackwardSpan x, isEmptySpan x] == False@
--

-- |
-- Whether the given span has a positive duration, i.e. whether its 'onset' is before its 'offset'.
--
isForwardSpan :: Span -> Bool
isForwardSpan = (> 0) . signum . _durationS

-- |
-- Whether the given span has a negative duration, i.e. whether its 'offset' is before its 'onset'.
--
isBackwardSpan :: Span -> Bool
isBackwardSpan = (< 0) . signum . _durationS

-- |
-- Whether the given span is empty, i.e. whether its 'onset' and 'offset' are equivalent.
--
isEmptySpan :: Span -> Bool
isEmptySpan = (== 0) . signum . _durationS


-- |
-- Reflect a span through its midpoint.
--
reverseSpan :: Span -> Span
reverseSpan s = reflectSpan (_midpointS s) s

-- |
-- Reflect a span through an arbitrary point.
--
reflectSpan :: Time -> Span -> Span
reflectSpan p = over (range . both) (reflectThrough p)

-- |
-- Normalize a span, i.e. reverse it if negative, and do nothing otherwise.
--
-- @
-- _duration s = _duration (normalizeSpan s)
-- _midpoint s = _midpoint (normalizeSpan s)
-- @
--
normalizeSpan :: Span -> Span
normalizeSpan s = if isForwardSpan s then s else reverseSpan s
-- TODO Duplicate as normalizeNoteSpan

-- |
-- Whether this is a proper span, i.e. whether @'_onset' x '<' '_offset' x@.
--
isProper :: Span -> Bool
isProper (view range -> (t, u)) = t < u
{-# DEPRECATED isProper "Use 'isForwardSpan'" #-}

infixl 5 `inside`
infixl 5 `encloses`
infixl 5 `properlyEncloses`
infixl 5 `overlaps`
-- infixl 5 `encloses`
-- infixl 5 `encloses`
-- infixl 5 `encloses`

-- |
-- Whether the given point falls inside the given span (inclusively).
--
-- Designed to be used infix, for example
--
-- >>> 0.5 `inside` 1 <-> 2
-- False
--
-- >>> 1.5 `inside` 1 <-> 2
-- True
--
-- >>> 1 `inside` 1 <-> 2
-- True
--
inside :: Time -> Span -> Bool
inside x (view range -> (t, u)) = t <= x && x <= u

-- |
-- Whether the first given span encloses the second span.
--
-- >>> 0 <-> 3 `encloses` 1 <-> 2
-- True
--
-- >>> 0 <-> 2 `encloses` 1 <-> 2
-- True
--
-- >>> 1 <-> 3 `encloses` 1 <-> 2
-- True
--
-- >>> 1 <-> 2 `encloses` 1 <-> 2
-- True
--
encloses :: Span -> Span -> Bool
a `encloses` b = _onsetS b `inside` a && _offsetS b `inside` a

-- |
-- Whether the first given span encloses the second span.
--
-- >>> 0 <-> 3 `properlyEncloses` 1 <-> 2
-- True
--
-- >>> 0 <-> 2 `properlyEncloses` 1 <-> 2
-- True
--
-- >>> 1 <-> 3 `properlyEncloses` 1 <-> 2
-- True
--
-- >>> 1 <-> 2 `properlyEncloses` 1 <-> 2
-- False
--
properlyEncloses :: Span -> Span -> Bool
a `properlyEncloses` b = a `encloses` b && a /= b



-- TODO more intuitive param order

afterOnset :: Time -> Span -> Bool
t `afterOnset` s = t >= _onsetS s

strictlyAfterOnset :: Time -> Span -> Bool
t `strictlyAfterOnset` s = t > _onsetS s

beforeOnset :: Time -> Span -> Bool
t `beforeOnset` s = t <= _onsetS s

strictlyBeforeOnset :: Time -> Span -> Bool
t `strictlyBeforeOnset` s = t < _onsetS s

afterOffset :: Time -> Span -> Bool
t `afterOffset` s = t >= _offsetS s

strictlyAfterOffset :: Time -> Span -> Bool
t `strictlyAfterOffset` s = t > _offsetS s

beforeOffset :: Time -> Span -> Bool
t `beforeOffset` s = t <= _offsetS s

strictlyBeforeOffset :: Time -> Span -> Bool
t `strictlyBeforeOffset` s = t < _offsetS s


-- Param order OK

-- Name?
startsWhenStarts :: Span -> Span -> Bool
a `startsWhenStarts` b = _onsetS a == _onsetS b

-- Name?
startsWhenStops :: Span -> Span -> Bool
a `startsWhenStops` b = _onsetS a == _offsetS b

-- Name?
stopsWhenStops :: Span -> Span -> Bool
a `stopsWhenStops` b = _offsetS a == _offsetS b

-- Name?
stopsWhenStarts :: Span -> Span -> Bool
a `stopsWhenStarts` b = _offsetS a == _onsetS b


startsBefore :: Span -> Span -> Bool
a `startsBefore` b = _onsetS a < _onsetS b

startsLater :: Span -> Span -> Bool
a `startsLater` b = _onsetS a > _onsetS b

stopsAtTheSameTime :: Span -> Span -> Bool
a `stopsAtTheSameTime` b = _offsetS a == _offsetS b

stopsBefore :: Span -> Span -> Bool
a `stopsBefore` b = _offsetS a < _offsetS b

stopsLater :: Span -> Span -> Bool
a `stopsLater` b = _offsetS a > _offsetS b

{-
contains
curtails
delays
happensDuring
intersects
trisects
isCongruentTo
overlapsAllOf
overlapsOnlyOnsetOf
overlapsOnlyOffsetOf
overlapsOnsetOf
overlapsOffsetOf



-}

-- timespantools.timespan_2_starts_during_timespan_1
-- timespantools.timespan_2_starts_when_timespan_1_starts
-- timespantools.timespan_2_starts_when_timespan_1_stops
-- timespantools.timespan_2_stops_after_timespan_1_starts
-- timespantools.timespan_2_stops_after_timespan_1_stops
-- timespantools.timespan_2_stops_before_timespan_1_starts
-- timespantools.timespan_2_stops_before_timespan_1_stops
-- timespantools.timespan_2_stops_during_timespan_1
-- timespantools.timespan_2_stops_when_timespan_1_starts
-- timespantools.timespan_2_stops_when_timespan_1_stops
-- timespantools.timespan_2_trisects_timespan_1



-- |
-- Whether the given span overlaps.
--
overlaps :: Span -> Span -> Bool
a `overlaps` b = not (a `isBefore` b) && not (b `isBefore` a)

-- |
-- Whether the first given span occurs before the second span.
--
isBefore :: Span -> Span -> Bool
a `isBefore` b = (_onsetS a `max` _offsetS a) <= (_onsetS b `min` _offsetS b)


-- TODO resolve this so we can use actual onset/offset etc in the above definitions
-- Same as (onset, offset), defined here for bootstrapping reasons
_onsetS    (view range -> (t1, t2)) = t1
_offsetS   (view range -> (t1, t2)) = t2
_midpointS  s = _onsetS s .+^ _durationS s / 2
_durationS s = _offsetS s .-. _onsetS s

{-
Two alternative definitions for midpoint:

midpoint x = onset x + duration x / 2
midpoint x = (onset x + offset x) / 2

Both equivalent. Proof:

  let d = b - a
  (a + b)/2 = a + d/2
  (a + b)/2 = a + (b - a)/2
  a + b     = 2a + (b - a)
  a + b     = a + b
-}




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
-------------------------------------------------------------------------------------

--
--
-- import           Data.AffineSpace
-- import           Data.AffineSpace.Point
-- import           Data.Functor.Adjunction  (unzipR)
-- import           Data.Functor.Context
-- import           Data.Map                 (Map)
-- import qualified Data.Map                 as Map
-- import           Data.Maybe
-- import           Data.Ratio
-- import           Data.Semigroup
-- import           Data.Set                 (Set)
-- import qualified Data.Set                 as Set
-- import           Data.Sequence            (Seq)
-- import qualified Data.Sequence            as Seq
-- import           Data.String
-- import           Data.VectorSpace
--
-- import           Music.Time.Reverse
-- import           Music.Time.Split
-- import           Music.Time.Stretched
--
-- import           Control.Applicative
-- import           Control.Lens             hiding (Indexable, Level, above,
--                                            below, index, inside, parts,
--                                            reversed, transform, (<|), (|>))
-- import           Control.Monad
-- import           Control.Monad.Compose
-- import           Control.Monad.Plus
-- import           Data.Foldable            (Foldable)
-- import qualified Data.Foldable            as Foldable
-- import qualified Data.List
-- import           Data.List.NonEmpty       (NonEmpty)
-- import           Data.Traversable         (Traversable)
-- import qualified Data.Traversable         as T
-- import           Data.Typeable
-- import           Music.Dynamics.Literal
-- import           Music.Pitch.Literal
-- import           Music.Time.Internal.Util
--
-- |
-- A 'Voice' is a sequential composition of non-overlapping stretched values.
--
-- Both 'Voice' and 'Stretched' have duration but no position. The difference
-- is that 'Stretched' sustains a single value throughout its duration, while
-- a voice may contain multiple values. It is called voice because it is
-- generalizes the notation of a voice in choral or multi-part instrumental music.
--
-- It may be useful to think about 'Voice' and 'Stretched' as vectors in time space
-- (i.e. 'Duration'), that also happens to carry around other values, such as pitches.
--
newtype Voice a = Voice { getVoice :: VoiceList (VoiceEv a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Eq)

instance (Show a, Transformable a) => Show (Voice a) where
  show x = show (x^.stretcheds) ++ "^.voice"

-- A voice is a list of events with explicit duration. Events can not overlap.
--
-- Voice is a 'Monoid' under sequential composition. 'mempty' is the empty part and 'mappend'
-- appends parts.

--
-- Voice is a 'Monad'. 'return' creates a part containing a single value of duration
-- one, and '>>=' transforms the values of a part, allowing the addition and
-- removal of values under relative duration. Perhaps more intuitively, 'join' scales
-- each inner part to the duration of the outer part, then removes the
-- intermediate structure.

-- Can use [] or Seq here
type VoiceList = []

-- Can use any type as long as voiceEv provides an Iso
type VoiceEv a = Stretched a
-- type VoiceEv a = ((),Stretched a)

voiceEv :: Iso (Stretched a) (Stretched b) (VoiceEv a) (VoiceEv b)
voiceEv = id
-- voiceEv = iso add remove
--   where
--     add x = ((),x)
--     remove ((),x) = x

instance Applicative Voice where
  pure  = return
  (<*>) = ap

instance Alternative Voice where
  (<|>) = (<>)
  empty = mempty

instance Monad Voice where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

instance MonadPlus Voice where
  mzero = mempty
  mplus = mappend

instance Wrapped (Voice a) where
  type Unwrapped (Voice a) = (VoiceList (VoiceEv a))
  _Wrapped' = iso getVoice Voice

instance Rewrapped (Voice a) (Voice b)

instance Cons (Voice a) (Voice b) (Stretched a) (Stretched b) where
  _Cons = prism (\(s,v) -> (view voice.return $ s) <> v) $ \v -> case view stretcheds v of
    []      -> Left  mempty
    (x:xs)  -> Right (x, view voice xs)

instance Snoc (Voice a) (Voice b) (Stretched a) (Stretched b) where
  _Snoc = prism (\(v,s) -> v <> (view voice.return $ s)) $ \v -> case unsnoc (view stretcheds v) of
    Nothing      -> Left  mempty
    Just (xs, x) -> Right (view voice xs, x)

instance Transformable (Voice a) where
  transform s = over _Wrapped' (transform s)

instance HasDuration (Voice a) where
  _duration = Foldable.sum . fmap _duration . view _Wrapped'

instance Splittable a => Splittable (Voice a) where
  split t x
    | t <= 0           = (mempty, x)
    | t >= _duration x = (x,      mempty)
    | otherwise        = let (a,b) = split' t {-split-} (x^._Wrapped) in (a^._Unwrapped, b^._Unwrapped)
    where
      split' = error "TODO"

instance Reversible a => Reversible (Voice a) where
  rev = over _Wrapped' (fmap rev) -- TODO OK?


-- Lifted instances

instance IsString a => IsString (Voice a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Voice a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Voice a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Voice a) where
  fromDynamics = pure . fromDynamics

-- Bogus instance, so we can use [c..g] expressions
instance Enum a => Enum (Voice a) where
  toEnum = return . toEnum
  fromEnum = list 0 (fromEnum . head) . Foldable.toList

-- Bogus instance, so we can use numeric literals
instance Num a => Num (Voice a) where
  fromInteger = return . fromInteger
  abs    = fmap abs
  signum = fmap signum
  (+)    = error "Not implemented"
  (-)    = error "Not implemented"
  (*)    = error "Not implemented"

-- Bogus instances, so we can use c^*2 etc.
instance AdditiveGroup (Voice a) where
  zeroV   = error "Not implemented"
  (^+^)   = error "Not implemented"
  negateV = error "Not implemented"

instance VectorSpace (Voice a) where
  type Scalar (Voice a) = Duration
  d *^ s = d `stretch` s


-- |
-- Create a 'Voice' from a list of 'Stretched' values.
--
-- This is a 'Getter' (rather than a function) for consistency:
--
-- @
-- [ (0 '<->' 1, 10)^.'stretched',
--   (1 '<->' 2, 20)^.'stretched',
--   (3 '<->' 4, 30)^.'stretched' ]^.'voice'
-- @
--
-- @
-- 'view' 'voice' $ 'map' ('view' 'stretched') [(0 '<->' 1, 1)]
-- @
--
-- Se also 'stretcheds'.
--
voice :: Getter [Stretched a] (Voice a)
voice = from unsafeStretcheds
-- voice = to $ flip (set stretcheds) empty
{-# INLINE voice #-}

-- |
-- View a 'Voice' as a list of 'Stretched' values.
--
-- @
-- 'view' 'stretcheds'                        :: 'Voice' a -> ['Stretched' a]
-- 'set'  'stretcheds'                        :: ['Stretched' a] -> 'Voice' a -> 'Voice' a
-- 'over' 'stretcheds'                        :: (['Stretched' a] -> ['Stretched' b]) -> 'Voice' a -> 'Voice' b
-- @
--
-- @
-- 'preview'  ('stretcheds' . 'each')           :: 'Voice' a -> 'Maybe' ('Stretched' a)
-- 'preview'  ('stretcheds' . 'element' 1)      :: 'Voice' a -> 'Maybe' ('Stretched' a)
-- 'preview'  ('stretcheds' . 'elements' odd)   :: 'Voice' a -> 'Maybe' ('Stretched' a)
-- @
--
-- @
-- 'set'      ('stretcheds' . 'each')           :: 'Stretched' a -> 'Voice' a -> 'Voice' a
-- 'set'      ('stretcheds' . 'element' 1)      :: 'Stretched' a -> 'Voice' a -> 'Voice' a
-- 'set'      ('stretcheds' . 'elements' odd)   :: 'Stretched' a -> 'Voice' a -> 'Voice' a
-- @
--
-- @
-- 'over'     ('stretcheds' . 'each')           :: ('Stretched' a -> 'Stretched' b) -> 'Voice' a -> 'Voice' b
-- 'over'     ('stretcheds' . 'element' 1)      :: ('Stretched' a -> 'Stretched' a) -> 'Voice' a -> 'Voice' a
-- 'over'     ('stretcheds' . 'elements' odd)   :: ('Stretched' a -> 'Stretched' a) -> 'Voice' a -> 'Voice' a
-- @
--
-- @
-- 'toListOf' ('stretcheds' . 'each')                :: 'Voice' a -> ['Stretched' a]
-- 'toListOf' ('stretcheds' . 'elements' odd)        :: 'Voice' a -> ['Stretched' a]
-- 'toListOf' ('stretcheds' . 'each' . 'filtered'
--              (\\x -> '_duration' x \< 2))  :: 'Voice' a -> ['Stretched' a]
-- @
--
-- This is not an 'Iso', as the note list representation does not contain meta-data.
-- To construct a score from a note list, use 'score' or @'flip' ('set' 'stretcheds') 'empty'@.
--
stretcheds :: Lens (Voice a) (Voice b) [Stretched a] [Stretched b]
stretcheds = unsafeStretcheds
{-# INLINE stretcheds #-}

eventsV :: Lens (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
eventsV = unsafeEventsV
{-# INLINE eventsV #-}

unsafeEventsV :: Iso (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
unsafeEventsV = iso (map (^.from stretched) . (^.stretcheds)) ((^.voice) . map (^.stretched))
{-# INLINE unsafeEventsV #-}

unsafeStretcheds :: Iso (Voice a) (Voice b) [Stretched a] [Stretched b]
unsafeStretcheds = _Wrapped
{-# INLINE unsafeStretcheds #-}

singleStretched :: Prism' (Voice a) (Stretched a)
singleStretched = unsafeStretcheds . single
{-# INLINE singleStretched #-}
{-# DEPRECATED singleStretched "Use 'unsafeStretcheds . single'" #-}


-- |
-- Unzip the given voice. This is specialization of 'unzipR'.
--
unzipVoice :: Voice (a, b) -> (Voice a, Voice b)
unzipVoice = unzipR

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoice :: Voice a -> Voice b -> Voice (a, b)
zipVoice = zipVoiceWith (,)

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoice3 :: Voice a -> Voice b -> Voice c -> Voice (a, (b, c))
zipVoice3 a b c = zipVoice a (zipVoice b c)

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoice4 :: Voice a -> Voice b -> Voice c -> Voice d -> Voice (a, (b, (c, d)))
zipVoice4 a b c d = zipVoice a (zipVoice b (zipVoice c d))

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoice5 :: Voice a -> Voice b -> Voice c -> Voice d -> Voice e -> Voice (a, (b, (c, (d, e))))
zipVoice5 a b c d e = zipVoice a (zipVoice b (zipVoice c (zipVoice d e)))

-- |
-- Join the given voices by pairing values and selecting the first duration.
--
zipVoiceNoScale :: Voice a -> Voice b -> Voice (a, b)
zipVoiceNoScale = zipVoiceWithNoScale (,)

-- |
-- Join the given voices by pairing values and selecting the first duration.
--
zipVoiceNoScale3 :: Voice a -> Voice b -> Voice c -> Voice (a, (b, c))
zipVoiceNoScale3 a b c = zipVoiceNoScale a (zipVoiceNoScale b c)

-- |
-- Join the given voices by pairing values and selecting the first duration.
--
zipVoiceNoScale4 :: Voice a -> Voice b -> Voice c -> Voice d -> Voice (a, (b, (c, d)))
zipVoiceNoScale4 a b c d = zipVoiceNoScale a (zipVoiceNoScale b (zipVoiceNoScale c d))

-- |
-- Join the given voices by pairing values and selecting the first duration.
--
zipVoiceNoScale5 :: Voice a -> Voice b -> Voice c -> Voice d -> Voice e -> Voice (a, (b, (c, (d, e))))
zipVoiceNoScale5 a b c d e = zipVoiceNoScale a (zipVoiceNoScale b (zipVoiceNoScale c (zipVoiceNoScale d e)))


-- |
-- Join the given voices by multiplying durations and combining values using the given function.
--
zipVoiceWith :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith = zipVoiceWith' (*)

-- |
-- Join the given voices without combining durations.
--
zipVoiceWithNoScale :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWithNoScale = zipVoiceWith' const

-- |
-- Join the given voices by combining durations and values using the given function.
--
zipVoiceWith' :: (Duration -> Duration -> Duration) -> (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith' f g
  ((unzip.view eventsV) -> (ad, as))
  ((unzip.view eventsV) -> (bd, bs))
  = let cd = zipWith f ad bd
        cs = zipWith g as bs
     in view (from unsafeEventsV) (zip cd cs)

-- |
-- Merge consecutive equal notes.
--
fuse :: Eq a => Voice a -> Voice a
fuse = fuseBy (==)

-- |
-- Merge consecutive equal notes using the given function.
--
fuseBy :: (a -> a -> Bool) -> Voice a -> Voice a
fuseBy p = fuseBy' p head

-- |
-- Merge consecutive equal notes using the given equality predicate and merge function.
--
fuseBy' :: (a -> a -> Bool) -> ([a] -> a) -> Voice a -> Voice a
fuseBy' p g = over unsafeEventsV $ fmap foldNotes . Data.List.groupBy (inspectingBy snd p)
  where
    -- Add up durations and use a custom function to combine notes
    --
    -- Typically, the combination function us just 'head', as we know that group returns
    -- non-empty lists of equal elements.
    foldNotes (unzip -> (ds, as)) = (sum ds, g as)

-- |
-- Fuse all rests in the given voice. The resulting voice will have no consecutive rests.
--
fuseRests :: Voice (Maybe a) -> Voice (Maybe a)
fuseRests = fuseBy (\x y -> isNothing x && isNothing y)

-- |
-- Remove all rests in the given voice by prolonging the previous note. Returns 'Nothing'
-- if and only if the given voice contains rests only.
--
coverRests :: Voice (Maybe a) -> Maybe (Voice a)
coverRests x = if hasOnlyRests then Nothing else Just (fmap fromJust $ fuseBy merge x)
  where
    norm = fuseRests x
    merge Nothing  Nothing  = error "Voice normalized, so consecutive rests are impossible"
    merge (Just x) Nothing  = True
    merge Nothing  (Just x) = True
    merge (Just x) (Just y) = False
    hasOnlyRests = all isNothing $ toListOf traverse x -- norm


withContext :: Voice a -> Voice (Ctxt a)
withContext = over valuesV addCtxt

-- TODO expose?
voiceFromRhythm :: [Duration] -> Voice ()
voiceFromRhythm = mkVoice . fmap (, ())

mkVoice = view voice . fmap (view stretched)

--
-- TODO more elegant definition of durationsV and valuesV using indexed traversal or similar?
--

durationsV :: Lens' (Voice a) [Duration]
durationsV = lens getDurs (flip setDurs)
  where
    getDurs :: Voice a -> [Duration]
    getDurs = map fst . view eventsV

    setDurs :: [Duration] -> Voice a -> Voice a
    setDurs ds as = zipVoiceWith' (\a b -> a) (\a b -> b) (mconcat $ map durToVoice ds) as

    durToVoice d = stretch d $ pure ()

valuesV :: Lens (Voice a) (Voice b) [a] [b]
valuesV = lens getValues (flip setValues)
  where
    -- getValues :: Voice a -> [a]
    getValues = map snd . view eventsV

    -- setValues :: [a] -> Voice b -> Voice a
    setValues as bs = zipVoiceWith' (\a b -> b) (\a b -> a) (listToVoice as) bs

    listToVoice = mconcat . map pure

-- |
-- Transform the durations, leaving values intact.
withDurations :: ([Duration] -> [Duration]) -> Voice a -> Voice a
withDurations = over durationsV

-- |
-- Transform the values, leaving durations intact.
withValues :: ([a] -> [b]) -> Voice a -> Voice b
withValues = over valuesV

-- |
-- Rotate durations by the given number of steps, leaving values intact.
--
rotateDurations :: Int -> Voice a -> Voice a
rotateDurations n = over durationsV (rotate n)

-- |
-- Rotate values by the given number of steps, leaving durations intact.
--
rotateValues :: Int -> Voice a -> Voice a
rotateValues n = over valuesV (rotate n)

-- |
-- Reverse durations, leaving values intact.
--
reverseDurations :: Voice a -> Voice a
reverseDurations = over durationsV reverse

-- |
-- Reverse values, leaving durations intact.
--
reverseValues :: Voice a -> Voice a
reverseValues = over valuesV reverse

-- Lens "filtered" through a voice
voiceLens :: (s -> a) -> (b -> s -> t) -> Lens (Voice s) (Voice t) (Voice a) (Voice b)
voiceLens getter setter = lens (fmap getter) (flip $ zipVoiceWithNoScale setter)

-- TODO generalize to any zippable thing
-- voiceL :: ALens s t a b -> Lens (Voice s) (Voice t) (Voice a) (Voice b)
voiceL l = voiceLens (view $ cloneLens l) (set $ cloneLens l)


-- TODO not meta-safe
voiceAsList :: Iso (Voice a) (Voice b) [a] [b]
voiceAsList = iso voiceToList listToVoice
  where
    voiceToList = map snd . view eventsV
    listToVoice = mconcat . fmap pure

listAsVoice :: Iso [a] [b] (Voice a) (Voice b)
listAsVoice = from voiceAsList


--
-- TODO
-- Implement meta-data
--

-- List functions
headV, lastV :: Voice a -> Maybe (Stretched a)
headV = preview _head
lastV = preview _head

tailV, initV :: Voice a -> Maybe (Voice a)
tailV = preview _tail
initV = preview _init

consV :: Stretched a -> Voice a -> Voice a
unconsV :: Voice a -> Maybe (Stretched a, Voice a)
consV = cons
unconsV = uncons

snocV :: Voice a -> Stretched a -> Voice a
unsnocV :: Voice a -> Maybe (Voice a, Stretched a)
snocV = snoc
unsnocV = unsnoc

nullV :: Voice a -> Bool
nullV = nullOf eventsV

lengthV :: Voice a -> Int
lengthV = lengthOf eventsV

mapV :: (a -> b) -> Voice a -> Voice b
mapV = fmap


-- Voice-specific

sameDurations :: Voice a -> Voice b -> Bool
sameDurations a b = view durationsV a == view durationsV b

mergeIfSameDuration :: Voice a -> Voice b -> Maybe (Voice (a, b))
mergeIfSameDuration = mergeIfSameDurationWith (,)

mergeIfSameDurationWith :: (a -> b -> c) -> Voice a -> Voice b -> Maybe (Voice c)
mergeIfSameDurationWith f a b
  | sameDurations a b = Just $ zipVoiceWithNoScale f a b
  | otherwise         = Nothing

-- splitAt :: [Duration] -> Voice a -> [Voice a]
-- splitTiesAt :: Tiable a => [Duration] -> Voice a -> [Voice a]

-- |
-- Split all notes of the latter voice at the onset/offset of the former.
--
-- >>> ["a",(2,"b")^.stretched,"c"]^.voice
-- [(1,"a")^.stretched,(2,"b")^.stretched,(1,"c")^.stretched]^.voice
--
splitLatterToAssureSameDuration :: Voice b -> Voice b -> Voice b
splitLatterToAssureSameDuration = splitLatterToAssureSameDurationWith dup
  where
    dup x = (x,x)

splitLatterToAssureSameDurationWith :: (b -> (b, b)) -> Voice b -> Voice b -> Voice b
splitLatterToAssureSameDurationWith = undefined

polyToHomophonic      :: [Voice a] -> Maybe (Voice [a])
polyToHomophonic = undefined

polyToHomophonicForce :: [Voice a] -> Voice [a]
polyToHomophonicForce = undefined

homoToPolyphonic      :: Voice [a] -> [Voice a]
homoToPolyphonic = undefined

changeCrossing   :: Ord a => Voice a -> Voice a -> (Voice a, Voice a)
changeCrossing = undefined

changeCrossingBy :: Ord b => (a -> b) -> Voice a -> Voice a -> (Voice a, Voice a)
changeCrossingBy = undefined

processExactOverlaps :: (a -> a -> (a, a)) -> Voice a -> Voice a -> (Voice a, Voice a)
processExactOverlaps = undefined

processExactOverlaps' :: (a -> b -> Either (a,b) (b,a)) -> Voice a -> Voice b -> (Voice (Either b a), Voice (Either a b))
processExactOverlaps' = undefined

onsetsRelative    :: Time -> Voice a -> [Time]
onsetsRelative = undefined

offsetsRelative   :: Time -> Voice a -> [Time]
offsetsRelative = undefined

midpointsRelative :: Time -> Voice a -> [Time]
midpointsRelative = undefined

erasRelative      :: Time -> Voice a -> [Span]
erasRelative = undefined

onsetMap  :: Time -> Voice a -> Map Time a
onsetMap = undefined

offsetMap :: Time -> Voice a -> Map Time a
offsetMap = undefined

midpointMap :: Time -> Voice a -> Map Time a
midpointMap = undefined

eraMap :: Time -> Voice a -> Map Span a
eraMap = undefined

durations :: Voice a -> [Duration]
durations = undefined

-- values :: Voice a -> [a] -- Same as Foldable.toList
-- values = undefined



{-

sameDurations           :: Voice a -> Voice b -> Bool
mergeIfSameDuration     :: Voice a -> Voice b -> Maybe (Voice (a, b))
mergeIfSameDurationWith :: (a -> b -> c) -> Voice a -> Voice b -> Maybe (Voice c)
splitAt :: [Duration] -> Voice a -> [Voice a]
-- splitTiesAt :: Tiable a => [Duration] -> Voice a -> [Voice a]
splitLatterToAssureSameDuration :: Voice b -> Voice b -> Voice b
splitLatterToAssureSameDurationWith :: (b -> (b, b)) -> Voice b -> Voice b -> Voice b
polyToHomophonic      :: [Voice a] -> Maybe (Voice [a])
polyToHomophonicForce :: [Voice a] -> Voice [a]
homoToPolyphonic      :: Voice [a] -> [Voice a]
joinVoice             :: Voice (Voice a) -> a
changeCrossing   :: Ord a => Voice a -> Voice a -> (Voice a, Voice a)
changeCrossingBy :: Ord b => (a -> b) -> Voice a -> Voice a -> (Voice a, Voice a)
processExactOverlaps :: (a -> a -> (a, a)) -> Voice a -> Voice a -> (Voice a, Voice a)
processExactOverlaps' :: (a -> b -> Either (a,b) (b,a)) -> Voice a -> Voice b -> (Voice (Either b a), Voice (Either a b))
onsetsRelative    :: Time -> Voice a -> [Time]
offsetsRelative   :: Time -> Voice a -> [Time]
midpointsRelative :: Time -> Voice a -> [Time]
erasRelative      :: Time -> Voice a -> [Span]
onsetMap  :: Time -> Voice a -> Map Time a
offsetMap :: Time -> Voice a -> Map Time a
midpointMap :: Time -> Voice a -> Map Time a
eraMap :: Time -> Voice a -> Map Span a
durations :: Voice a -> [Duration]
values    :: Voice a -> [a] -- Same as Foldable.toList
isPossiblyInfinite :: Voice a -> Bool
hasMelodicDissonanceWith :: (a -> a -> Bool) -> Voice a -> Bool
hasIntervalWith :: AffineSpace a => (Diff a -> Bool) -> Voice a -> Bool
hasDurationWith :: (Duration -> Bool) -> Voice a -> Bool
reifyVoice :: Voice a -> Voice (Duration, a)
mapWithIndex :: (Int -> a -> b) -> Voice a -> Voice b
mapWithDuration :: (Duration -> a -> b) -> Voice a -> Voice b
mapWithIndexAndDuration :: (Int -> Duration -> a -> b) -> Voice a -> Voice b
_ :: Iso (Voice ()) [Duration]
asingleton' :: Prism (Voice a) (Duration, a)
asingleton :: Prism (Voice a) a
separateVoicesWith :: (a -> k) -> Voice a -> Map k (Voice a)
freeVoiceR :: (forall a. -> [a] -> a)          -> Voice a -> (a, Duration)
freeVoiceRNoDur :: ([a] -> a)          -> Voice a -> a
freeVoice  :: (forall a. -> [a] -> [a])        -> Voice a -> Voice a
freeVoice2 :: (forall a. -> [a] -> [a] -> [a]) -> Voice a -> Voice a -> Voice a
empty :: Voice a
singleton :: a -> Voice a
cons :: a -> Voice a -> Voice a
snoc :: Voice a -> a -> Voice a
append :: Voice a -> Voice a -> Voice a
ap :: Voice (a -> b) -> Voice a -> Voice b
apDur :: Voice (Duration -> Duration -> a -> b) -> Voice a -> Voice b
intersperse :: Duration -> a -> Voice a -> Voice a
-- intercalate :: Voice a -> Voice (Voice a) -> Voice a
subsequences :: Voice a -> [Voice a]
permutations :: Voice a -> [Voice a]
iterate :: (a -> a) -> a -> Voice a
repeat :: a -> Voice a
replicate :: Int -> a -> Voice a
unfoldr :: (b -> Maybe (a, b)) -> b -> Voice a
Differences between Voice and Chord (except the obviously different composition styles):
  - Voice is a Monoid, Chord just a Semigroup (??)
  - TODO represent spanners using (Voice a, Map (Int,Int) s)
  Arguably this should be part of Voice
  TODO the MVoice/TVoice stuff
newtype MVoice = Voice (Maybe a)
newtype PVoice = [Either Duration (Voice a)]
expandRepeats :: [Voice (Variant a)] -> Voice a

-}

