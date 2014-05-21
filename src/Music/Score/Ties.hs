
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
-- Provides a representation for tied notes, and a way to split a single note
-- into a pair of tied notes.
--
-------------------------------------------------------------------------------------

module Music.Score.Ties (
        -- * Tiable class
        Tiable(..),
        TieT(..),

        -- * Splitting tied notes in scores
        splitTiesVoice,
        splitTiesVoiceAt,
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens            hiding (transform)
import           Control.Monad
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.Default
import           Data.Foldable           hiding (concat)
import           Data.Functor.Adjunction (unzipR)
import qualified Data.List               as List
import           Data.Maybe
import           Data.Ratio
import           Data.Semigroup
import           Data.Typeable
import           Data.VectorSpace        hiding (Sum)

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Time

-- |
-- Class of types that can be tied. Ties are added to a score by splitting a single note
-- into two and annotating them with a /begin tie/ and /end tie/ mark respectively.
--
--
-- Minimal definition: 'toTied', or both 'beginTie' and 'endTie'.
--
class Tiable a where
  -- |
  -- Modify a note to be the first note in a tied note pair.
  --
  beginTie :: a -> a
  beginTie = fst . toTied

  -- |
  -- Modify a note to be the second note in a tied note pair.
  --
  endTie :: a -> a
  endTie = snd . toTied

  -- |
  -- Split a single note into a pair of tied notes.
  --
  -- The first returned element should have the original 'onset' and the second
  -- element should have the original 'offset'. Formally
  --
  -- > (onset . fst . toTied) a = onset a
  -- > (offset . snd . toTied) a = offset a
  --
  toTied    :: a -> (a, a)
  toTied a = (beginTie a, endTie a)

newtype TieT a = TieT { getTieT :: ((Any, Any), a) }
  deriving (Eq, Ord, Show, Functor, Foldable, Typeable, Applicative, Monad)

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (TieT a) where
  type Unwrapped (TieT a) = ((Any, Any), a)
  _Wrapped' = iso getTieT TieT

instance Rewrapped (TieT a) (TieT b)

instance Tiable Double      where { beginTie = id ; endTie = id }
instance Tiable Float       where { beginTie = id ; endTie = id }
instance Tiable Char        where { beginTie = id ; endTie = id }
instance Tiable Int         where { beginTie = id ; endTie = id }
instance Tiable Integer     where { beginTie = id ; endTie = id }
instance Tiable ()          where { beginTie = id ; endTie = id }
instance Tiable (Ratio a)   where { beginTie = id ; endTie = id }

instance Tiable a => Tiable (TieT a) where
  toTied (TieT ((prevTie, nextTie), a))   = (TieT ((prevTie, Any True), b), TieT ((Any True, nextTie), c))
       where (b,c) = toTied a

instance Tiable a => Tiable [a] where
  toTied = unzip . fmap toTied

instance Tiable a => Tiable (Behavior a) where
  toTied = unzipR . fmap toTied

--
-- There is no (HasPart ChordT) instance, so PartT must be outside ChordT in the stack
-- This restriction assures all chord notes are in the same part
--
instance Tiable a => Tiable (c, a) where
  toTied = unzipR . fmap toTied

instance Tiable a => Tiable (Maybe a) where
  toTied = unzipR . fmap toTied

instance Tiable a => Tiable (Sum a) where
  toTied = unzipR . fmap toTied

instance Tiable a => Tiable (Product a) where
  toTied = unzipR . fmap toTied

-- Lifted instances

instance IsPitch a => IsPitch (TieT a) where
  fromPitch = pure . fromPitch

instance IsDynamics a => IsDynamics (TieT a) where
  fromDynamics = return . fromDynamics

instance Transformable a => Transformable (TieT a) where
  transform s = fmap (transform s)

instance Reversible a => Reversible (TieT a) where
  rev = fmap rev

instance Num a => Num (TieT a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (TieT a) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance Floating a => Floating (TieT a) where
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

instance Enum a => Enum (TieT a) where
  toEnum = pure . toEnum
  fromEnum = fromEnum . get1

instance Bounded a => Bounded (TieT a) where
  minBound = pure minBound
  maxBound = pure maxBound

instance (Num a, Ord a, Real a) => Real (TieT a) where
  toRational = toRational . get1

instance (Real a, Enum a, Integral a) => Integral (TieT a) where
  quot = liftA2 quot
  rem = liftA2 rem
  toInteger = toInteger . get1

-- |
-- Split all notes that cross a barlines into a pair of tied notes.
--
splitTiesVoice :: Tiable a => Voice a -> Voice a
splitTiesVoice = (^. voice) . map (^. stretched)
  . concat . snd . List.mapAccumL g 0
  . map (^. from stretched) . (^. stretcheds)
  where
    g t (d, x) = (t + d, occs)
      where
        (_, barTime) = properFraction t
        remBarTime   = 1 - barTime
        occs         = splitDurThen remBarTime 1 (d,x)

-- |
-- Split all voice into bars, using the given bar durations. Music that does not
-- fit into the given durations is discarded.
--
-- Notes that cross a barlines are split into tied notes.
--
splitTiesVoiceAt :: Tiable a => [Duration] -> Voice a -> [Voice a]
splitTiesVoiceAt barDurs x = fmap ((^. voice) . map (^. stretched)) $ splitTiesVoiceAt' barDurs ((map (^. from stretched) . (^. stretcheds)) x)

splitTiesVoiceAt' :: Tiable a => [Duration] -> [(Duration, a)] -> [[(Duration, a)]]
splitTiesVoiceAt' []  _  =  []
splitTiesVoiceAt' _  []  =  []
splitTiesVoiceAt' (barDur : rbarDur) occs = case splitDurFor barDur occs of
  (barOccs, [])       -> barOccs : []
  (barOccs, restOccs) -> barOccs : splitTiesVoiceAt' rbarDur restOccs

tsplitTiesVoiceAt :: [Duration] -> [Duration] -> [[(Duration, Char)]]
tsplitTiesVoiceAt barDurs = fmap (map (^. from stretched) . (^. stretcheds)) . splitTiesVoiceAt barDurs . ((^. voice) . map (^. stretched)) . fmap (\x -> (x,'_'))

-- |
-- Split an event into one chunk of the duration @s@, followed parts shorter than duration @t@.
--
-- The returned list is always non-empty. All elements but the first and the last must have duration @t@.
--
-- > sum $ fmap fst $ splitDur s (x,a) = x
--
splitDurThen :: Tiable a => Duration -> Duration -> (Duration, a) -> [(Duration, a)]
splitDurThen s t x = case splitDur s x of
  (a, Nothing) -> [a]
  (a, Just b)  -> a : splitDurThen t t b


-- |
-- Extract as many notes or parts of notes as possible in the given positive duration, and
-- return it with remaining notes.
--
-- The extracted notes always fit into the given duration, i.e.
--
-- > sum $ fmap duration $ fst $ splitDurFor maxDur xs <= maxDur
--
-- If there are remaining notes, they always fit exactly, i.e.
--
-- > sum $ fmap duration $ fst $ splitDurFor maxDur xs == maxDur  iff  (not $ null $ snd $ splitDurFor maxDur xs)
--
splitDurFor :: Tiable a => Duration -> [(Duration, a)] -> ([(Duration, a)], [(Duration, a)])
splitDurFor remDur []       = ([], [])
splitDurFor remDur (x : xs) = case splitDur remDur x of
  (x@(d,_), Nothing)   ->
      if d < remDur then
          first (x:) $ splitDurFor (remDur - d) xs
      else -- d == remDur
          ([x], xs)
  (x@(d,_), Just rest) -> ([x], rest : xs)

tsplitDurFor :: Duration -> [Duration] -> ([(Duration,Char)], [(Duration,Char)])
tsplitDurFor maxDur xs = splitDurFor maxDur $ fmap (\x -> (x,'_')) xs
-- instance Tiable Char where
  -- toTied _ = ('(',')')

-- |
-- Split a note if it is longer than the given duration. Returns the first part of the
-- note (which always <= s) and the rest.
--
-- > splitDur maxDur (d,a)
--
splitDur :: Tiable a => Duration -> (Duration, a) -> ((Duration, a), Maybe (Duration, a))
splitDur maxDur (d,a)
  | maxDur <= 0 = error "splitDur: maxDur must be > 0"
  | d <= maxDur =  ((d, a), Nothing)
  | d >  maxDur =  ((maxDur, b), Just (d - maxDur, c)) where (b,c) = toTied a


-- TODO use extract
get1 (TieT (_,x)) = x

