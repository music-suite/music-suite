
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
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
-------------------------------------------------------------------------------------

module Music.Time.Stretched (
        -- * Note type
        Note,
        -- * Construction
        note,
        notee,
        durationNote,
        noteComplement,
  ) where

import           Control.Applicative
import           Control.Lens           hiding (Indexable, Level, above, below,
                                         index, inside, parts, reversed,
                                         transform, (<|), (|>))
import           Data.Bifunctor
import           Data.Foldable          (Foldable)
import qualified Data.Foldable          as Foldable
import           Data.Functor.Couple
import           Data.String
import           Data.Typeable
import           Data.VectorSpace

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Time.Reverse
import           Music.Time.Split


-- |
-- A value 'Note' value, representing a suspended stretch of some 'Transformable'
-- value. We can access the value in bothits original and note form using 'note'
-- and 'notee', respectively.
--
-- Placing a value inside 'Note' makes it invariant under 'delay', however the inner
-- value can still be delayed using @'fmap' 'delay'@.
--
newtype Note a = Note { getNote :: Duration `Couple` a }
  deriving (
    Eq,
    Ord,
    Typeable,
    Foldable,
    Traversable,

    Functor,
    Applicative,
    Monad,

    Num,
    Fractional,
    Floating,
    Real,
    RealFrac
    )
            -- Comonad,

instance (Show a, Transformable a) => Show (Note a) where
  show x = show (x^.from note) ++ "^.note"

instance Wrapped (Note a) where
  type Unwrapped (Note a) = (Duration, a)
  _Wrapped' = iso (getCouple . getNote) (Note . Couple)

instance Rewrapped (Note a) (Note b)

instance Transformable (Note a) where
  transform t = over (from note . _1) (transform t)

instance HasDuration (Note a) where
  _duration = _duration . view (from note)

instance IsString a => IsString (Note a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Note a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Note a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Note a) where
  fromDynamics = pure . fromDynamics

-- | View a note value as a pair of the original value and a stretch factor.
note :: Iso (Duration, a) (Duration, b) (Note a) (Note b)
note = _Unwrapped

-- | Access the note value.
-- Taking a value out carries out the stretch (using the 'Transformable' instance),
-- while putting a value in carries out the reverse transformation.
--
-- >>> view notee $ (2,3::Duration)^.note
-- 6
--
-- >>> set notee 6 $ (2,1::Duration)^.note
-- (2,3)^.note
--
notee :: Transformable a => Lens (Note a) (Note a) a a
notee = _Wrapped `dependingOn` (transformed . stretching)

-- | A note value as a duration carrying an associated value.
-- Whitness by picking a trivial value.
--
-- >>> 2^.durationNote
-- (2,())^.note
--
durationNote :: Iso' Duration (Note ())
durationNote = iso (\d -> (d,())^.note) (^.duration)

-- >>> (pure ())^.from durationNote
-- 1
-- >>> (pure () :: Note ())^.duration
-- 1

-- TODO could also be an iso...
noteComplement :: Note a -> Note a
noteComplement (Note (Couple (d,x))) = Note $ Couple (negateV d, x)

-- FIXME negateV is negate not recip
-- The negateV method should follow (^+^), which is (*) for durations (is this bad?)






dependingOn :: Lens' s (x,a) -> (x -> Lens' a c) -> Lens' s c
dependingOn l f = lens getter setter
  where
    getter s = let
      (x,a) = view l s
      l2    = f x
      in view l2 a
    setter s b = let
      (x,_) = view l s
      l2    = f x
      in set (l._2.l2) b s

