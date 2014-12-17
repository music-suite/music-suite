
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TupleSections              #-}

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

module Music.Time.Note (
        -- * Note type
        Note,
        -- * Construction
        note,
        notee,
        durationNote,
        -- noteComplement,
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
import           Music.Time.Juxtapose


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
notee :: (Transformable a, Transformable b) => Lens (Note a) (Note b) a b
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


-- TODO consolidate
dependingOn :: Lens s t (x,a) (x,b) -> (x -> Lens a b c d) -> Lens s t c d
dependingOn l depending f = l (\ (x,a) -> (x,) <$> depending x f a)





