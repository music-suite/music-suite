
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes                 #-}
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

module Music.Time.Event (
        -- * Event type
        Event,

        -- * Construction
        event,
        eventee,
        spanEvent,
        triple
  ) where

import           Control.Applicative
import           Data.Functor.Couple
import           Control.Comonad
import           Control.Lens             hiding (Indexable, Level, above,
                                           below, index, inside, parts,
                                           reversed, transform, (<|), (|>))
import           Data.PairMonad
import           Data.String
import           Data.VectorSpace
import           Data.Foldable            (Foldable)
import qualified Data.Foldable            as Foldable
import           Data.Typeable

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Time.Internal.Util (through, tripped)
import           Music.Time.Reverse
import           Music.Time.Split


-- |
-- A 'Event' is a value with an 'onset' and and 'offset' in time. It is an instance
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
newtype Event a = Event { getEvent :: Span `Couple` a }
  deriving (
    Eq,
    Ord,
    Typeable,
    Foldable,
    Traversable,
    
    Functor,
    Applicative,
    Monad,
    Comonad,
    
    Num,
    Fractional,
    Floating,
    Real,
    RealFrac
    )

instance Wrapped (Event a) where
  type Unwrapped (Event a) = (Span, a)
  _Wrapped' = iso (getCouple . getEvent) (Event . Couple)

instance Rewrapped (Event a) (Event b)

instance Transformable (Event a) where
  transform t = over eventSpan (transform t)

instance HasDuration (Event a) where
  _duration = _duration . view eventSpan

instance HasPosition (Event a) where
  _era = view eventSpan

instance IsString a => IsString (Event a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Event a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Event a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Event a) where
  fromDynamics = pure . fromDynamics

instance (Show a, Transformable a) => Show (Event a) where
  show x = show (x^.from event) ++ "^.event"

-- | View a event as a pair of the original value and the transformation (and vice versa).
event :: Iso (Span, a) (Span, b) (Event a) (Event b)
event = _Unwrapped

eventSpan :: Lens' (Event a) Span
eventSpan = from event . _1

eventValue :: Lens (Event a) (Event b) a b
eventValue = from event . _2

-- | View the value in the event.
eventee :: (Transformable a, Transformable b) => Lens (Event a) (Event b) a b
eventee = from event `dependingOn` (transformed)

-- | Event as a span with a trivial value.
spanEvent :: Iso' Span (Event ())
spanEvent = iso (\s -> (s,())^.event) (^.era)

-- | View a event as a @(time, duration, value)@ triple.
triple :: Iso (Event a) (Event b) (Time, Duration, a) (Time, Duration, b)
triple = from event . bimapping delta id . tripped


-- TODO consolidate
dependingOn :: Lens s t (x,a) (x,b) -> (x -> Lens a b c d) -> Lens s t c d
dependingOn l depending f = l (\ (x,a) -> (x,) <$> depending x f a)

