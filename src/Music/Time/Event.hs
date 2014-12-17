
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
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
import           Control.Comonad
import           Control.Lens             hiding (Indexable, Level, above,
                                           below, index, inside, parts,
                                           reversed, transform, (<|), (|>))
import           Control.Monad            (join, mapM)
import           Control.Monad.Compose
import           Data.Distributive        (distribute)
import           Data.Foldable            (Foldable)
import qualified Data.Foldable            as Foldable
import           Data.Functor.Classes
import           Data.Functor.Compose
import           Data.Functor.Couple
import           Data.PairMonad
import           Data.Semigroup
import           Data.String
import           Data.Typeable
import           Data.VectorSpace

import           Music.Dynamics.Literal
import           Music.Pitch.Literal

import           Music.Time.Internal.Util (through, tripped, dependingOn)
import           Music.Time.Juxtapose
import           Music.Time.Meta


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

#ifndef GHCI
-- TODO move
instance Traversable AddMeta where
  traverse = annotated
instance Eq1 AddMeta where
  eq1 = (==)
instance Eq a => Eq1 (Couple a) where
  eq1 = (==)
instance Ord1 AddMeta where
  compare1 = compare
instance Ord a => Ord1 (Couple a) where
  compare1 = compare

instance Num (f (g a)) => Num (Compose f g a) where
  Compose a + Compose b = Compose (a + b)
  Compose a - Compose b = Compose (a - b)
  Compose a * Compose b = Compose (a * b)
  signum (Compose a) = Compose (signum a)
  abs (Compose a) = Compose (abs a)
  fromInteger = Compose . fromInteger
instance Fractional (f (g a)) => Fractional (Compose f g a) where
  Compose a / Compose b = Compose (a / b)
  fromRational = Compose . fromRational
instance Floating (f (g a)) => Floating (Compose f g a) where

instance (Real (f (g a)), Ord1 f, Ord1 g, Ord a, Functor f) => Real (Compose f g a) where
  -- TODO
instance (RealFrac (f (g a)), Ord1 f, Ord1 g, Ord a, Functor f) => RealFrac (Compose f g a) where
  -- TODO
instance (Functor f, Monad f, Monad g, Traversable g) => Monad (Compose f g) where
  return = Compose . return . return
  xs >>= f = Compose $ mbind (getCompose . f) (getCompose xs)
instance (Comonad f, Comonad g) => Comonad (Compose f g) where
  extract (Compose f) = (extract . extract) f
  duplicate = error "No Comonad Compose.duplicate (in Music.Time.Event)"
  -- TODO duplicate
#endif

newtype Event a = Event { getEvent :: Compose AddMeta (Couple Span) a }
  deriving (
    Eq,
    Ord,
    Typeable,
    Foldable,
    Applicative,
    Monad,
    -- Comonad,
    Traversable,

    Functor,

    Num,
    Fractional,
    Floating,
    Real,
    RealFrac
    )

instance Wrapped (Event a) where
  type Unwrapped (Event a) = AddMeta (Span, a)
  _Wrapped' = iso (fmap getCouple . getCompose . getEvent) (Event . Compose . fmap Couple)

instance Rewrapped (Event a) (Event b)

instance Transformable (Event a) where
  transform t = over eventSpan (transform t)

instance HasDuration (Event a) where
  _duration = _duration . _era

instance HasPosition (Event a) where
  _era = view eventSpan

instance HasMeta (Event a) where
  meta = _Wrapped . meta

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

instance Comonad Event where
  extract e   = e^.eventValue
  duplicate e = set meta (e^.meta) $ (e^.eventSpan,e)^.event


-- | View a event as a pair of the original value and the transformation (and vice versa).
event :: Iso (Span, a) (Span, b) (Event a) (Event b)
event = from (_Wrapped . unsafeAnnotated)
-- TODO not safe anymore...

-- safeUnevent = _Wrapped . annotated == from event
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

