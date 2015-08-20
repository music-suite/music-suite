
-- {-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE CPP                        #-}
-- {-# LANGUAGE DeriveDataTypeable         #-}
-- {-# LANGUAGE DeriveFoldable             #-}
-- {-# LANGUAGE DeriveFunctor              #-}
-- {-# LANGUAGE DeriveTraversable          #-}
-- {-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE StandaloneDeriving         #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}

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
import           Data.Aeson                    (ToJSON (..), FromJSON(..))
import qualified Data.Aeson                    as JSON

import           Music.Dynamics.Literal
import           Music.Pitch.Literal

import           Music.Time.Internal.Util (dependingOn, through, tripped)
import           Music.Time.Juxtapose
import           Music.Time.Meta


-- |
-- A 'Event' is a value transformed to appear in some 'Span'. Like 'Span', it is an instance of 'Transformable'.
--
newtype Event a = Event { getEvent :: Span `Couple` a }
  deriving (Eq, Ord, Typeable, Foldable, Applicative, Monad, Comonad, Traversable,
            Functor, Num, Fractional, Floating, Real, RealFrac)

instance Wrapped (Event a) where
  type Unwrapped (Event a) = (Span, a)
  _Wrapped' = iso (getCouple . getEvent) (Event . Couple)

instance Rewrapped (Event a) (Event b)

instance Transformable (Event a) where
  transform t = over eventSpan (transform t)

instance HasDuration (Event a) where
  _duration = _duration . _era

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

instance ToJSON a => ToJSON (Event a) where
  -- TODO meta
  toJSON a = JSON.object [ ("span", toJSON s), ("value", toJSON x) ]
    where
      (s, x) = a^.from event

instance FromJSON a => FromJSON (Event a) where
  parseJSON (JSON.Object x) = liftA2 (\x y -> (x,y)^.event) era value
    where
      era   = x JSON..: "span" -- TODO should change this name
      value = x JSON..: "value"
  parseJSON _ = empty

-- | View a event as a pair of the original value and the transformation (and vice versa).
event :: Iso (Span, a) (Span, b) (Event a) (Event b)
event = from _Wrapped

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
triple = from event . bimapping onsetAndDuration id . tripped

