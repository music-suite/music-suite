
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
{-# LANGUAGE FlexibleContexts              #-}

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
import           Control.Monad (join, mapM)
import           Data.Semigroup
import           Data.Functor.Couple
import           Data.Functor.Classes
import           Data.Functor.Compose
import           Control.Monad.Compose
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

-- TODO
instance Traversable AddMeta where
  traverse = annotated
instance Eq1 AddMeta where
  eq1 = (==)
instance Eq a => Eq1 (Couple a) where
  eq1 = (==)
instance Ord1 AddMeta where
instance Eq a => Ord1 (Couple a) where
instance Num a => Num (Compose f g a) where
instance Fractional a => Fractional (Compose f g a) where
instance Floating a => Floating (Compose f g a) where
instance (Real a, Ord1 f, Ord1 g, Functor f) => Real (Compose f g a) where
instance (RealFrac a, Ord1 f, Ord1 g, Functor f) => RealFrac (Compose f g a) where
instance (Functor f, Monad f, Monad g, Traversable g) => Monad (Compose f g) where
  return = Compose . return . return
  xs >>= f = Compose $ mbind (getCompose . f) (getCompose xs)
instance (Comonad f, Comonad g) => Comonad (Compose f g)
  
  
newtype Event a = Event { getEvent :: Compose AddMeta (Couple Span) a }
  deriving (
    Eq,
    Ord,
    Typeable,
    Foldable,
    Applicative,
    Monad,
    Comonad,
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


-- TODO consolidate
dependingOn :: Lens s t (x,a) (x,b) -> (x -> Lens a b c d) -> Lens s t c d
dependingOn l depending f = l (\ (x,a) -> (x,) <$> depending x f a)









-- newtype AddMeta1 f a = AddMeta1 { getAddMeta1 :: Twain Meta (f a) }
--   deriving (
--     )
-- 
-- instance Wrapped (AddMeta1 f a) where
--   type Unwrapped (AddMeta1 f a) = Twain Meta (f a)
--   _Wrapped' = iso getAddMeta1 AddMeta1
-- 
-- instance Rewrapped (AddMeta1 f a) (AddMeta1 f b)
-- 
-- instance HasMeta (AddMeta1 f a) where
--   -- twain, pair, element
--   meta = _Wrapped . _Wrapped . _1
-- 
-- 
-- -- instance FunctorWithIndex i AddMeta1 where
--   -- imap f = over annotated $ imap f
-- -- 
-- -- instance FoldableWithIndex Span Score where
-- --   ifoldMap f (Score (m,x)) = ifoldMap f x
-- -- 
-- -- instance TraversableWithIndex Span Score where
-- --   itraverse f (Score (m,x)) = fmap (\x -> Score (m,x)) $ itraverse f x
-- 
-- instance Transformable (f a) => Transformable (AddMeta1 f a) where
--   transform t = over meta (transform t) . over annotated1 (transform t)
-- 
-- instance Reversible (f a) => Reversible (AddMeta1 f a) where
--   rev = over meta rev . over annotated1 rev
-- 
-- -- instance Splittable a => Splittable (AddMeta1 a) where
-- --   split t = unzipR . fmap (split t)
-- 
-- instance HasPosition (f a) => HasPosition (AddMeta1 f a) where
--   _era = _era . extract
--   _position = _position . extract
-- 
-- instance HasDuration (f a) => HasDuration (AddMeta1 f a) where
--   _duration = _duration . extract
-- 
-- -- |
-- -- Access the annotated value.
-- --
-- -- @
-- -- over annotated = fmap
-- -- @
-- --
-- annotated1 :: Lens (AddMeta1 f a) (AddMeta1 f b) (f a) (f b)
-- annotated1 = unsafeAnnotated1
-- 
-- -- |
-- -- Access the annotated value.
-- --
-- -- @
-- -- view fromAnnotated = pure
-- -- @
-- --
-- unannotated1 :: Getter (f a) (AddMeta1 f a)
-- unannotated1 = from unsafeAnnotated1
-- 
-- unsafeAnnotated1 :: Iso (AddMeta1 f a) (AddMeta1 f b) (f a) (f b)
-- unsafeAnnotated1 = _Wrapped . extracted
-- 
-- unzipR f = (map fst f, map snd f)
-- 
-- extracted :: (Applicative m, Comonad m) => Iso (m a) (m b) a b
-- extracted = iso extract pure
