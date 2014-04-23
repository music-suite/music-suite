
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Music.Score.Note (
        Note,
        getNote,
        getNoteSpan,
        getNoteValue,
        (=:),
  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Comonad.Env
import           Control.Monad

import           Data.Foldable          (Foldable)
import qualified Data.Foldable          as F
import           Data.PairMonad         ()
import           Data.Traversable       (Traversable)
import qualified Data.Traversable       as T

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Score.Pitch
import           Music.Time

newtype Note a = Note { getNote_ :: (Span, a) }
    deriving (Eq, Ord, Show, {-Read, -}Functor, Applicative, Monad, Comonad, Foldable, Traversable)

-- |
-- Deconstruct a note.
--
-- Typically used with the @ViewPatterns@ extension, as in
--
-- > foo (getNote -> (s,x)) = ...
--
getNote :: Note a -> (Span, a)
getNote (Note x) = x

-- | Get the span of the note. Same as 'era' and 'ask'.
getNoteSpan :: Note a -> Span
getNoteSpan = fst . getNote

-- | Get the value of the note. Same as 'extract'.
getNoteValue :: Note a -> a
getNoteValue = snd . getNote

-- Note that
-- extract = getNoteValue
-- ask = getNoteSpan

instance ComonadEnv Span Note where
    ask = getNoteSpan

instance Delayable (Note a) where
    delay n (Note (s,x)) = Note (delay n s, x)

instance Stretchable (Note a) where
    stretch n (Note (s,x)) = Note (stretch n s, x)

instance HasOnset (Note a) where
    onset (Note (s,x)) = onset s

instance HasOffset (Note a) where
    offset (Note (s,x)) = offset s

instance IsPitch a => IsPitch (Note a) where
    fromPitch = pure . fromPitch

instance IsDynamics a => IsDynamics (Note a) where
    fromDynamics = pure . fromDynamics

instance IsInterval a => IsInterval (Note a) where
    fromInterval = pure . fromInterval

type instance Pitch (Note a) = Pitch a
instance HasGetPitch a => HasGetPitch (Note a) where
    __getPitch  = __getPitch . getNoteValue
instance HasSetPitch a b => HasSetPitch (Note a) (Note b) where
    type SetPitch g (Note a) = Note (SetPitch g a)
    __mapPitch f = fmap (__mapPitch f)


-- | Construct a note from a span and value.
--
-- Typically used with the span constructors as in:
--
-- > 0 <-> 2 =: c
-- > 0 >-> 1 =: d
--
(=:) :: Span -> a -> Note a
s =: x  =  Note (s,x)
