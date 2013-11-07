
{-# LANGUAGE
    GeneralizedNewtypeDeriving, 
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    TypeFamilies,
    MultiParamTypeClasses,
    FlexibleInstances #-}

module Music.Score.Note (
        Note(..),
        unnote,
        (=:),
  ) where

import Control.Monad
import Control.Applicative

import Data.PairMonad ()
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Music.Time

newtype Note a = Note { getNote :: (Span, a) }
    deriving (Eq, Ord, Show, {-Read, -}Functor, Applicative, Monad, Foldable, Traversable)

instance Delayable (Note a) where
    delay n (Note (s,x)) = Note (delay n s, x)
instance Stretchable (Note a) where
    stretch n (Note (s,x)) = Note (stretch n s, x)
instance HasOnset (Note a) where
    onset (Note (s,x)) = onset s
instance HasOffset (Note a) where
    offset (Note (s,x)) = offset s

unnote :: Note a -> (Span, a)
unnote (Note x) = x

-- | Construct a note from a span and value.
-- 
-- Typically used with the span constructors as in:
--
-- > 0 <-> 2 =: c
-- > 0 --> 1 =: d
--
(=:) :: Span -> a -> Note a
s =: x  =  Note (s,x)
