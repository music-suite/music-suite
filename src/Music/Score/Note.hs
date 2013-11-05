
{-# LANGUAGE

    GeneralizedNewtypeDeriving,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    StandaloneDeriving,

    TypeFamilies, -- Debug

    MultiParamTypeClasses,
    FlexibleInstances       -- for Newtype
    #-}

module Music.Score.Note (
        Note(..),
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
